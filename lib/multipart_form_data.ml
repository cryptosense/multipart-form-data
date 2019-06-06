module Request = struct
  type t =
    { headers : (string * string) list
    ; body : string Lwt_stream.t
    }
end

module Part = struct
  type t =
    { name: string
    ; filename: string option
    ; content_length: int64 option
    ; content: string Lwt_stream.t
    }
end

(**
 * Utils
 **)
let rec lowercase_first = function
  | (h, v)::t -> ((String.lowercase_ascii h), v)::(lowercase_first t)
  | [] -> []

(**
 * Read multipart streams
 **)

type content_part_struct =
  { cache: (string list) ref
  ; finished: bool ref
  ; stream: string Lwt_stream.t
  }

let stream_factory () =
  let cache = (ref []: (string list) ref) in
  let finished = (ref false) in
  let generator () =
    match (!cache, !finished) with
    | ([], true) -> None
    | ([], false) -> Some ""
    | (h::t, _) -> cache := t; Some h
  in
  let stream = Lwt_stream.from_direct generator in
  { cache = cache
  ; finished = finished
  ; stream = stream
  }

let callback_factory ~callback =
  let current_part = ref (stream_factory ()) in
  let generate_new_stream = ref true in
  let caching_callback ~name ~filename line is_finished =
    if !generate_new_stream then
      (* We finished the previous part and start a new one *)
      let _ =
        current_part := stream_factory ();
        (* Add line to the stream *)
        !current_part.cache := line::!(!current_part.cache);
        !current_part.finished := is_finished;
      in
      generate_new_stream := is_finished;
      (* Call the callback with the stream that we just generated *)
      callback
        { Part.name = name
        ; filename = filename
        ; content_length = None
        ; content = !current_part.stream
        }
    else
      (* Continuation of the last part *)
      let _ =
        !current_part.cache := line::!(!current_part.cache);
        !current_part.finished := is_finished;
      in
      (* If the current part is finished, we need to put the next
       * incoming line into a new stream.
      *)
      generate_new_stream := is_finished;
      Lwt.return ()
  in
  caching_callback

let part_parser callback headers body =
  let callback = callback_factory ~callback in
  let lowercased_headers = lowercase_first headers in
  let content_type = List.assoc "content-type" lowercased_headers in
  print_endline content_type;
  try (Reader.parse ~stream:body ~content_type ~callback) with
  | _ -> print_endline "EXCEPTION"; Ok ()

let read ~request ~handle_part =
  let {Request.headers; body} = request in
  part_parser handle_part headers body

(**
 * Write multipart requests
 **)

let add_part_to_multipart_request multipart_request part =
  match part with
  | {Part.name = _; filename = _; content_length = None; content = _}
    ->
    failwith "File length is required when writing a multipart request body"
  | {name = name; filename = filename; content_length = Some content_length; content = content}
    ->
    Writer.add_from_stream ~name ~filename ~content ~content_length multipart_request


let write_with_boundary ~boundary ~request =
  let open CCResult.Infix in
  let multipart_request =
    Seq.fold_left
      add_part_to_multipart_request
      (Writer.init boundary)
      request
  in
  Writer.r_headers multipart_request
  >>= fun headers -> Writer.r_body multipart_request
  >|= fun body ->
  { Request.headers = headers
  ; body = body
  }

let write ~request =
  Random.self_init ();
  (* It does not matter if the random numbers are not safe here *)
  let boundary = "-----------------" ^ (string_of_int (Random.int 536870912)) in
  write_with_boundary ~boundary ~request
