module Request = struct
  type t =
    { headers : (string * string) list
    ; body : string Lwt_stream.t
    }
end

module Part = struct
  module Value = struct
    type t =
      | Variable of string
      | File of {filename : string; content : string Lwt_stream.t; length : int64 option}
  end

  type t =
    { name: string
    ; value: Value.t
    }
end

(**
 * Read multipart streams
 **)

let variable_callback_factory ~callback ~name ~value =
  callback {Part.name = name; value = Variable value}

let file_callback_factory ~callback =
  let cache = (ref []: (string list) ref) in
  let finished = (ref false) in
  let generator () =
    match (!cache, !finished) with
    | ([], true) -> None
    | ([], false) -> Some ""
    | (h::t, _) -> cache := t; Some h
  in
  let stream = Lwt_stream.from_direct generator in
  let file_callback ~name ~filename line is_finished =
    cache := line::!cache;
    finished := is_finished;
    callback
      { Part.name = name
      ; value = File {filename = filename; content = stream; length = None}
      }
  in
  file_callback

let part_parser callback headers body =
  let file_callback = file_callback_factory ~callback in
  let variable_callback = variable_callback_factory ~callback in
  let content_type = List.assoc "Content-Type" headers in
  Reader.parse ~stream:body ~content_type ~variable_callback ~file_callback

let read ~request ~handle_part =
  let {Request.headers; body} = request in
  part_parser handle_part headers body

(**
 * Write multipart requests
 **)

let add_part_to_multipart_request multipart_request part =
  match part with
  | {Part.name=name; value=Variable value}
    ->
    Writer.add_form_element ~name ~value multipart_request
  | {name=name; value=(File {filename=filename; content=content; length=Some content_length})}
    ->
    Writer.add_file_from_stream ~name ~filename ~content ~content_length multipart_request
  | {name=_; value=File {filename=_; content=_; length=None}}
    ->
    failwith "File length is required when writing a multipart request body"


let write_with_separator ~separator ~request =
  let open CCResult.Infix in
  let multipart_request =
    Seq.fold_left
      add_part_to_multipart_request
      (Writer.init separator)
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
  let separator = "-----------------" ^ (string_of_int (Random.int 536870912)) in
  write_with_separator ~separator ~request
