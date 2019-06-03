module MultipartRequest = struct

  type form_element =
    { key : string
    ; value : string
    }

  type stream_element =
    { name : string
    ; filename : string
    ; content : string Lwt_stream.t
    ; length : int64
    }

  type element =
    | Form of form_element
    | Stream of stream_element

  type t =
    { elements : element list
    ; separator : string
    }

end

let init separator =
  { MultipartRequest.elements = []
  ; separator = separator
  }

let add_form_element ~name ~value mp =
  let open MultipartRequest in
  { mp with elements = Form { key=name; value=value } :: mp.elements}

let add_file_from_string ~name ~filename ~content mp =
  let open MultipartRequest in
  { mp with
    elements =
      Stream { content = Lwt_stream.of_list [ content ]
             ; name=name
             ; filename=filename
             ; length=Int64.of_int(String.length content)
             }
      :: mp.elements
  }

let add_file_from_stream ~name ~filename ~content ~content_length mp =
  let open MultipartRequest in
  { mp with
    elements =
      Stream { content = content
             ; name=name
             ; filename=filename
             ; length=content_length
             }
      :: mp.elements
  }

let open_file path =
  (* This function returns a buffered IO read of a file *)
  let open Lwt.Infix in
  let read_while_not_empty channel () =
    (Lwt_io.read ~count:4096 channel)
    >|= (fun chunck ->
        match chunck with
        | "" -> None
        | _ -> Some chunck
      )
  in
  path
  |> Lwt_io.open_file ~mode:Lwt_io.Input
  >|= read_while_not_empty
  >|= Lwt_stream.from
  |> CCResult.pure

let safe_open_file path =
  try open_file path with
  | Unix.Unix_error(Unix.ENOENT, _, _) -> CCResult.fail ("File " ^ path ^ " not found")
  | Unix.Unix_error(Unix.EACCES, _, _) -> CCResult.fail ("Permission denied on " ^ path)
  | Unix.Unix_error(Unix.EBUSY, _, _) -> CCResult.fail ("File " ^ path ^ " was busy")
  | Unix.Unix_error(Unix.EISDIR, _, _) -> CCResult.fail ("File " ^ path ^ " is a directory")
  | _ -> CCResult.fail ("Unknown error while reading file " ^ path)

let file_size path =
  path
  |> Lwt_io.file_length
  |> CCResult.pure

let safe_file_size path =
  try file_size path with
  | Unix.Unix_error(Unix.ENOENT, _, _) -> CCResult.fail ("File " ^ path ^ " not found")
  | Unix.Unix_error(Unix.EACCES, _, _) -> CCResult.fail ("Permission denied on " ^ path)
  | Unix.Unix_error(Unix.EBUSY, _, _) -> CCResult.fail ("File " ^ path ^ " was busy")
  | Unix.Unix_error(Unix.EISDIR, _, _) -> CCResult.fail ("File " ^ path ^ " is a directory")
  | _ -> CCResult.fail ("Unknown error while reading file " ^ path)

let element_header separator element =
  match element with
  | MultipartRequest.Form f
    ->
    "\r\n--"
    ^ separator
    ^ "\r\nContent-Disposition: form-data; name=\""
    ^ f.key
    ^ "\"\r\n\r\n"
  | Stream s
    ->
    "\r\n--"
    ^ separator
    ^ "\r\nContent-Disposition: form-data; name=\""
    ^ s.name
    ^ "\"; filename=\""
    ^ s.filename
    ^ "\"\r\nContent-Type: application/octet-stream\r\n\r\n"

let closing_line separator =
  "\r\n--" ^ separator ^ "--\r\n"

let closing_line_size separator =
  Int64.of_int (String.length (closing_line separator))


let element_to_string separator element =
  match element with
  | MultipartRequest.Form f
    ->
    CCResult.return (
      Lwt_stream.of_list
        [ (element_header separator element)
        ; f.value
        ]
    )
  | Stream s
    ->
    let file_header = element_header separator element in
    let file_header_stream = Lwt_stream.of_list [file_header] in
    CCResult.return (
      Lwt_stream.append file_header_stream s.content
    )


let element_size separator element =
  match element with
  | MultipartRequest.Form _
    ->
    CCResult.return (
      Int64.of_int (String.length (element_header separator element))
    )
  | Stream s
    ->
    let file_header = (element_header separator element) in
    CCResult.return (
      Int64.add (Int64.of_int (String.length file_header)) s.length
    )


let rec mfoldl f acc l =
  match l with
  | h::t
    ->
    CCResult.(>>=)
      h
      (fun value -> mfoldl f (f value acc) t)
  | []
    ->
    CCResult.return acc


let r_body mp =
  let {MultipartRequest.elements; separator} = mp in
  elements
  |> List.map (element_to_string separator)
  |> mfoldl Lwt_stream.append (Lwt_stream.of_list [closing_line separator])

let r_headers mp =
  let {MultipartRequest.elements; separator} = mp in
  let open CCResult.Infix in
  elements
  |> List.map (element_size separator)
  |> mfoldl Int64.add (closing_line_size separator)
  >|= (fun (total_size) ->
      [ ("Content-Type", "multipart/form-data; boundary=" ^ separator)
      ; ("Content-Length", Int64.to_string total_size)])
