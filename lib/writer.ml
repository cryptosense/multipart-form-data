module MultipartRequest = struct

  type element =
    { name : string
    ; filename : string option
    ; content : string Lwt_stream.t
    ; length : int64
    }

  type t =
    { elements : element list
    ; separator : string
    }

end

let init separator =
  { MultipartRequest.elements = []
  ; separator = separator
  }

let add_from_stream ~name ~filename ~content ~content_length mp =
  let open MultipartRequest in
  { mp with
    elements =
      { content = content
      ; name=name
      ; filename=filename
      ; length=content_length
      }
      :: mp.elements
  }


let element_header separator element =
  match element.MultipartRequest.filename with
  | None
    ->
    "\r\n--"
    ^ separator
    ^ "\r\nContent-Disposition: form-data; name=\""
    ^ element.name
    ^ "\"\r\n\r\n"
  | Some filename
    ->
    "\r\n--"
    ^ separator
    ^ "\r\nContent-Disposition: form-data; name=\""
    ^ element.name
    ^ "\"; filename=\""
    ^ filename
    ^ "\"\r\nContent-Type: application/octet-stream\r\n\r\n"

let closing_line separator =
  "\r\n--" ^ separator ^ "--\r\n"

let closing_line_size separator =
  Int64.of_int (String.length (closing_line separator))


let element_to_string separator element =
  let file_header = element_header separator element in
  let file_header_stream = Lwt_stream.of_list [file_header] in
  CCResult.return (
    Lwt_stream.append file_header_stream element.content
  )


let element_size separator element =
  let file_header = (element_header separator element) in
  CCResult.return (
    Int64.add (Int64.of_int (String.length file_header)) element.length
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
