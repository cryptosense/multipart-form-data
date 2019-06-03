let stream_to_string stream =
  stream
  |> Lwt_stream.get_available
  |> String.concat ""

let part_to_testable part =
  match part with
  | {Multipart_form_data.Part.name=name; value=Variable value}
    ->
    (["variable"; name; ""; value], None)
  | {name=name; value=File {filename=filename; content=content; length=length}}
    ->
    (["file"; name; filename; stream_to_string content], length)

let testable_callback_factory () =
  let parts = ref [] in
  let callback part =
    Lwt.return (parts := part :: !parts)
  in
  let read () =
    !parts
  in
  (callback, read)

let empty_request = {Multipart_form_data.Request.headers = []; body = Lwt_stream.of_list [""]}

let separator = "===============1269689916"

let test_headers = [("Content-Type", "multipart/form-data; boundary=" ^ separator)]
