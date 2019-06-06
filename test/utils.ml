let stream_to_string stream =
  stream
  |> Lwt_stream.get_available
  |> String.concat ""

let part_to_testable = function
  | {Multipart_form_data.Part.name = name; filename = filename; content = content; content_length = length}
    ->
    ([name; stream_to_string content], (filename, length))

let test_parts ~name ~expected values =
    let expected_testable = List.map part_to_testable expected in
    let values_testable = List.map part_to_testable values in
    Alcotest.(check (list (pair (list string) (pair (option string) (option int64)))))
        name
        expected_testable
        values_testable

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

let boundary = "===============1269689916"

let test_headers = [("Content-Type", "multipart/form-data; boundary=" ^ boundary)]
