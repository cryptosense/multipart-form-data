let get_file name parts =
  match Multipart_form_data.Reader.StringMap.find name parts with
  | `File file -> file
  | `String _ -> failwith "expected a file"

module String_or_file = struct
  type t = [`String of string | `File of Multipart_form_data.Reader.file]

  let equal = (=)

  let pp fmt (part : t) =
    let s = match part with
      | `File _ -> "File _"
      | `String s -> s
    in
    Format.pp_print_string fmt s
end

let string_or_file = (module String_or_file : Alcotest.TESTABLE with type t = String_or_file.t)

module TestInput = struct
  type t =
    | Form of (string * string)
    | File of (string * string)
    | Stream of (string * string)
    | String of (string * string)
end

let multipart_request_to_string mp =
  let body_result =
    mp
    |> Multipart_form_data.Writer.r_body
    |> Lwt_main.run in
  let header_result =
    mp
    |> Multipart_form_data.Writer.r_headers
    |> Lwt_main.run in
  match (header_result, body_result) with
  | (Ok headers, Ok stream)
    ->
    ( headers
    , stream
      |> Lwt_stream.get_available
      |> String.concat ""
    )
  | (_, Error err)
  | (Error err, _)
    ->
    ([], err)

let separator = "---------------16456c9a1a"

let add_test_element mp element =
  match element with
  | TestInput.Form (name, value) -> Multipart_form_data.Writer.add_form_element ~name ~value mp
  | File (name, path) -> Multipart_form_data.Writer.add_file_from_disk ~name ~path mp
  | Stream (name, content) ->
    Multipart_form_data.Writer.add_file_from_stream
      ~name
      ~content:(Lwt_stream.of_list [ content ])
      ~content_length:(String.length content)
      mp
  | String (name, content) ->
    Multipart_form_data.Writer.add_file_from_string
      ~name
      ~content
      mp


let test ~name ~input ~expected_headers ~expected_body =
  ( name
  , `Quick
  , fun () ->
    let (headers, body) =
      input
      |> List.fold_left
        add_test_element
        (Multipart_form_data.Writer.init_with_separator separator)
      |> multipart_request_to_string
    in
    Alcotest.(check (list (pair string string))) (name ^ "_headers") expected_headers headers;
    Alcotest.(check string) (name ^ "_body") expected_body body
  )

let test_fail ~name ~input ~expected_error =
  ( name
  , `Quick
  , fun () ->
    let (_, error) =
      input
      |> List.fold_left
        add_test_element
        (Multipart_form_data.Writer.init_with_separator separator)
      |> multipart_request_to_string
    in
    Alcotest.(check string) name expected_error error
  )

let writer_tests =
  [ test
      ~name:"Empty"
      ~input:[]
      ~expected_headers:
        [ ("Content-Type", "multipart/form-data; boundary=" ^ separator)
        ; ("Content-Length", "33")
        ]
      ~expected_body:("\r\n--" ^ separator ^ "--\r\n")
  ; test
      ~name:"Simple form"
      ~input:[Form ("key", "value")]
      ~expected_headers:[("Content-Type", "multipart/form-data; boundary=" ^ separator);
                         ("Content-Length", "110")]
      ~expected_body:("\r\n--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"key\""
                      ^ "\r\n" ^ "\r\n"
                      ^ "value"
                      ^ "\r\n"
                      ^ "--" ^ separator ^ "--"
                      ^ "\r\n"
                     )
  ; test_fail
      ~name:"Missing file"
      ~input:[File ("missing_file", "/this/file/does/not/exist")]
      ~expected_error:"File /this/file/does/not/exist not found"
  ; test
      ~name:"File from string"
      ~input:[String ("filename", "this is the content of our file\r\n")]
      ~expected_headers:[("Content-Type", "multipart/form-data; boundary=" ^ separator);
                         ("Content-Length", "205")]
      ~expected_body:("\r\n--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"file\"; filename=\"filename\""
                      ^ "\r\n"
                      ^ "Content-Type: application/octet-stream"
                      ^ "\r\n" ^ "\r\n"
                      ^ "this is the content of our file\r\n"
                      ^ "\r\n"
                      ^ "--" ^ separator ^ "--"
                      ^ "\r\n"
                     )
  ; test
      ~name:"File from stream"
      ~input:[Stream ("filename", "this is the content of our file\r\n")]
      ~expected_headers:[("Content-Type", "multipart/form-data; boundary=" ^ separator);
                         ("Content-Length", "205")]
      ~expected_body:("\r\n--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"file\"; filename=\"filename\""
                      ^ "\r\n"
                      ^ "Content-Type: application/octet-stream"
                      ^ "\r\n" ^ "\r\n"
                      ^ "this is the content of our file\r\n"
                      ^ "\r\n"
                      ^ "--" ^ separator ^ "--"
                      ^ "\r\n"
                     )
  ]
