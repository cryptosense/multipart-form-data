(* A series of tests confirming that the reader and writer
 * modules are reversible.
*)


let string2_list = Alcotest.(list (pair string string))

let string3_list = Alcotest.(list (pair (pair string string) string))

let test ~name ~input ~expected_parts ~expected_calls =
  ( name
  , `Quick
  , fun () ->
    let (headers, body) =
      input
      |> List.fold_left
        Test_writer.add_test_element
        (Multipart_form_data.Writer.init_with_separator Test_writer.separator)
      |> Test_writer.multipart_request_to_string
    in
    let content_type =
      headers
      |> List.fold_left
        (fun acc (h, v) ->
           match h with
           | "Content-Type" -> v
           | _ -> acc
        )
        ""
    in
    let stream = Lwt_stream.of_list [ body ] in
    let calls = ref [] in
    let callback ~name ~filename line =
      calls := !calls @ [((name, filename), line)];
      Lwt.return_unit
    in
    let parts =
      Multipart_form_data.Reader.parse ~stream ~content_type ~callback
      |> Lwt_main.run
    in
    Alcotest.(check string "header" ("multipart/form-data; boundary=" ^ Test_writer.separator) content_type);
    Alcotest.check string2_list "parts" expected_parts parts;
    Alcotest.check
      string3_list
      "calls"
      (List.map (fun (x, y, z) -> ((x, y), z)) expected_calls)
      !calls;
  )

let read_write_tests =
  [ test
      ~name:"Simple form"
      ~input:[Form ("key", "value")]
      ~expected_parts:[("key", "value")]
      ~expected_calls:[]
  ; test
      ~name:"Simple file"
      ~input:[String ("filename", "file\r\ncontent\r\n")]
      ~expected_parts:[]
      ~expected_calls:[("file", "filename", "file\r\ncontent\r\n")]
  ; test
      ~name:"File and form"
      ~input:
          [ String ("filename", "file\r\ncontent\r\n")
          ; Form ("key", "value\r\n")
          ]
      ~expected_parts:[("key", "value\r\n")]
      ~expected_calls:[("file", "filename", "file\r\ncontent\r\n")]
  ]
