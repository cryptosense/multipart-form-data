open OUnit2

let get_file name parts =
  match Multipart.StringMap.find name parts with
  | `File file -> file
  | `String _ -> assert_failure "expected a file"

let test_parse ctxt =
  let body =
    String.concat "\r\n"
      [ {|--------------------------1605451f456c9a1a|}
      ; {|Content-Disposition: form-data; name="a"|}
      ; {||}
      ; {|b|}
      ; {|--------------------------1605451f456c9a1a|}
      ; {|Content-Disposition: form-data; name="c"|}
      ; {||}
      ; {|d|}
      ; {|--------------------------1605451f456c9a1a|}
      ; {|Content-Disposition: form-data; name="upload"; filename="testfile"|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|testfilecontent|}
      ; {||}
      ; {|--------------------------1605451f456c9a1a--|}
      ]
  in
  let content_type = "multipart/form-data; boundary=------------------------1605451f456c9a1a" in
  let stream = Lwt_stream.of_list [body] in
  let thread =
    let%lwt parts_stream = Multipart.parse_stream ~stream ~content_type in
    let%lwt parts = Multipart.get_parts parts_stream in
    assert_equal (`String "b") (Multipart.StringMap.find "a" parts);
    assert_equal (`String "d") (Multipart.StringMap.find "c" parts);
    let file = get_file "upload" parts in
    assert_equal ~ctxt ~printer:[%show: string] "upload" (Multipart.file_name file);
    assert_equal ~ctxt "application/octet-stream" (Multipart.file_content_type file);
    let%lwt file_chunks = Lwt_stream.to_list (Multipart.file_stream file) in
    assert_equal ~ctxt "testfilecontent" (String.concat "" file_chunks);
    Lwt.return_unit
  in
  Lwt_main.run thread

let test_parse_request ctxt =
  let cr = "\r" in
  let chunks =
    List.map (String.concat "\n") @@
      [ [ {|--9219489391874b51bb29b52a10e8baac|} ^ cr
        ; {|Content-Disposition: form-data; name="foo"|} ^ cr
        ; {||} ^ cr
        ; {|toto|} ^ cr
        ; {|--9219489391874b51bb29b52a10e8baac|} ^ cr
        ; {|Content-Disposition: form-data; name="bar"; filename="filename.data"|} ^ cr
        ; {|Content-Type: application/octet-stream|} ^ cr
        ; {||} ^ cr
        ; {|line1|}
        ; {|line2|}
        ; {||}
        ]
      ; [ {|line3|}
        ; {|line4|}
        ; {||}
        ]
      ; [ {|line5|}
        ; {|line6|}
        ; {|--9219489391874b51bb29b52a10e8baac--|} ^ cr
        ; {||}
        ]
      ]
  in
  let stream = Lwt_stream.of_list chunks in
  let thread =
    let content_type = "multipart/form-data; boundary=9219489391874b51bb29b52a10e8baac" in
    let calls = ref [] in
    let callback ~name ~filename line =
      calls := !calls @ [(name, filename, line)];
      Lwt.return_unit
    in
    let%lwt parts = Multipart.parse ~stream ~content_type ~callback in
    assert_equal ~ctxt ~printer:[%show: (string * string) list] [("foo", "toto")] parts;
    assert_equal
      ~ctxt
      ~printer:[%show: (string * string * string) list]
      [ ("bar", "filename.data", "line1\nline2\n")
      ; ("bar", "filename.data", "line3\nline4\n")
      ; ("bar", "filename.data", "line5\nline6\n")
      ] !calls;
    Lwt.return_unit
  in
  Lwt_main.run thread

let test_split ctxt =
  let in_stream =
    Lwt_stream.of_list
      [ "ABCD"
      ; "EFap"
      ; "ple"
      ; "ABCDEFor"
      ; "angeABC"
      ; "HHpl"
      ; "umABCDEFkiwi"
      ; "ABCDEF"
      ]
  in
  let expected =
    [ ["ap" ; "ple"]
    ; ["or"; "ange"; "ABCHHpl"; "um"]
    ; ["kiwi"]
    ; []
    ]
  in
  let stream = Multipart.align in_stream "ABCDEF" in
  Lwt_main.run (
    let%lwt streams = Lwt_stream.to_list stream in
    let%lwt result = Lwt_list.map_s Lwt_stream.to_list streams in
    assert_equal
      ~ctxt
      ~printer:[%show: string list list]
      expected
      result;
    Lwt.return_unit
  )

let suite =
  "multipart-form-data" >:::
    [ "parse" >:: test_parse
    ; "parse_request" >:: test_parse_request
    ; "split" >:: test_split
    ]

let _ = run_test_tt_main suite
