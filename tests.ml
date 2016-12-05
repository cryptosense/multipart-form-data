open OUnit2

let tc ctxt content_type chunks expected_parts expected_calls =
  let stream = Lwt_stream.of_list chunks in
  let calls = ref [] in
  let callback ~name ~filename line =
    calls := !calls @ [(name, filename, line)];
    Lwt.return_unit
  in
  let%lwt parts = Multipart.parse ~stream ~content_type ~callback in
  assert_equal ~ctxt ~printer:[%show: (string * string) list] expected_parts  parts;
  assert_equal
    ~ctxt
    ~printer:[%show: (string * string * string) list]
    expected_calls
     !calls;
  Lwt.return_unit

let test_parse_request ctxt =
  let cr = "\r" in
  let lf = "\n" in
  let crlf = cr ^ lf in
  let thread =
    tc ctxt
      "multipart/form-data; boundary=9219489391874b51bb29b52a10e8baac"
      ( List.map (String.concat "\n") @@
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
      )
      [ ("foo", "toto") ]
      [ ("bar", "filename.data", "line1\nline2\n")
      ; ("bar", "filename.data", "line3\nline4\n")
      ; ("bar", "filename.data", "line5\nline6\n")
      ]
      >>
    tc ctxt
      "multipart/form-data; boundary=9219489391874b51bb29b52a10e8baac"
      (
        [ {|--9219489391874b51bb29b52a10e8baac|} ^ crlf
        ; {|Content-Disposition: form-data; name="foo"|} ^ crlf
        ; crlf
        ; {|toto|} ^ crlf
        ; {|--9219489391874b|}
        ; {|51bb29b52a10e8baac--|} ^ crlf
        ]
      )
      [ ("foo", "toto") ]
      []
  in
  Lwt_main.run thread

let suite =
  "multipart-form-data" >:::
    [ "parse_request" >:: test_parse_request
    ]

let _ = run_test_tt_main suite
