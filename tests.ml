open OUnit2

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
  Lwt_main.run @@
    let open Lwt.Infix in
    Multipart.parse_stream ~stream ~content_type >>= fun parts_stream ->
    Multipart.get_parts parts_stream >>= fun parts ->
    assert_equal (Multipart.Text "b") (Multipart.get_part parts "a");
    assert_equal (Multipart.Text "d") (Multipart.get_part parts "c");
    match Multipart.get_part parts "upload" with
    | Multipart.Text _ -> assert_failure "expected a file"
    | Multipart.File file ->
      begin
        assert_equal ~ctxt ~printer:[%show: string] "upload" (Multipart.file_name file);
        assert_equal ~ctxt "application/octet-stream" (Multipart.file_content_type file);
        Lwt_stream.to_list (Multipart.file_stream file) >>= fun file_chunks ->
        assert_equal ~ctxt "testfilecontent" (String.concat "" file_chunks);
        Lwt.return_unit
      end

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
  let join_stream = Multipart.split_join in_stream "ABCDEF" in
  let xss =
    let open Lwt.Infix in
    Lwt_main.run (
      Lwt_stream.to_list join_stream >>= fun streams ->
      Lwt_list.map_s Lwt_stream.to_list streams
    )
  in
  assert_equal
    ~ctxt
    ~printer:[%show: string list list]
    expected
    xss
    

let suite =
  "multipart-form-data" >:::
    [ "parse" >:: test_parse
    ; "split" >:: test_split
    ]

let _ = run_test_tt_main suite
