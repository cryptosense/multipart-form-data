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
  let m =
    match Multipart.parse ~body ~content_type with
    | Some x -> x
    | None -> assert_failure "expected a result for parse"
  in
  assert_equal
    ~ctxt
    ~printer:[%show: int]
    ~msg:(Printf.sprintf "part names = %s" ([%show: string list] (Multipart.part_names m)))
    3
    (Multipart.num_parts m);
  let get_partx name =
    match Multipart.get_part m name with
    | Some x -> x
    | None -> assert_failure ("expected a result for get_part")
  in
  let part_a = get_partx "a" in
  assert_equal ~ctxt ~printer:[%show: string]
    ~msg:(Multipart.debug m)
    "b" (Multipart.part_body part_a);
  let part_c = get_partx "c" in
  assert_equal ~ctxt ~printer:[%show: string] "d" (Multipart.part_body part_c);
  let part_upload = get_partx "upload" in
  assert_equal ~ctxt ~printer:[%show: string] "testfilecontent" (Multipart.part_body part_upload)

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
