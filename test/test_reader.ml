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

let test_parse () =
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
    let%lwt parts_stream = Multipart_form_data.Reader.parse_stream ~stream ~content_type in
    let%lwt parts = Multipart_form_data.Reader.get_parts parts_stream in
    Alcotest.check string_or_file "'a' value" (`String "b") (Multipart_form_data.Reader.StringMap.find "a" parts);
    Alcotest.check string_or_file "'c' value" (`String "d") (Multipart_form_data.Reader.StringMap.find "c" parts);
    let file = get_file "upload" parts in
    Alcotest.check Alcotest.string "filename" "upload" (Multipart_form_data.Reader.file_name file);
    Alcotest.check Alcotest.string "content_type" "application/octet-stream" (Multipart_form_data.Reader.file_content_type file);
    let%lwt file_chunks = Lwt_stream.to_list (Multipart_form_data.Reader.file_stream file) in
    Alcotest.check Alcotest.string "contents" "testfilecontent" (String.concat "" file_chunks);
    Lwt.return_unit
  in
  Lwt_main.run thread

let tc content_type chunks expected_parts expected_calls =
  let stream = Lwt_stream.of_list chunks in
  let calls = ref [] in
  let callback ~name ~filename line =
    calls := !calls @ [((name, filename), line)];
    Lwt.return_unit
  in
  let%lwt parts = Multipart_form_data.Reader.parse ~stream ~content_type ~callback in
  let string2_list = Alcotest.(list (pair string string)) in
  let string3_list = Alcotest.(list (pair (pair string string) string)) in
  Alcotest.check string2_list "parts" expected_parts parts;
  Alcotest.check string3_list "calls"
    (List.map (fun (x, y, z) -> ((x, y), z)) expected_calls)
    !calls;
  Lwt.return_unit

let test_parse_request_complex () =
  let cr = "\r" in
  let lf = "\n" in
  let crlf = cr ^ lf in
  let thread =
    tc "multipart/form-data; boundary=9219489391874b51bb29b52a10e8baac"
      ( List.map (String.concat lf) @@
        [ [ {|--9219489391874b51bb29b52a10e8baac|} ^ cr
          ; {|Content-Disposition: form-data; name="foo"|} ^ cr
          ; {||} ^ cr
          ; {|toto|} ^ cr
          ; {|--9219489391874b51bb29b52a10e8baac|} ^ cr
          ; {|Content-Disposition: form-data; name="bar"; filename="filename.data"|} ^ cr
          ; {|Content-Type: application/octet-stream|} ^ cr
          ; {||} ^ cr
          ; {|line1|} ^ crlf
          ; {|line2|}
          ; {||}
          ]
        ; [ {|line3|}
          ; {|line4|}
          ; {||}
          ]
        ; [ {|line5|}
          ; {|line6|} ^ crlf ^ cr
          ; {|--9219489391874b51bb29b52a10e8baac--|} ^ cr
          ; {||}
          ]
        ]
      )
      [ ("foo", "toto") ]
      [ ("bar", "filename.data", "line1\r\n\nline2\n")
      ; ("bar", "filename.data", "line3\nline4\n")
      ; ("bar", "filename.data", "line5\nline6\r\n")
      ]
  in
  Lwt_main.run thread

let test_parse_request_easy () =
  let cr = "\r" in
  let lf = "\n" in
  let crlf = cr ^ lf in
  let thread =
    tc
      "multipart/form-data; boundary=9219489391874b51bb29b52a10e8baac"
      (
        [ {|--9219489391874b51bb29b52a10e8baac|} ^ crlf
        ; {|Content-Disposition: form-data; name="foo"|} ^ crlf
        ; crlf
        ; {|toto|} ^ crlf
        ; {|--9219489391874b51bb29b52a10e8baac--|} ^ crlf
        ]
      )
      [ ("foo", "toto") ]
      []
  in
  Lwt_main.run thread

let test_split () =
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
  let stream = Multipart_form_data.Reader.align in_stream "ABCDEF" in
  Lwt_main.run (
    let%lwt streams = Lwt_stream.to_list stream in
    let%lwt result = Lwt_list.map_s Lwt_stream.to_list streams in
    Alcotest.check Alcotest.(list (list string)) "contents" expected result;
    Lwt.return_unit
  )

