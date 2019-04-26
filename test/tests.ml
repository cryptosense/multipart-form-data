let () =
  Alcotest.run "multipart-form-data" [
    ("Multipart_form_data.Reader",
     [ "parse", `Quick, Test_reader.test_parse
     ; "parse_easy_request", `Quick, Test_reader.test_parse_request_easy
     ; "parse_complex_request", `Quick, Test_reader.test_parse_request_complex
     ; "split", `Quick, Test_reader.test_split
     ]
    )
  ; ("Multipart_form_data.Writer",
     Test_writer.writer_tests
    )
  ; ("Multipart_form_data.Reader & Writer",
     Test_read_write.read_write_tests
    )
  ]
