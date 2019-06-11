let () =
  Alcotest.run "multipart-form-data" [
    ("Multipart_form_data - read",
     Test_reader.reader_tests
    )
  ; ("Multipart_form_data - write",
     Test_writer.writer_tests
    )
  ; ("Multipart_form_data - read & write",
     Test_reader_writer.read_write_tests
    )
  ]
