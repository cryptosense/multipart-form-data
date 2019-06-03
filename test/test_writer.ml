open Utils

let test ~name ~input ~expected_headers ~expected_body =
  ( name
  , `Quick
  , fun () ->
    let request =
      match Multipart_form_data.write_with_separator ~separator ~request:(List.to_seq input) with
      | Ok r -> r
      | Error _ -> empty_request
    in
    Alcotest.(check (list (pair string string))) (name ^ " headers") expected_headers request.headers;
    Alcotest.(check string) (name ^ " body") expected_body (stream_to_string request.body)
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
      ~input:[{ Multipart_form_data.Part.name = "key"
              ; value = Variable "value"
              }]
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
  ; test
      ~name:"File from string"
      ~input:[{ Multipart_form_data.Part.name = "filename"
              ; value = File { filename = "originalname"
                             ; content = Lwt_stream.of_list ["this is the content of our file\r\n"]
                             ; length = Some (Int64.of_int 33)
                             }
              }]
      ~expected_headers:[("Content-Type", "multipart/form-data; boundary=" ^ separator);
                         ("Content-Length", "213")]
      ~expected_body:("\r\n--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\""
                      ^ "\r\n"
                      ^ "Content-Type: application/octet-stream"
                      ^ "\r\n" ^ "\r\n"
                      ^ "this is the content of our file\r\n"
                      ^ "\r\n"
                      ^ "--" ^ separator ^ "--"
                      ^ "\r\n"
                     )
  ; test
      ~name:"Mixed variable and file"
      ~input:[ { Multipart_form_data.Part.name = "var1"
               ; value = Variable "\r\ntest\r\n"
               }
             ; { Multipart_form_data.Part.name = "filename"
               ; value = File { filename = "originalname"
                              ; content = Lwt_stream.of_list ["this is \r\nthe content of our file\r\n"]
                              ; length = Some (Int64.of_int 35)
                              }
               }
             ; { Multipart_form_data.Part.name = "var2"
               ; value = Variable "end===stuff"
               }
             ]
      ~expected_headers:[("Content-Type", "multipart/form-data; boundary=" ^ separator);
                         ("Content-Length", "371")]
      ~expected_body:("\r\n--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"var1\""
                      ^ "\r\n" ^ "\r\n"
                      ^ "\r\ntest\r\n"
                      ^ "\r\n"
                      ^ "--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\""
                      ^ "\r\n"
                      ^ "Content-Type: application/octet-stream"
                      ^ "\r\n" ^ "\r\n"
                      ^ "this is \r\nthe content of our file\r\n"
                      ^ "\r\n"
                      ^ "--" ^ separator
                      ^ "\r\n"
                      ^ "Content-Disposition: form-data; name=\"var2\""
                      ^ "\r\n" ^ "\r\n"
                      ^ "end===stuff"
                      ^ "\r\n"
                      ^ "--" ^ separator ^ "--"
                      ^ "\r\n"
                     )
  ]
