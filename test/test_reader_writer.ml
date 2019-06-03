open Utils

let test ~name ~input ~expected_parts =
  ( name
  , `Quick
  , fun () ->
    let request =
      { Multipart_form_data.Request.headers = test_headers
      ; body = Lwt_stream.of_list [ input ]
      }
    in
    let (callback, read) = testable_callback_factory () in
    let result = Multipart_form_data.read ~request ~handle_part:callback in
    Alcotest.(check (result unit string)) (name ^ " read result") (Ok ()) result;
    let resulting_parts = read () in
    Alcotest.(check int)
      (name ^ " read parts vs expected parts")
      (List.length expected_parts)
      (List.length resulting_parts);
    let request =
      match Multipart_form_data.write_with_separator
              ~separator
              ~request:(List.to_seq expected_parts)
      with
      | Ok r -> r
      | Error _ -> empty_request
    in
    Alcotest.(check string) (name ^ " body") input (stream_to_string request.body)
  )


let read_write_tests =
  [ test
      ~name:"Simple form"
      ~input:("\r\n--" ^ separator
              ^ "\r\n"
              ^ "Content-Disposition: form-data; name=\"key\""
              ^ "\r\n" ^ "\r\n"
              ^ "value"
              ^ "\r\n"
              ^ "--" ^ separator ^ "--"
              ^ "\r\n"
             )
      ~expected_parts:[{ Multipart_form_data.Part.name = "key"
                       ; value = Variable "value"
                       }]
  ; test
      ~name:"File from string"
      ~input:("\r\n--" ^ separator
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
      ~expected_parts:[{ Multipart_form_data.Part.name = "filename"
                       ; value = File { filename = "originalname"
                                      ; content = Lwt_stream.of_list ["this is the content of our file\r\n"]
                                      ; length = Some (Int64.of_int 33)
                                      }
                       }]
  ; test
      ~name:"Mixed variable and file"
      ~input:("\r\n--" ^ separator
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
      ~expected_parts:[ { Multipart_form_data.Part.name = "var1"
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
  ]
