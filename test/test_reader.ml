let test ~name ~input ~expected_parts =
  ( name
  , `Quick
  , fun () ->
    let request =
      { Multipart_form_data.Request.headers = Utils.test_headers
      ; body = Lwt_stream.of_list [ input ]
      }
    in
    let (callback, read) = Utils.testable_callback_factory () in
    let result = Multipart_form_data.read ~request ~handle_part:callback in
    let resulting_parts =
      read ()
    in
    Alcotest.(check (result unit string)) (name ^ " result") (Ok ()) result;
    Alcotest.(check int)
      (name ^ " part count")
      (List.length expected_parts)
      (List.length resulting_parts);
    Utils.test_parts
      ~name:(name ^ "parts")
      ~expected:expected_parts
      resulting_parts
  )

let reader_tests =
  [ test
      ~name:"Simple form"
      ~input:
        (Printf.sprintf
           "\
                \r\n\
                --%s\r\n\
                Content-Disposition: form-data; name=\"key\"\r\n\
                \r\n\
                value\r\n\
                --%s--\r\n\
            "
           Utils.boundary
           Utils.boundary
        )
      ~expected_parts:
        [ { Multipart_form_data.Part.name = "key"
          ; content = Lwt_stream.of_list [ "value" ]
          ; content_length = None
          ; filename = None
          }
        ]
  ; test
      ~name:"File"
      ~input:
        (Printf.sprintf
           "\
                \r\n\
                --%s\r\n\
                Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\"\r\n\
                Content-Type: application/octet-stream\r\n\
                \r\n\
                this is the content of our file\r\n\r\n\
                --%s--\r\n\
            "
           Utils.boundary
           Utils.boundary
        )
      ~expected_parts:
        [ { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is the content of our file\r\n"]
          ; content_length = None
          }
        ]

  ; test
      ~name:"Mixed"
      ~input:
        (Printf.sprintf
           "\
            \r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var1\"\r\n\
            \r\n\
            \r\ntest\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\"\r\n\
            Content-Type: application/octet-stream\r\n\
            \r\n\
            this is \r\nthe content of our file\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var2\"\r\n\
            \r\n\
            end===stuff\
            \r\n\
            --%s--\r\n\
            "
           Utils.boundary
           Utils.boundary
           Utils.boundary
           Utils.boundary
        )
      ~expected_parts:
        [ { Multipart_form_data.Part.name = "var2"
          ; content = Lwt_stream.of_list [ "end===stuff" ]
          ; content_length = None
          ; filename = None
          }
        ; { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is \r\nthe content of our file\r\n"]
          ; content_length = None
          }
        ; { Multipart_form_data.Part.name = "var1"
          ; content = Lwt_stream.of_list [ "\r\ntest\r\n" ]
          ; content_length = None
          ; filename = None
          }
        ]
  ; test
      ~name:"Double file"
      ~input:
        (Printf.sprintf
           "\
            \r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var1\"\r\n\
            \r\n\
            \r\ntest\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\"\r\n\
            Content-Type: application/octet-stream\r\n\
            \r\n\
            this is \r\nthe content of our file\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"filename2\"; filename=\"originalname2\"\r\n\
            Content-Type: application/octet-stream\r\n\
            \r\n\
            this is \r\nthe content of another file\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var2\"\r\n\
            \r\n\
            end===stuff\
            \r\n\
            --%s--\r\n\
            "
           Utils.boundary
           Utils.boundary
           Utils.boundary
           Utils.boundary
           Utils.boundary
        )
      ~expected_parts:
        [ { Multipart_form_data.Part.name = "var2"
          ; content = Lwt_stream.of_list [ "end===stuff" ]
          ; content_length = None
          ; filename = None
          }
        ; { Multipart_form_data.Part.name = "filename2"
          ; filename = Some "originalname2"
          ; content = Lwt_stream.of_list ["this is \r\nthe content of another file\r\n"]
          ; content_length = None
          }
        ; { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is \r\nthe content of our file\r\n"]
          ; content_length = None
          }
        ; { Multipart_form_data.Part.name = "var1"
          ; content = Lwt_stream.of_list [ "\r\ntest\r\n" ]
          ; content_length = None
          ; filename = None
          }
        ]
  ]
