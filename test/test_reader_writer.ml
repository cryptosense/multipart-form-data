let rec to_seq = function
    | h::t -> (fun () -> Seq.Cons (h, to_seq t))
    | [] -> (fun () -> Seq.Nil)

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
    Alcotest.(check (result unit string)) (name ^ " read result") (Ok ()) result;
    let resulting_parts = read () in
    Alcotest.(check int)
      (name ^ " read parts vs expected parts")
      (List.length expected_parts)
      (List.length resulting_parts);
    let request =
      match Multipart_form_data.write_with_boundary
              ~boundary:Utils.boundary
              ~request:(to_seq expected_parts)
      with
      | Ok r -> r
      | Error _ -> Utils.empty_request
    in
    Alcotest.(check string) (name ^ " body") input (Utils.stream_to_string request.body)
  )


let read_write_tests =
  [ test
      ~name:"Simple form"
      ~input:
        (Printf.sprintf
           "\
            \r\n--%s\r\n\
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
          ; content_length = Some (Int64.of_int 5)
          ; filename = None
          }
        ]
  ; test
      ~name:"File from string"
      ~input:
        (Printf.sprintf
           "\
            \r\n--%s\r\n\
            Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\"\r\n\
            Content-Type: application/octet-stream\r\n\
            \r\n\
            this is the content of our file\r\n\
            \r\n\
            --%s--\r\n\
           "
           Utils.boundary
           Utils.boundary
        )
      ~expected_parts:
        [ { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is the content of our file\r\n"]
          ; content_length = Some (Int64.of_int 38)
          }
        ]
  ; test
      ~name:"Mixed variable and file"
      ~input:
        (Printf.sprintf
           "\
              \r\n--%s\r\n\
              Content-Disposition: form-data; name=\"var1\"\r\n\
              \r\n\
              \r\ntest\r\n\
              \r\n--%s\r\n\
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
        [ { Multipart_form_data.Part.name = "var1"
          ; content = Lwt_stream.of_list [ "\r\ntest\r\n" ]
          ; content_length = Some (Int64.of_int 8)
          ; filename = None
          }
        ; { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is \r\nthe content of our file\r\n"]
          ; content_length = Some (Int64.of_int 38)
          }
        ; { Multipart_form_data.Part.name = "var2"
          ; content = Lwt_stream.of_list [ "end===stuff" ]
          ; content_length = Some (Int64.of_int 11)
          ; filename = None
          }
        ]
  ]
