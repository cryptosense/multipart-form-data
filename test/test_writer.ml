let rec to_seq = function
    | h::t -> (fun () -> Seq.Cons (h, to_seq t))
    | [] -> (fun () -> Seq.Nil)

let test ~name ~input ~expected_headers ~expected_body =
  ( name
  , `Quick
  , fun () ->
    let request =
      match
        Multipart_form_data.write
          ~boundary:Utils.boundary
          ~parts:(to_seq input)
      with
      | Ok r -> r
      | Error _ -> Utils.empty_request
    in
    Alcotest.(check (list (pair string string))) (name ^ " headers") expected_headers request.headers;
    Alcotest.(check string) (name ^ " body") expected_body (Utils.stream_to_string request.body)
  )

let writer_tests =
  [ test
      ~name:"Empty"
      ~input:[]
      ~expected_headers:
        [ ("Content-Type", "multipart/form-data; boundary=" ^ Utils.boundary)
        ; ("Content-Length", "33")
        ]
      ~expected_body:("\r\n--" ^ Utils.boundary ^ "--\r\n")
  ; test
      ~name:"Simple form"
      ~input:
        [ { Multipart_form_data.Part.name = "key"
          ; content = Lwt_stream.of_list [ "value" ]
          ; content_length = Some (Int64.of_int 5)
          ; filename = None
          }
        ]
      ~expected_headers:
        [ ("Content-Type", "multipart/form-data; boundary=" ^ Utils.boundary);
          ("Content-Length", "115")
        ]
      ~expected_body:
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
  ; test
      ~name:"File from string"
      ~input:
        [ { Multipart_form_data.Part.name = "filename"
          ; filename = Some "originalname"
          ; content = Lwt_stream.of_list ["this is the content of our file\r\n"]
          ; content_length = Some (Int64.of_int 33)
          }
        ]
      ~expected_headers:
        [ ("Content-Type", "multipart/form-data; boundary=" ^ Utils.boundary);
          ("Content-Length", "213")
        ]
      ~expected_body:
        (Printf.sprintf
           "\
             \r\n\
             --%s\r\n\
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
  ; test
      ~name:"Mixed variable and file"
      ~input:
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
      ~expected_headers:
        [ ("Content-Type", "multipart/form-data; boundary=" ^ Utils.boundary);
          ("Content-Length", "393")
        ]
      ~expected_body:
        (Printf.sprintf
           "\
            \r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var1\"\r\n\
            \r\n\
            \r\ntest\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"filename\"; filename=\"originalname\"\
            \r\n\
            Content-Type: application/octet-stream\r\n\
            \r\n\
            this is \r\nthe content of our file\r\n\r\n\
            --%s\r\n\
            Content-Disposition: form-data; name=\"var2\"\r\n\
            \r\n\
            end===stuff\r\n\
            --%s--\
            \r\n\
           "
           Utils.boundary
           Utils.boundary
           Utils.boundary
           Utils.boundary
        )
  ]
