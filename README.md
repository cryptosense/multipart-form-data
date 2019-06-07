multipart/form-data (RFC2388) parser for OCaml
==============================================

 [![Build Status](https://travis-ci.org/cryptosense/multipart-form-data.svg?branch=master)](https://travis-ci.org/cryptosense/multipart-form-data) [![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://cryptosense.github.io/multipart-form-data/doc/)

This is a parser for structured form data based on `Lwt_stream` in order to use
it with [cohttp](https://github.com/mirage/ocaml-cohttp/). You can use it to
receive and send POST parameters.

# Cohttp

This library integrates well with cohttp and cohttp-lwt.

### Read API

Example with a cohttp-lwt request :
```ocaml
open Lwt.Infix

let request =
    Cohttp_lwt_unix.Client.get ~ctx (Uri.of_string "http://url/")
    >>= fun (resp, body) ->
        { Multipart_part_form.Request.headers = resp |> Cohttp.Header.to_list
        ; body = body |> Cohttp_lwt.to_stream
        }

let callback part =
    (* Do stuff with the elements of the response *)

Multipart_part_form.read request callback
```

Example with a cohttp-lwt server :
```ocaml
open Cohttp
open Cohttp_lwt_unix

let file_upload_callback part =
  Lwt.return (print_endline ("Incoming variable '" ^ part.Multipart_form_data.Part.name ^ "'"))

let server =
  let callback _conn req body =
    let _uri = req |> Request.uri |> Uri.to_string in
    let _meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_list in
    body
    |> Cohttp_lwt.Body.to_stream
    |> (fun body ->
        { Multipart_form_data.Request.headers = headers
        ; body = body
        }
      )
    |> (fun request ->
        Multipart_form_data.read ~request ~handle_part:file_upload_callback
      )
    |> (fun result ->
        match result with
        | Ok () -> Server.respond_string ~status:`OK ~body:"Success" ()
        | Error e -> print_endline e; Server.respond_string ~status:`Bad_request ~body:e ()
      )
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = Lwt_main.run server
```

### Write API

Example with cohttp-lwt :
```ocaml
open Lwt
open Cohttp
open Cohttp_lwt_unix

let multipart =
  [ { Multipart_form_data.Part.name = "var"
    ; filename = None
    ; content = Lwt_stream.of_list [ "content" ]
    ; content_length = Some (Int64.of_int 7)
    }
  ]
  |> List.to_seq

let request =
  Multipart_form_data.write ~request:multipart
  |> function
      | Ok r -> r
      | Error _ -> failwith "Failure"

let body =
  (Client.post
     ~body:(request.Multipart_form_data.Request.body |> Cohttp_lwt.Body.of_stream)
     ~headers:(request.headers |> Cohttp.Header.of_list)
     (Uri.of_string "http://localhost:8000")
  )
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
```
