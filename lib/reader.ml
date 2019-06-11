module StringMap = Map.Make(String)

let string_eq ~a ~a_start ~b ~len =
  let r = ref true in
  for i = 0 to len - 1 do
    let a_i = a_start + i in
    let b_i = i in
    if a.[a_i] <> b.[b_i] then
      r := false
  done;
  !r

let ends_with ~suffix ~suffix_length s =
  let s_length = String.length s in
  (s_length >= suffix_length) &&
  (string_eq ~a:s ~a_start:(s_length - suffix_length) ~b:suffix ~len:suffix_length)

let rec first_matching p = function
  | [] -> None
  | x::xs ->
    begin
      match p x with
      | Some y -> Some y
      | None -> first_matching p xs
    end

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let find_common_idx a b =
  let rec go i =
    if i <= 0 then
      None
    else
      begin
        if ends_with ~suffix:b ~suffix_length:i a then
          Some (String.length a - i)
        else
          go (i - 1)
      end
  in
  go (String.length b)

let word = function
  | "" -> []
  | w -> [Some w]

let split_on_string ~pattern s =
  let pattern_length = String.length pattern in
  let rec go start acc =
    match Stringext.find_from ~start s ~pattern with
    | Some match_start ->
      let before = String.sub s start (match_start - start) in
      let new_acc = None::(word before)@acc in
      let new_start = match_start + pattern_length in
      go new_start new_acc
    | None ->
      (word (Stringext.string_after s start))@acc
  in
  List.rev (go 0 [])

let extract_boundary content_type =
  Stringext.chop_prefix ~prefix:"multipart/form-data; boundary=" content_type

let unquote s =
  Scanf.sscanf s "%S" @@ (fun x -> x);;

let parse_name s =
  option_map unquote @@ Stringext.chop_prefix ~prefix:"form-data; name=" s

let parse_header s =
  match Stringext.cut ~on:": " s with
  | Some (key, value) -> (key, value)
  | None -> invalid_arg ("Could not parse header :" ^ s)

let parse_filename s =
  let parts = split_on_string s ~pattern:"; " in
  let f = function
    | None -> None
    | Some part ->
      begin
        match Stringext.cut part ~on:"=" with
        | Some ("filename", quoted_string) -> Some (unquote quoted_string)
        | _ -> None
      end
  in
  first_matching f parts

let concat a b =
  match (a, b) with
  | (_, "") -> a
  | ("", _) -> b
  | _ -> a ^ b

module Reader = struct
  type t =
    { mutable buffer : string
    ; source : string Lwt_stream.t
    }

  let make stream =
    { buffer = ""
    ; source = stream
    }

  let unread r s =
    r.buffer <- concat s r.buffer

  let empty r =
    if r.buffer = "" then
      Lwt_stream.is_empty r.source
    else
      Lwt.return false

  let read_next r =
    let%lwt next_chunk = Lwt_stream.next r.source in
    r.buffer <- concat r.buffer next_chunk;
    Lwt.return_unit

  let read_chunk r =
    try%lwt
      let%lwt () =
        if r.buffer = "" then
          read_next r
        else
          Lwt.return_unit
      in
      let res = r.buffer in
      r.buffer <- "";
      Lwt.return (Some res)
    with Lwt_stream.Empty ->
      Lwt.return None

  let buffer_contains r s =
    match Stringext.cut r.buffer ~on:s with
    | Some _ -> true
    | None -> false

  let rec read_until r cond =
    if cond () then
      Lwt.return_unit
    else
      begin
        let%lwt () = read_next r in
        read_until r cond
      end

  let read_line r =
    let delim = "\r\n" in
    let%lwt () = read_until r (fun () -> buffer_contains r delim) in
    match Stringext.cut r.buffer ~on:delim with
    | None -> assert false
    | Some (line, next) ->
      begin
        r.buffer <- next;
        Lwt.return (line ^ delim)
      end
end

let read_inital_comments boundary reader =
  let rec go comments =
    let%lwt line = Reader.read_line reader in
    if line = boundary ^ "\r\n" then
      Lwt.return comments
    else
      go (comments ^ line)
  in
  go ""


let read_headers reader =
  let rec go headers =
    let%lwt line = Reader.read_line reader in
    if line = "\r\n" then
      Lwt.return headers
    else
      let header = parse_header line in
      go (header::headers)
  in
  go []

let rec compute_case reader boundary =
  match%lwt Reader.read_chunk reader with
  | None -> Lwt.return `Empty
  | Some line ->
    begin
      match Stringext.cut line ~on:("\r\n" ^ boundary ^ "\r\n") with
      | Some (pre, post) -> Lwt.return @@ `Boundary (pre, post)
      | None ->
        begin
          match Stringext.cut line ~on:("\r\n" ^ boundary ^ "--\r\n") with
          | Some (pre, post) -> Lwt.return @@ `Boundary (pre, post)
          | None ->
            begin
              match find_common_idx line ("\r\n" ^ boundary) with
              | Some 0 ->
                begin
                  Reader.unread reader line;
                  let%lwt () = Reader.read_next reader in
                  compute_case reader boundary
                end
              | Some amb_idx ->
                let unambiguous = String.sub line 0 amb_idx in
                let ambiguous = String.sub line amb_idx (String.length line - amb_idx) in
                Lwt.return @@ `May_end_with_boundary (unambiguous, ambiguous)
              | None -> Lwt.return @@ `App_data line
            end
        end
    end

(**
 * Read file part
 * We construct a progressive stream that
 * gets passed to the callback.
 **)
let iter_part reader boundary callback =
  let fin = ref false in
  let last () =
    fin := true;
    Lwt.return_unit
  in
  let handle ~send ~unread ~finish =
    let%lwt () = callback send finish in
    Reader.unread reader unread;
    if finish then
      last ()
    else
      Lwt.return_unit
  in
  while%lwt not !fin do
    let%lwt res = compute_case reader boundary in
    match res with
    | `Empty
      ->
      last ()
    | `Boundary (pre, post)
      ->
      handle ~send:pre ~unread:post ~finish:true
    | `May_end_with_boundary (unambiguous, ambiguous)
      ->
      handle ~send:unambiguous ~unread:ambiguous ~finish:false
    | `App_data line
      ->
      callback line false
  done

let read_file_part reader boundary callback =
  iter_part reader boundary callback

(**
 * Read string part
 * We construct a buffer that will contain the entirety of
 * the value before being passed to the callback.
 **)
let read_part reader boundary callback =
  let%lwt headers = read_headers reader in
  let content_disposition = List.assoc "Content-Disposition" headers in
  let name =
    match parse_name content_disposition with
    | Some x -> x
    | None -> invalid_arg "handle_multipart"
  in
  let filename = parse_filename content_disposition in
  read_file_part reader boundary (callback ~name ~filename)

let handle_multipart reader boundary callback =
  let%lwt read_multipart =
    let%lwt _comments = read_inital_comments boundary reader in
    let fin = ref false in
    while%lwt not !fin do
      if%lwt Reader.empty reader then
        Lwt.return (fin := true)
      else
        read_part reader boundary callback
    done
  in
  Lwt.return read_multipart

let parse ~stream ~content_type ~callback =
  let reader = Reader.make stream in
  let boundary =
    match extract_boundary content_type with
    | Some s -> "--" ^ s
    | None -> invalid_arg ("Could not extract boundary from Content-Type : " ^ content_type)
  in
  try
    handle_multipart reader boundary callback
    |> Lwt_main.run
    |> fun _ -> Ok ()
  with
  | Invalid_argument e -> Error e
