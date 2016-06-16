module StringMap = Map.Make(String)

let ends_with ~suffix s =
  let suffix_length = String.length suffix in
  let s_length = String.length s in
  if s_length >= suffix_length && Str.last_chars s suffix_length = suffix then
    let prefix = Str.first_chars s (s_length - suffix_length) in
    Some (prefix, suffix)
  else
    None

let prefixes s =
  let rec go i =
    if i < 0 then
      []
    else
      (String.sub s 0 i)::go (i-1)
  in
  go (String.length s)

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

let find_common a b =
  let p suffix =
    ends_with ~suffix a
  in
  first_matching p @@ prefixes b

let word = function
  | "" -> []
  | w -> [`Word w]

let split_and_process_string ~boundary s =
  let re = Str.regexp_string boundary in
  let rec go start acc =
    try
      let match_start = Str.search_forward re s start in
      let before = String.sub s start (match_start - start) in
      let new_acc = `Delim::(word before)@acc in
      let new_start = match_start + String.length boundary in
      go new_start new_acc
    with
      Not_found -> (word (Str.string_after s start))@acc
  in
  List.rev (go 0 [])

let split s boundary =
  let r = ref None in
  let push v =
    match !r with
    | None -> r := Some v
    | Some _ -> assert false
  in
  let pop () =
    let res = !r in
    r := None;
    res
  in
  let go c0 =
    let c =
      match pop () with
      | Some x -> x ^ c0
      | None -> c0
    in
    let string_to_process = match find_common c boundary with
    | None -> c
    | Some (prefix, suffix) ->
      begin
        push suffix;
        prefix
      end
    in
    Lwt.return @@ split_and_process_string ~boundary string_to_process
  in
  let initial = Lwt_stream.map_list_s go s in
  let final =
    Lwt_stream.flatten @@
    Lwt_stream.from_direct @@ fun () ->
    option_map (split_and_process_string ~boundary) @@ pop ()
  in
  Lwt_stream.append initial final

let until_next_delim s =
  Lwt_stream.from @@ fun () ->
  let%lwt res = Lwt_stream.get s in
  match res with
  | None
  | Some `Delim -> Lwt.return_none
  | Some (`Word w) -> Lwt.return_some w

let join s =
  Lwt_stream.filter_map (function
      | `Delim -> Some (until_next_delim @@ Lwt_stream.clone s)
      | `Word _ -> None
    ) s

let align stream boundary =
  join @@ split stream boundary

type header = string * string
  [@@deriving show]

let after_prefix ~prefix str =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  if (str_len >= prefix_len && Str.first_chars str prefix_len = prefix) then
    Some (Str.string_after str prefix_len)
  else
    None

let extract_boundary content_type =
  after_prefix ~prefix:"multipart/form-data; boundary=" content_type

let unquote s =
  Scanf.sscanf s "%S" @@ (fun x -> x);;

let parse_name s =
  option_map unquote @@ after_prefix ~prefix:"form-data; name=" s

let parse_header s =
  let regexp = Str.regexp_string ": " in
  let colon_pos = Str.search_forward regexp s 0 in
  let key = Str.string_before s colon_pos in
  let value = Str.string_after s (colon_pos + 2) in
  (key, value)

let non_empty st =
  let%lwt r = Lwt_stream.to_list @@ Lwt_stream.clone st in
  Lwt.return (String.concat "" r <> "")

let get_headers : string Lwt_stream.t Lwt_stream.t -> header list Lwt.t
  = fun lines ->
  let%lwt header_lines = Lwt_stream.get_while_s non_empty lines in
  Lwt_list.map_s (fun header_line_stream ->
      let%lwt parts = Lwt_stream.to_list header_line_stream in
      Lwt.return @@ parse_header @@ String.concat "" parts
    ) header_lines

type stream_part =
  { headers : header list
  ; body : string Lwt_stream.t
  }

let debug_stream_part {body;headers} =
  let%lwt body_chunks = Lwt_stream.to_list body in
  Lwt.return @@
  Printf.sprintf
    "headers : %s\nbody: %s\n"
    ([%show: header list] headers)
    (String.concat "" body_chunks)

let debug_stream sps =
  let%lwt parts = Lwt_list.map_s debug_stream_part sps in
  Lwt.return @@ String.concat "--\n" parts

let parse_part chunk_stream =
  let lines = align chunk_stream "\r\n" in
  match%lwt get_headers lines with
  | [] -> Lwt.return_none
  | headers ->
    let body = Lwt_stream.concat @@ Lwt_stream.clone lines in
    Lwt.return_some { headers ; body }

let parse_stream ~stream ~content_type =
  match extract_boundary content_type with
  | None -> Lwt.fail_with "Cannot parse content-type"
  | Some boundary ->
    begin
      let actual_boundary = ("--" ^ boundary) in
      Lwt.return @@ Lwt_stream.filter_map_s parse_part @@ align stream actual_boundary
    end

let s_part_body {body} = body

let s_part_name {headers} =
  match
    parse_name @@ List.assoc "Content-Disposition" headers
  with
  | Some x -> x
  | None -> invalid_arg "s_part_name"

let parse_regexp regexp s =
  if Str.string_match regexp s 0 then
    Some (Str.matched_group 1 s)
  else None

let parse_filename =
  parse_regexp @@ Str.regexp {|^form-data; name=".*"; filename="\(.*\)"|}

let s_part_filename {headers} =
  parse_filename @@ List.assoc "Content-Disposition" headers

type file = stream_part

let file_stream = s_part_body
let file_name = s_part_name

let file_content_type {headers} =
  List.assoc "Content-Type" headers

let as_part part =
  match s_part_filename part with
  | Some filename ->
      Lwt.return (`File part)
  | None ->
    let%lwt chunks = Lwt_stream.to_list part.body in
    let body = String.concat "" chunks in
    Lwt.return (`String body)

let get_parts s =
  let go part m =
    let name = s_part_name part in
    let%lwt parsed_part = as_part part in
    Lwt.return @@ StringMap.add name parsed_part m
  in
  Lwt_stream.fold_s go s StringMap.empty
