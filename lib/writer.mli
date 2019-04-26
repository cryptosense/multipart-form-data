module MultipartRequest : sig
  type t
end

val init : unit -> MultipartRequest.t
val init_with_separator : string -> MultipartRequest.t
val add_form_element : name:string -> value:string -> MultipartRequest.t -> MultipartRequest.t
val add_file_from_disk : name:string -> path:string -> MultipartRequest.t -> MultipartRequest.t
val add_file_from_string : name:string -> content:string -> MultipartRequest.t -> MultipartRequest.t
val add_file_from_stream :
  name:string ->
  content:(string Lwt_stream.t) ->
  content_length:int ->
  MultipartRequest.t ->
  MultipartRequest.t

val r_body : MultipartRequest.t -> (string Lwt_stream.t, string) Lwt_result.t
val r_headers : MultipartRequest.t -> ((string * string) list, string) Lwt_result.t
