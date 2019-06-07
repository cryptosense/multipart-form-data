module MultipartRequest : sig
    type t
end

val init : string -> MultipartRequest.t
val add_from_stream : 
    name:string
    -> filename:string option
    -> content:string Lwt_stream.t
    -> content_length:int64
    -> MultipartRequest.t
    -> MultipartRequest.t
val r_headers : MultipartRequest.t -> ((string * string) list, 'a) result
val r_body : MultipartRequest.t -> (string Lwt_stream.t, 'a) result
