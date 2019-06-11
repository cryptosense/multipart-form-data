val stream_to_string : string Lwt_stream.t -> string
val test_parts :
  name:string ->
  expected:Multipart_form_data.Part.t list ->
  Multipart_form_data.Part.t list ->
  unit
val testable_callback_factory : unit -> (('a -> unit Lwt.t) * (unit -> 'a list))

val empty_request : Multipart_form_data.Request.t
val boundary : string
val test_headers : (string * string) list
