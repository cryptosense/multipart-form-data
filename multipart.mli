type t

val split_join : string Lwt_stream.t -> string -> string Lwt_stream.t Lwt_stream.t

val debug : t -> string

val parse : body:string -> content_type:string -> t option

val num_parts : t -> int

type part

val get_part : t -> string -> part option

val part_body : part -> string

val part_names : t -> string list

type stream_part

val s_part_name : stream_part -> string

val s_part_body : stream_part -> string Lwt_stream.t

val parse_stream : stream:string Lwt_stream.t -> content_type:string -> stream_part Lwt_stream.t Lwt.t
