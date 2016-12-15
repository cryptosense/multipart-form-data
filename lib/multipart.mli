(**
   Align a stream on a particular sequence and remove these boundaries.
 *)
val align : string Lwt_stream.t -> string -> string Lwt_stream.t Lwt_stream.t

type stream_part

val s_part_name : stream_part -> string

val s_part_body : stream_part -> string Lwt_stream.t

val s_part_filename : stream_part -> string option

val parse_stream : stream:string Lwt_stream.t -> content_type:string -> stream_part Lwt_stream.t Lwt.t

type file

val file_name : file -> string
val file_content_type : file -> string
val file_stream : file -> string Lwt_stream.t

module StringMap : Map.S with type key = string

val get_parts : stream_part Lwt_stream.t -> [`String of string | `File of file] StringMap.t Lwt.t

(** Low level interface.

    To parse only string parts, just call [parse ~content_type body]: it will
    return an association list.

    To parse a file pass a callback in [callbacks] using the part name, a
    reference and a function. The reference will be set to the file name, and
    the function will be called on file chunks to save the file. String parts
    are returned as an association list.

    When a file part is encountered and no corresponding callback exists, the
    [default_callback] function is called on every chunk.
*)
val parse :
     content_type:string
     -> ?default_callback:(name:string -> filename:string -> string -> unit Lwt.t)
     -> ?callbacks:((string * (string option ref * (string -> unit Lwt.t))) list)
     -> string Lwt_stream.t
     -> (string * string) list Lwt.t
