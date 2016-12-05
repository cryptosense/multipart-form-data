(** Parse a stream of strings (for example corresponding to a
    [Cohttp_lwt_body.t]) as a multipart/form-data document (RFC2388).

    [content_type] is the Content-type header of the request, used to determine
    the boundary between parts.

    There are two kinds of parts that can be read:

    - file parts (with a [filename] field in their [Content-Disposition]
      header) are parsed using the [callback] argument. It will receive the part
      name, the filename and a chunk of data.
    - string parts (without a [filename]) are collected and returned in an
      association list.
    *)
val parse :
        stream:string Lwt_stream.t
     -> content_type:string
     -> callback:(name:string -> filename:string -> string -> unit Lwt.t)
     -> (string * string) list Lwt.t
