val parse :
  stream:string Lwt_stream.t
  -> content_type:string
  -> callback:(
      name:string -> filename:string option -> string -> bool -> unit Lwt.t
    )
  -> (unit, string) result
