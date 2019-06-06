module Request : sig
  type t =
    { headers : (string * string) list
    ; body : string Lwt_stream.t
    }
end

module Part : sig
  type t =
    { name: string
    ; filename: string option
    ; content_length: int64 option
    ; content: string Lwt_stream.t
    }
end

val read :
  request:Request.t
  -> handle_part:(Part.t -> unit Lwt.t)
  -> (unit, string) result

val write_with_boundary :
  boundary:string
  -> request:Part.t Seq.t
  -> (Request.t, string) result

val write :
  request:Part.t Seq.t
  -> (Request.t, string) result
