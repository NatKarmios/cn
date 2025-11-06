type id

type t

val make : unit -> t

val insert : ?prev:id -> Explain.log_entry -> t -> id

val dump : t -> unit
