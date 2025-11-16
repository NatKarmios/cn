type t

type stop_reason := Sedap_types.Stopped_event.Payload.Reason.t

val make : unit -> t

val launch : t -> (string * Display_trace.t) list -> unit

val step_in : t -> stop_reason

val step_over : t -> stop_reason

val step_out : t -> stop_reason

val step_back : t -> stop_reason

val continue : t -> stop_reason

val continue_back : t -> stop_reason

val jump : t -> string -> unit

val terminate : t -> unit

val get_frames : t -> Sedap_types.Stack_frame.t list

val get_scopes : t -> Sedap_types.Scope.t list

val get_variables : t -> int -> Sedap_types.Variable.t list

val set_breakpoints : t -> Sedap_types.Source.t -> int list -> unit

val get_map_update : t -> Sedap_types.Map_update_event_body.t

val get_full_map : t -> Sedap_types.Map_update_event_body.t
