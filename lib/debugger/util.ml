open Sedap_types

module type Launch = sig
  module Command : COMMAND with type Result.t = unit

  val launch : Command.Arguments.t -> bool Display_trace.t
end

type cfg =
  { rpc : Sedap_rpc.t;
    init_args : Initialize_command.Arguments.t;
    dbg : Trace_debugger.t;
    launch : (module Launch)
  }

let send_stopped_event rpc ?(thread_id = Some 0) reason =
  let payload = Stopped_event.Payload.(make ~reason ~thread_id ()) in
  Sedap_rpc.send_event rpc (module Stopped_event) payload
