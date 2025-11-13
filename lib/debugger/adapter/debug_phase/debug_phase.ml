open Sedap_types
open Util

let handle_disconnect { rpc; dbg; _ } resolver =
  Sedap_rpc.set_command_handler
    rpc
    (module Disconnect_command)
    (fun _ ->
       Sedap_rpc.remove_command_handler rpc (module Disconnect_command);
       Trace_debugger.terminate dbg;
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


let run cfg =
  let promise, resolver = Lwt.task () in
  handle_disconnect cfg resolver;
  Inspect.handle cfg;
  Steps.handle cfg;
  promise
