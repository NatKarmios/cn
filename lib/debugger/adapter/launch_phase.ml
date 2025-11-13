open Sedap_types
open Util

let handle_launch { rpc; dbg; launch; _ } resolver =
  let module Launch = (val launch : Launch) in
  Sedap_rpc.set_command_handler
    rpc
    (module Launch.Command)
    (fun launch_args ->
       Sedap_rpc.remove_command_handler rpc (module Launch.Command);
       let trace = Launch.launch launch_args in
       Trace_debugger.launch dbg trace;
       Lwt.wakeup_later resolver ();
       Lwt.return_unit)


let handle_disconnect { rpc; _ } resolver =
  Sedap_rpc.set_command_handler
    rpc
    (module Disconnect_command)
    (fun _ ->
       Sedap_rpc.remove_command_handler rpc (module Disconnect_command);
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


let run cfg =
  let promise, resolver = Lwt.task () in
  handle_launch cfg resolver;
  handle_disconnect cfg resolver;
  promise
