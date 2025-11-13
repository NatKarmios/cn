open Sedap_types
open Util

let handle_breakpoints { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Set_breakpoints_command)
    (fun { source; breakpoints; _ } ->
       let lines =
         breakpoints
         |> Option.value ~default:[]
         |> List.map (fun bp -> bp.Source_breakpoint.line)
       in
       Trace_debugger.set_breakpoints dbg source lines;
       let breakpoints =
         lines
         |> List.map (fun line ->
           Breakpoint.make ~id:(Some line) ~verified:true ~line:(Some line) ())
       in
       Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()))


let handle_config_done { rpc; _ } resolver =
  Sedap_rpc.set_command_handler
    rpc
    (module Configuration_done_command)
    (fun () ->
       Sedap_rpc.remove_command_handler rpc (module Configuration_done_command);
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
  handle_breakpoints cfg;
  handle_config_done cfg resolver;
  handle_disconnect cfg resolver;
  promise
