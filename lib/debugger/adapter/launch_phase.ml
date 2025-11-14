open Sedap_types
open Util

let handle_launch (module Cfg : Cfg) resolver =
  let module Cmd :
    COMMAND
    with type Arguments.t = Cfg.Launch_command.Arguments.t
     and type Result.t = Cfg.Launch_command.Result.t =
    Cfg.Launch_command
  in
  Cfg.handle_once
    (module Cmd)
    (fun launch_args ->
       match Cfg.launch launch_args with
       | Ok traces ->
         Trace_debugger.launch Cfg.dbg traces;
         Lwt.wakeup_later resolver ();
         Lwt.return_unit
       | Error e ->
         Lwt.wakeup_later_exn resolver Exit;
         Lwt.fail_with e)


let handle_disconnect (module Cfg : Cfg) resolver =
  Cfg.handle_once
    (module Disconnect_command)
    (fun _ ->
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


let run cfg =
  let promise, resolver = Lwt.task () in
  handle_launch cfg resolver;
  handle_disconnect cfg resolver;
  promise
