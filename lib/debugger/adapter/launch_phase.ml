open Sedap_types
open Util
open Log

let handle_breakpoints (module Cfg : Cfg) =
  Cfg.handle
    (module Set_breakpoints_command)
    (fun { source; breakpoints; _ } ->
       let lines =
         breakpoints
         |> Option.value ~default:[]
         |> List.map (fun bp -> bp.Source_breakpoint.line)
       in
       Tree_debugger.set_breakpoints Cfg.dbg source lines;
       let breakpoints =
         lines
         |> List.map (fun line ->
           Breakpoint.make ~id:(Some line) ~verified:true ~line:(Some line) ())
       in
       Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()))


let handle_config_done (module Cfg : Cfg) =
  Cfg.handle_once (module Configuration_done_command) (fun () -> Lwt.return_unit)


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
       log_to_file "launching!";
       match Cfg.launch launch_args with
       | Ok traces ->
         Tree_debugger.launch Cfg.dbg traces;
         Cfg.send_stopped Stopped_event.Payload.Reason.Step;%lwt
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
  log_to_file "launch phase";
  let promise, resolver = Lwt.task () in
  handle_breakpoints cfg;
  handle_config_done cfg;
  handle_launch cfg resolver;
  handle_disconnect cfg resolver;
  promise
