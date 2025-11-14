open Sedap_types
open Util

let handle_breakpoints (module Cfg : Cfg) =
  Cfg.handle
    (module Set_breakpoints_command)
    (fun { source; breakpoints; _ } ->
       let lines =
         breakpoints
         |> Option.value ~default:[]
         |> List.map (fun bp -> bp.Source_breakpoint.line)
       in
       Trace_debugger.set_breakpoints Cfg.dbg source lines;
       let breakpoints =
         lines
         |> List.map (fun line ->
           Breakpoint.make ~id:(Some line) ~verified:true ~line:(Some line) ())
       in
       Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()))


let handle_config_done (module Cfg : Cfg) resolver =
  Cfg.handle_once
    (module Configuration_done_command)
    (fun () ->
       Lwt.wakeup_later resolver ();
       Lwt.return_unit)


let handle_disconnect (module Cfg : Cfg) resolver =
  Cfg.handle_once
    (module Disconnect_command)
    (fun _ ->
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


let run cfg =
  let promise, resolver = Lwt.task () in
  handle_breakpoints cfg;
  handle_config_done cfg resolver;
  handle_disconnect cfg resolver;
  promise
