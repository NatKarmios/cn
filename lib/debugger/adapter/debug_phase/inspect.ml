open Sedap_types
open Util
open Tree_debugger

let handle_threads (module Cfg : Cfg) =
  Cfg.handle
    (module Threads_command)
    (fun () ->
       let main_thread = Thread.make ~id:0 ~name:"main" in
       Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()))


let handle_stack_trace (module Cfg : Cfg) =
  Cfg.handle
    (module Stack_trace_command)
    (fun _ ->
       let stack_frames = get_frames Cfg.dbg in
       Lwt.return Stack_trace_command.Result.(make ~stack_frames ()))


let handle_scopes (module Cfg : Cfg) =
  Cfg.handle
    (module Scopes_command)
    (fun _ ->
       let scopes = get_scopes Cfg.dbg in
       Lwt.return Scopes_command.Result.(make ~scopes ()))


let handle_variables (module Cfg : Cfg) =
  Cfg.handle
    (module Variables_command)
    (fun { variables_reference; _ } ->
       let variables = get_variables Cfg.dbg variables_reference in
       Lwt.return Variables_command.Result.(make ~variables ()))


let handle_full_map (module Cfg : Cfg) =
  Cfg.handle (module Get_full_map_command) (fun () -> Lwt.return (get_full_map Cfg.dbg))


let handle cfg =
  handle_threads cfg;
  handle_stack_trace cfg;
  handle_scopes cfg;
  handle_variables cfg;
  handle_full_map cfg
