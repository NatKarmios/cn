open Sedap_types
open Util
open Trace_debugger

let handle_threads { rpc; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Threads_command)
    (fun () ->
       let main_thread = Thread.make ~id:0 ~name:"main" in
       Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()))


let handle_stack_trace { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Stack_trace_command)
    (fun _ ->
       let stack_frames = get_frames dbg in
       Lwt.return Stack_trace_command.Result.(make ~stack_frames ()))


let handle_scopes { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Scopes_command)
    (fun _ ->
       let scopes = get_scopes dbg in
       Lwt.return Scopes_command.Result.(make ~scopes ()))


let handle_variables { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Variables_command)
    (fun { variables_reference; _ } ->
       let variables = get_variables dbg variables_reference in
       Lwt.return Variables_command.Result.(make ~variables ()))


let handle_full_map { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Get_full_map_command)
    (fun () -> Lwt.return (get_full_map dbg))


let handle cfg =
  handle_threads cfg;
  handle_stack_trace cfg;
  handle_scopes cfg;
  handle_variables cfg;
  handle_full_map cfg
