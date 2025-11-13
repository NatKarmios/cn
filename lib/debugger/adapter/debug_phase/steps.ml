open Sedap_types
open Util
open Trace_debugger

let handle_continue { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Continue_command)
    (fun _ ->
       send_stopped_event rpc (continue dbg);%lwt
       Lwt.return (Continue_command.Result.make ()))


let handle_next { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Next_command)
    (fun _ -> send_stopped_event rpc (step_over dbg))


let handle_reverse_continue { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Reverse_continue_command)
    (fun _ -> send_stopped_event rpc (continue_back dbg))


let handle_step_back { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Step_back_command)
    (fun _ -> send_stopped_event rpc (step_back dbg))


let handle_step_in { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Step_in_command)
    (fun _ -> send_stopped_event rpc (step_in dbg))


let handle_step_out { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Step_out_command)
    (fun _ -> send_stopped_event rpc (step_out dbg))


let handle_jump { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Jump_command)
    (fun { step_id } ->
       jump dbg step_id;
       Lwt.return_unit)


let handle_step_specific { rpc; dbg; _ } =
  Sedap_rpc.set_command_handler
    rpc
    (module Step_specific_command)
    (fun { step_id; branch_case } ->
       send_stopped_event rpc (step_specific dbg step_id branch_case))


let handle cfg =
  handle_continue cfg;
  handle_next cfg;
  handle_reverse_continue cfg;
  handle_step_back cfg;
  handle_step_in cfg;
  handle_step_out cfg;
  handle_jump cfg;
  handle_step_specific cfg
