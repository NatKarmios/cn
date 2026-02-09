open Sedap_types
open Util
open Tree_debugger

let handle_continue (module Cfg : Cfg) =
  Cfg.handle
    (module Continue_command)
    (fun _ ->
       Cfg.send_stopped (continue Cfg.dbg);%lwt
       Lwt.return (Continue_command.Result.make ()))


let handle_next (module Cfg : Cfg) =
  Cfg.handle (module Next_command) (fun _ -> Cfg.send_stopped (step_over Cfg.dbg))


let handle_reverse_continue (module Cfg : Cfg) =
  Cfg.handle
    (module Reverse_continue_command)
    (fun _ -> Cfg.send_stopped (continue_back Cfg.dbg))


let handle_step_back (module Cfg : Cfg) =
  Cfg.handle (module Step_back_command) (fun _ -> Cfg.send_stopped (step_back Cfg.dbg))


let handle_step_in (module Cfg : Cfg) =
  Cfg.handle (module Step_in_command) (fun _ -> Cfg.send_stopped (step_in Cfg.dbg))


let handle_step_out (module Cfg : Cfg) =
  Cfg.handle (module Step_out_command) (fun _ -> Cfg.send_stopped (step_out Cfg.dbg))


let handle_jump (module Cfg : Cfg) =
  Cfg.handle
    (module Jump_command)
    (fun { step_id } ->
       jump Cfg.dbg step_id;
       Cfg.send_stopped Stopped_event.Payload.Reason.Step)


let handle_step_in_at (module Cfg : Cfg) =
  Cfg.handle
    (module Step_in_at_command)
    (fun { step_id } ->
       let stop_reason = step_in_at Cfg.dbg step_id in
       Cfg.send_stopped stop_reason)


let handle_step_over_at (module Cfg : Cfg) =
  Cfg.handle
    (module Step_over_at_command)
    (fun { step_id } ->
       let stop_reason = step_over_at Cfg.dbg step_id in
       Cfg.send_stopped stop_reason)


let handle cfg =
  handle_continue cfg;
  handle_next cfg;
  handle_reverse_continue cfg;
  handle_step_back cfg;
  handle_step_in cfg;
  handle_step_out cfg;
  handle_jump cfg;
  handle_step_in_at cfg;
  handle_step_over_at cfg
