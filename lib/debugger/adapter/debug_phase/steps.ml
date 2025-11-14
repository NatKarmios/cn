open Sedap_types
open Util
open Trace_debugger

let handle_continue (module Cfg : Cfg) =
  Cfg.handle
    (module Continue_command)
    (fun _ ->
       Cfg.send_stopped_event (continue Cfg.dbg);%lwt
       Lwt.return (Continue_command.Result.make ()))


let handle_next (module Cfg : Cfg) =
  Cfg.handle (module Next_command) (fun _ -> Cfg.send_stopped_event (step_over Cfg.dbg))


let handle_reverse_continue (module Cfg : Cfg) =
  Cfg.handle
    (module Reverse_continue_command)
    (fun _ -> Cfg.send_stopped_event (continue_back Cfg.dbg))


let handle_step_back (module Cfg : Cfg) =
  Cfg.handle
    (module Step_back_command)
    (fun _ -> Cfg.send_stopped_event (step_back Cfg.dbg))


let handle_step_in (module Cfg : Cfg) =
  Cfg.handle (module Step_in_command) (fun _ -> Cfg.send_stopped_event (step_in Cfg.dbg))


let handle_step_out (module Cfg : Cfg) =
  Cfg.handle
    (module Step_out_command)
    (fun _ -> Cfg.send_stopped_event (step_out Cfg.dbg))


let handle_jump (module Cfg : Cfg) =
  Cfg.handle
    (module Jump_command)
    (fun { step_id } ->
       jump Cfg.dbg step_id;
       Lwt.return_unit)


let handle_step_specific (module Cfg : Cfg) =
  Cfg.handle
    (module Step_specific_command)
    (fun { step_id; branch_case } ->
       Cfg.send_stopped_event (step_specific Cfg.dbg step_id branch_case))


let handle cfg =
  handle_continue cfg;
  handle_next cfg;
  handle_reverse_continue cfg;
  handle_step_back cfg;
  handle_step_in cfg;
  handle_step_out cfg;
  handle_jump cfg;
  handle_step_specific cfg
