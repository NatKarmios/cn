open Sedap_types

let handle_initialize rpc resolver =
  Debug_rpc.set_command_handler
    rpc
    (module Initialize_command)
    (fun init_args ->
       Sedap_rpc.remove_command_handler rpc (module Initialize_command);
       let caps =
         Capabilities.make
           ~supports_configuration_done_request:(Some true)
           ~supports_step_back:(Some true)
           ()
       in
       let debugger = Trace_debugger.make () in
       Lwt.wakeup_later resolver (init_args, caps, debugger);
       Lwt.return caps)


let handle_attach rpc =
  Sedap_rpc.set_command_handler
    rpc
    (module Attach_command)
    (fun _ ->
       Sedap_rpc.remove_command_handler rpc (module Attach_command);
       Lwt.fail_with "Attach request is unsupported")


let handle_disconnect rpc resolver =
  Sedap_rpc.set_command_handler
    rpc
    (module Disconnect_command)
    (fun _ ->
       Sedap_rpc.remove_command_handler rpc (module Disconnect_command);
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


(** Handles debug adapter initialization once the "initialize" command is received. *)
let run rpc =
  let promise, resolver = Lwt.task () in
  handle_initialize rpc resolver;
  handle_attach rpc;
  handle_disconnect rpc resolver;
  promise
