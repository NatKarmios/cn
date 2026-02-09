open Sedap_types
open Util
open Log

let handle_initialize (module Rpc : Rpc) resolver =
  Rpc.handle_once
    (module Initialize_command)
    (fun init_args ->
       let caps =
         Capabilities.make
           ~supports_configuration_done_request:(Some true)
           ~supports_step_back:(Some true)
           ()
       in
       let debugger = Tree_debugger.make () in
       Lwt.wakeup_later resolver (init_args, caps, debugger);
       Lwt.return caps)


let handle_attach (module Rpc : Rpc) =
  Rpc.handle_once
    (module Attach_command)
    (fun _ -> Lwt.fail_with "Attach request is unsupported")


let handle_disconnect (module Rpc : Rpc) resolver =
  Rpc.handle_once
    (module Disconnect_command)
    (fun _ ->
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


(** Handles debug adapter initialization once the "initialize" command is received. *)
let run rpc =
  log_to_file "init phase";
  let promise, resolver = Lwt.task () in
  handle_initialize rpc resolver;
  handle_attach rpc;
  handle_disconnect rpc resolver;
  promise
