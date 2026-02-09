open Sedap_types
open Util
open Log

let handle_disconnect (module Cfg : Cfg) resolver =
  Cfg.handle_once
    (module Disconnect_command)
    (fun _ ->
       Tree_debugger.terminate Cfg.dbg;
       Lwt.wakeup_later_exn resolver Exit;
       Lwt.return_unit)


let run cfg =
  log_to_file "debug phase";
  let promise, resolver = Lwt.task () in
  handle_disconnect cfg resolver;
  Inspect.handle cfg;
  Steps.handle cfg;
  promise
