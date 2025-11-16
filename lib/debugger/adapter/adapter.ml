open Sedap_types
open Util
open Log

let run_debugger rpc launch =
  let module Rpc = (val rpc : Rpc) in
  try%lwt
    log_to_file "run_debugger";
    let%lwt init_args, _caps, dbg = Init_phase.run rpc in
    let cfg = make_cfg rpc init_args dbg launch in
    Rpc.send (module Initialized_event) ();%lwt
    let%lwt _launch_args = Launch_phase.run cfg in
    let%lwt () = Debug_phase.run cfg in
    Lwt.return_unit
  with
  | Exit -> Lwt.return_unit


let start launch_command launch =
  reset_log_file ();
  log_to_file "Started";
  let launch = make_launch launch_command launch in
  Lwt_main.run
  @@ try%lwt
       let module Rpc = (val make_rpc () : Rpc) in
       let cancel = ref (fun () -> ()) in
       Lwt.async (fun () ->
         run_debugger (module Rpc) launch;%lwt
         !cancel ();
         Lwt.return_unit);
       let loop = Sedap_rpc.start Rpc.rpc in
       (cancel := fun () -> Lwt.cancel loop);
       let%lwt () = try%lwt loop with Lwt.Canceled -> Lwt.return_unit in
       Lwt.return ()
     with
     | exn ->
       (* TODO: handle error *)
       Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())
