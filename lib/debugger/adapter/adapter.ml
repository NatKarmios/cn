open Sedap_types
open Util

let run_debugger rpc launch =
  try%lwt
    let%lwt init_args, _caps, dbg = Init_phase.run rpc in
    let cfg = { rpc; dbg; init_args; launch } in
    Sedap_rpc.send_event rpc (module Initialized_event) ();%lwt
    let%lwt () = Config_phase.run cfg in
    let%lwt _launch_args = Launch_phase.run cfg in
    let%lwt () = Debug_phase.run cfg in
    Lwt.return_unit
  with
  | Exit -> Lwt.return_unit


let start launch =
  Lwt_main.run
  @@ try%lwt
       let rpc =
         let in_, out = Lwt_io.(stdin, stdout) in
         Sedap_rpc.create ~in_ ~out ()
       in
       let cancel = ref (fun () -> ()) in
       Lwt.async (fun () ->
         run_debugger rpc launch;%lwt
         !cancel ();
         Lwt.return_unit);
       let loop = Sedap_rpc.start rpc in
       (cancel := fun () -> Lwt.cancel loop);
       let%lwt () = try%lwt loop with Lwt.Canceled -> Lwt.return_unit in
       Lwt.return ()
     with
     | exn ->
       (* TODO: handle error *)
       Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())
