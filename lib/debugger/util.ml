open Sedap_types
open Log

type stop_reason = Stopped_event.Payload.Reason.t

module type T = sig
  val type_ : string
end

let get_command_type_
      (type a b)
      (module C : COMMAND with type Arguments.t = a and type Result.t = b)
  =
  C.type_


let get_event_type_ (type a) (module E : EVENT with type Payload.t = a) = E.type_

module type Rpc = sig
  val rpc : Sedap_rpc.t

  val handle
    :  (module COMMAND with type Arguments.t = 'a and type Result.t = 'b) ->
    ('a -> 'b Lwt.t) ->
    unit

  val unhandle : (module COMMAND) -> unit

  val handle_once
    :  (module COMMAND with type Arguments.t = 'a and type Result.t = 'b) ->
    ('a -> 'b Lwt.t) ->
    unit

  val send : (module EVENT with type Payload.t = 'a) -> 'a -> unit Lwt.t

  val send_stopped : ?thread_id:int option -> stop_reason -> unit Lwt.t
end

let make_rpc () : (module Rpc) =
  (module struct
    let rpc =
      let in_, out = Lwt_io.(stdin, stdout) in
      Sedap_rpc.create ~in_ ~out ()


    let handle cmd f =
      let type_ = get_command_type_ cmd in
      Sedap_rpc.set_command_handler rpc cmd (fun x ->
        log_to_file ("handling " ^ type_);
        try%lwt f x with
        | exc ->
          let bt = Printexc.get_raw_backtrace () in
          let exc' = Printexc.to_string exc in
          let bt' = Printexc.raw_backtrace_to_string bt in
          log_to_file (bt' ^ "\n" ^ exc');
          Printexc.raise_with_backtrace exc bt)


    let unhandle cmd = Sedap_rpc.remove_command_handler rpc cmd

    let handle_once (type a b) cmd f =
      let module Cmd : COMMAND =
        (val cmd : COMMAND with type Arguments.t = a and type Result.t = b)
      in
      handle cmd (fun x ->
        unhandle (module Cmd);
        f x)


    let send ev p =
      log_to_file ("sending " ^ get_event_type_ ev);
      Sedap_rpc.send_event rpc ev p


    let send_stopped ?(thread_id = Some 0) reason =
      let payload = Stopped_event.Payload.(make ~reason ~thread_id ()) in
      send (module Stopped_event) payload
  end)


module type Launch_command = COMMAND with type Result.t = Launch_command.Result.t

module type Launch = sig
  module Launch_command : Launch_command

  type trees := ((string * Display_tree.t) list, string) result

  val launch : Launch_command.Arguments.t -> trees
end

let make_launch
      (type a)
      (module Launch_command : Launch_command with type Arguments.t = a)
      launch
  : (module Launch)
  =
  (module struct
    module Launch_command = Launch_command

    let launch = launch
  end)


module type Cfg = sig
  include Rpc

  include Launch

  val init_args : Initialize_command.Arguments.t

  val dbg : Tree_debugger.t
end

let make_cfg (module Rpc : Rpc) init_args dbg (module Launch : Launch) : (module Cfg) =
  (module struct
    include Rpc

    let init_args = init_args

    let dbg = dbg

    let send_stopped ?thread_id reason =
      Rpc.send_stopped ?thread_id reason;%lwt
      Rpc.send (module Map_update_event) (Tree_debugger.get_map_update dbg)


    include Launch
  end)
