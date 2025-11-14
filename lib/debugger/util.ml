open Sedap_types

type stop_reason = Stopped_event.Payload.Reason.t

let send_stopped_event rpc ?(thread_id = Some 0) reason =
  let payload = Stopped_event.Payload.(make ~reason ~thread_id ()) in
  Sedap_rpc.send_event rpc (module Stopped_event) payload


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

  val send_stopped_event : ?thread_id:int option -> stop_reason -> unit Lwt.t
end

let make_rpc () : (module Rpc) =
  (module struct
    let rpc =
      let in_, out = Lwt_io.(stdin, stdout) in
      Sedap_rpc.create ~in_ ~out ()


    let handle cmd f = Sedap_rpc.set_command_handler rpc cmd f

    let unhandle cmd = Sedap_rpc.remove_command_handler rpc cmd

    let handle_once (type a b) cmd f =
      let module Cmd : COMMAND =
        (val cmd : COMMAND with type Arguments.t = a and type Result.t = b)
      in
      handle cmd (fun x ->
        unhandle (module Cmd);
        f x)


    let send_stopped_event = send_stopped_event rpc
  end)


module type Launch_command = COMMAND with type Result.t = Launch_command.Result.t

module type Launch = sig
  module Launch_command : Launch_command

  val launch : Launch_command.Arguments.t -> (Display_trace.t list, string) result
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

  val dbg : Trace_debugger.t
end

let make_cfg (module Rpc : Rpc) init_args dbg (module Launch : Launch) : (module Cfg) =
  (module struct
    include Rpc

    let init_args = init_args

    let dbg = dbg

    include Launch
  end)
