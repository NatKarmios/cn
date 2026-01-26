module Variable = struct
  type t =
    { name : string;
      value : string;
      type_ : string option;
      children : t list
    }

  type ts = (string * t list) list
end

type stack_frame =
  { index : int;
    name : string;
    source : string option;
    start_line : int;
    start_column : int;
    end_line : int option;
    end_column : int option
  }

type state =
  { vars : Variable.ts;
    frames : stack_frame list
  }

type 'nest breakpoint =
  { msg : string;
    nest : 'nest list;
    get_state : unit -> state
  }

type t' = (string, string) Result.t

module Delayed_simple = struct
  type 'a t = unit -> 'a

  let map d f = fun () -> f (d ())

  let compute d = d ()

  let poll _ = None
end

module T = Trace.Make_memoized (struct
    type nonrec 'nest breakpoint = 'nest breakpoint

    type case = string

    type nest_result = t'

    let get_nested b = b.nest

    module Next = Delayed_simple
    module Choice = Delayed_simple
  end)

type t = t' T.t

type next = t' T.next

type choice = t' T.choice

let breakpoint ~get_state ~nest ~msg = T.Bp { msg; nest; get_state }
