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
  | Step of
      { msg : string;
        get_state : unit -> state
      }
  | Nest of 'nest list
  | Step_in
  | Step_out

type t' = (string, string) Result.t

module T = Tree.Make_memoized (struct
    type nonrec 'nest breakpoint = 'nest breakpoint

    type case = string * int

    type 'a next = unit -> 'a

    type nest_result = t'

    let map_next n f = fun () -> f (n ())

    let compute_next n = n ()
  end)

type t = t' T.t

type next = t' T.next

let step get_state msg = T.Bp (Step { msg; get_state })

let nest nest = T.Bp (Nest nest)

let step_in = T.Bp Step_in

let step_out = T.Bp Step_out
