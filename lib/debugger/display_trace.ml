module Variable = struct
  type t =
    { name : string;
      value : string;
      type_ : string option;
      children : t list
    }

  type ts = (string * t) list
end

type state = { vars : Variable.ts }

type 'nest breakpoint =
  { msg : string;
    nest : 'nest list;
    get_state : unit -> state
  }

type t' = (string, string) Result.t

module T = Trace.Make_memoized (struct
    type nonrec 'nest breakpoint = 'nest breakpoint

    type case = int

    type 'a next = unit -> 'a

    type nest_result = t'

    let map_next n f = fun () -> f (n ())

    let compute_next n = n ()

    let get_nested b = b.nest
  end)

type t = t' T.t

type next = t' T.next

let breakpoint ~get_state ~nest ~msg = T.Bp { msg; nest; get_state }
