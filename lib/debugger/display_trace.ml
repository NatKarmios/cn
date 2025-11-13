type 'nest breakpoint' = string * 'nest list

include Trace.Make_memoized (struct
    type 'nest breakpoint = 'nest breakpoint'

    type case = int * string

    type 'a next = unit -> 'a

    let map_next n f = fun () -> f (n ())

    let compute_next n = n ()

    let get_nested (_, nest) = nest
  end)
