module Types = struct
  type ('result, 'breakpoint, 'choice_case, 'next) trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of ('choice_case * 'next) list
end

include Types

module type Args = sig
  type 'nest breakpoint

  type case

  type 'a next

  val map_next : 'a next -> ('a -> 'b) -> 'b next

  val compute_next : 'a next -> 'a

  val get_nested : 'nest breakpoint -> 'nest list
end

module type S = sig
  type 'n breakpoint'

  type case

  type 'a next'

  type 'a next

  type 'a t = ('a, breakpoint, case, 'a next) trace

  and breakpoint = Bp of unit next breakpoint'

  val compute_next : 'a next -> 'a t

  val next : 'a t next' -> 'a next

  val breakpoint : unit next breakpoint' -> 'a t next' -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val bind_next : 'a next -> ('a -> 'b t) -> 'b next

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val flaky_fold : ('b -> 'a -> 'b) -> 'b -> ('a, 'c) Result.t t -> ('b, 'c) Result.t
end

module type Intf = sig
  module type Args = Args

  module type S = S

  module Make (A : Args) :
    S
    with type 'n breakpoint' := 'n A.breakpoint
     and type case = A.case
     and type 'a next' := 'a A.next

  module Make_memoized (A : Args) : sig
    include
      S
      with type 'n breakpoint' := 'n A.breakpoint
       and type case = A.case
       and type 'a next' := 'a A.next

    val poll_next : 'a next -> 'a t option
  end

  type ('result, 'breakpoint, 'choice_case, 'next) trace =
        ('result, 'breakpoint, 'choice_case, 'next) Types.trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of ('choice_case * 'next) list
end
