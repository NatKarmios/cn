module Types = struct
  type ('result, 'breakpoint, 'choice_case, 'next) trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of 'breakpoint * ('choice_case * 'next) list
end

include Types

module type Args = sig
  type 'nest breakpoint

  type case

  type 'a next

  type nest_result

  val map_next : 'a next -> ('a -> 'b) -> 'b next

  val compute_next : 'a next -> 'a

  val get_nested : 'nest breakpoint -> 'nest list
end

module type S = sig
  type 'n breakpoint'

  type case

  type 'a next'

  type 'a next

  type nest_result

  type breakpoint = Bp of nest_result next breakpoint'

  type 'a t = ('a, breakpoint, case, 'a next) trace

  val compute_next : 'a next -> 'a t

  val next : 'a t next' -> 'a next

  val breakpoint : nest_result next breakpoint' -> 'a t next' -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val bind_next : 'a next -> ('a -> 'b t) -> 'b next

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val map_next : 'a next -> ('a -> 'b) -> 'b next

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
     and type nest_result = A.nest_result

  module Make_memoized (A : Args) : sig
    include
      S
      with type 'n breakpoint' := 'n A.breakpoint
       and type case = A.case
       and type 'a next' := 'a A.next
       and type nest_result = A.nest_result

    val poll_next : 'a next -> 'a t option

    val next_to_memo : 'a next -> 'a t Memo.t

    val next_of_memo : 'a t Memo.t -> 'a next
  end

  type ('result, 'breakpoint, 'choice_case, 'next) trace =
        ('result, 'breakpoint, 'choice_case, 'next) Types.trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of 'breakpoint * ('choice_case * 'next) list
end
