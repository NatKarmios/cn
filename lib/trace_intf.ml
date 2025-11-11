module Types = struct
  type ('result, 'breakpoint, 'choice_case, 'next) trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of ('choice_case * 'next) list
end

include Types

module type Args = sig
  type breakpoint

  type case

  type 'a next

  val map_next : 'a next -> ('a -> 'b) -> 'b next

  val compute_next : 'a next -> 'a
end

module type S = sig
  type breakpoint

  type case

  type 'a next'

  type 'a next

  type 'a t = ('a, breakpoint, case, 'a next) trace

  val next : 'a t next' -> 'a next

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val flaky_fold : ('b -> 'a -> 'b) -> 'b -> ('a, 'c) Result.t t -> ('b, 'c) Result.t
end

module type Make = functor (A : Args) ->
  S
  with type breakpoint = A.breakpoint
   and type case = A.case
   and type 'a next' = 'a A.next

module type Intf = sig
  module type Args = Args

  module type S = S

  module type Make = Make

  module Make : Make

  module Make_memoized : Make

  type ('result, 'breakpoint, 'choice_case, 'next) trace =
        ('result, 'breakpoint, 'choice_case, 'next) Types.trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of ('choice_case * 'next) list
end
