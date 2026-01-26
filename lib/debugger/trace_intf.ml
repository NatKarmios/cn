module Types = struct
  type ('result, 'breakpoint, 'next, 'choice) trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of 'breakpoint * 'choice
end

include Types

module type Delayed = sig
  type 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val compute : 'a t -> 'a
end

module type Delayed_memo = sig
  include Delayed

  val poll : 'a t -> 'a option
end

module type Args' = sig
  module type Delayed

  type 'nest breakpoint

  type case

  type nest_result

  val get_nested : 'nest breakpoint -> 'nest list

  module Next : Delayed

  module Choice : Delayed
end

module type Args = Args' with module type Delayed := Delayed

module type Args_memo = Args' with module type Delayed := Delayed_memo

module type S = sig
  type 'n breakpoint'

  type case

  type 'a next

  type 'a choice

  type nest_result

  type breakpoint = Bp of nest_result next breakpoint'

  type 'a t = ('a, breakpoint, 'a next, 'a choice) trace

  module Next : sig
    type 'a pre

    val compute : 'a next -> 'a t

    val make : 'a t pre -> 'a next

    val bind : 'a next -> ('a -> 'b t) -> 'b next

    val map : 'a next -> ('a -> 'b) -> 'b next

    type 'a t = 'a next
  end

  module Choice : sig
    type 'a pre

    type 'a cases = (case * 'a Next.t) list

    val compute : 'a choice -> 'a cases

    val make : (case * 'a t Next.pre) list pre -> 'a choice

    val bind : 'a choice -> ('a -> 'b t) -> 'b choice

    val map : 'a choice -> ('a -> 'b) -> 'b choice

    type 'a t = 'a choice
  end

  val breakpoint : nest_result next breakpoint' -> 'a t Next.pre -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

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
     and type 'a Next.pre = 'a A.Next.t
     and type 'a Choice.pre = 'a A.Choice.t
     and type nest_result = A.nest_result

  module Make_memoized (A : Args_memo) : sig
    include
      S
      with type 'n breakpoint' := 'n A.breakpoint
       and type case = A.case
       and type 'a Next.pre = 'a A.Next.t
       and type 'a Choice.pre = 'a A.Choice.t
       and type nest_result = A.nest_result

    module Next : sig
      val poll : 'a Next.t -> 'a t option

      val to_memo : 'a Next.t -> 'a t Memo.t

      val of_memo : 'a t Memo.t -> 'a Next.t

      include module type of Next
    end

    module Choice : sig
      include module type of Choice

      val poll : 'a Choice.t -> 'a cases option

      val to_memo : 'a Choice.t -> 'a cases Memo.t

      val of_memo : 'a cases Memo.t -> 'a Choice.t
    end

    val poll_fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b option
  end

  type ('result, 'breakpoint, 'next, 'choice) trace =
        ('result, 'breakpoint, 'next, 'choice) Types.trace =
    | End of 'result
    | Vanish
    | Breakpoint of 'breakpoint * 'next
    | Choice of 'breakpoint * 'choice
end
