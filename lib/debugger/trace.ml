include Trace_intf

module Make (A : Args) = struct
  type case = A.case

  type nest_result = A.nest_result

  type 'a t = ('a, breakpoint, 'a next, 'a choice) trace

  and 'a next = Nx of 'a t A.Next.t

  and 'a choice = Ch of (case * 'a next) list A.Choice.t

  and breakpoint = Bp of nest_result next A.breakpoint

  let next n = Nx n

  let breakpoint b n = Breakpoint (Bp b, next n)

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with
    | End x -> f x
    | Vanish -> Vanish
    | Breakpoint (b, n) -> Breakpoint (b, bind_next n f)
    | Choice (b, cs) -> Choice (b, bind_choices cs f)


  and bind_next (Nx n : 'a next) (f : 'a -> 'b t) : 'b next =
    Nx (A.Next.map n (fun m -> bind m f))


  and bind_choices (Ch c : 'a choice) (f : 'a -> 'b t) : 'b choice =
    Ch (A.Choice.map c (fun choices -> List.map_snd (fun n -> bind_next n f) choices))


  let return x = End x

  module Next = struct
    type 'a pre = 'a A.Next.t

    let compute (Nx n) = A.Next.compute n

    let make n = Nx n

    let bind = bind_next

    let map n f = bind n (fun x -> return (f x))

    type 'a t = 'a next
  end

  module Choice = struct
    type 'a pre = 'a A.Choice.t

    type 'a cases = (case * 'a Next.t) list

    let compute (Ch c) = A.Choice.compute c

    let make c =
      let c' = A.Choice.map c (fun choices -> List.map_snd Next.make choices) in
      Ch c'


    let bind = bind_choices

    let map c f = bind c (fun x -> return (f x))

    type 'a t = 'a choice
  end

  let map m f = bind m (fun x -> return (f x))

  let rec flaky_fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : ('a, 'c) result t)
    : ('b, 'c) result
    =
    let rec aux acc = function
      | [] -> Ok acc
      | (_, n) :: rest ->
        (match flaky_fold f acc (Next.compute n) with
         | Ok acc' -> aux acc' rest
         | Error e -> Error e)
    in
    match m with
    | End (Ok x) -> Ok (f acc x)
    | End (Error e) -> Error e
    | Vanish -> Ok acc
    | Breakpoint (_, n) -> flaky_fold f acc (Next.compute n)
    | Choice (_, cs) -> aux acc (Choice.compute cs)


  let rec fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : 'a t) : 'b =
    match m with
    | End x -> f acc x
    | Vanish -> acc
    | Breakpoint (_, n) -> fold f acc (Next.compute n)
    | Choice (_, c) ->
      let cs = Choice.compute c in
      List.fold_left (fun acc (_, n) -> fold f acc (Next.compute n)) acc cs
end

module Make_memoized (A : Args_memo) = struct
  module T = Make (struct
      type 'n breakpoint = 'n A.breakpoint

      type case = A.case

      type nest_result = A.nest_result

      let get_nested = A.get_nested

      module Next = Memo
      module Choice = Memo
    end)

  include T

  module Next = struct
    include T.Next

    type 'a pre = 'a A.Next.t

    let make' n = Memo.make A.Next.compute n

    let make n = Nx (make' n)

    let poll (Nx n) = Memo.poll n

    let to_memo (Nx n) = n

    let of_memo = T.Next.make
  end

  module Choice = struct
    include T.Choice

    type 'a pre = 'a A.Choice.t

    let make' c =
      let open A.Choice in
      Memo.make ~poll compute c


    let make c : 'a choice =
      let c' = A.Choice.map c (List.map_snd Next.make) in
      Ch (make' c')


    let poll (Ch c) = Memo.poll c

    let to_memo (Ch c) = c

    let of_memo c = Ch c
  end

  let breakpoint b n = breakpoint b (Next.make' n)

  let ( let* ) = Option.bind

  let rec poll_fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : 'a t) : 'b option =
    match m with
    | End x -> Some (f acc x)
    | Vanish -> Some acc
    | Breakpoint (_, n) ->
      let* t = Next.poll n in
      poll_fold f acc t
    | Choice (_, c) ->
      let* c = Choice.poll c in
      poll_fold_choices f acc c


  and poll_fold_choices f acc = function
    | [] -> Some acc
    | (_, n) :: rest ->
      let* t = Next.poll n in
      let* acc = poll_fold f acc t in
      poll_fold_choices f acc rest
end
