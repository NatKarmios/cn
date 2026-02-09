include Tree_intf

module Make (A : Args) = struct
  type case = A.case

  type nest_result = A.nest_result

  type 'a t = ('a, breakpoint, case, 'a next) tree

  and 'a next = Nx of 'a t A.next

  and breakpoint = Bp of nest_result next A.breakpoint

  let next n = Nx n

  let breakpoint b n = Breakpoint (Bp b, next n)

  let compute_next (Nx n) = A.compute_next n

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with
    | End x -> f x
    | Vanish -> Vanish
    | Breakpoint (b, n) -> Breakpoint (b, bind_next n f)
    | Choice cs -> Choice (List.map_snd (fun n -> bind_next n f) cs)


  and bind_next (Nx n : 'a next) (f : 'a -> 'b t) : 'b next =
    Nx (A.map_next n (fun m -> bind m f))


  let return x = End x

  let map m f = bind m (fun x -> return (f x))

  let map_next n f = bind_next n (fun x -> return (f x))

  let rec flaky_fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : ('a, 'c) result t)
    : ('b, 'c) result
    =
    let rec aux acc = function
      | [] -> Ok acc
      | (_, n) :: rest ->
        (match flaky_fold f acc (compute_next n) with
         | Ok acc' -> aux acc' rest
         | Error e -> Error e)
    in
    match m with
    | End (Ok x) -> Ok (f acc x)
    | End (Error e) -> Error e
    | Vanish -> Ok acc
    | Breakpoint (_, n) -> flaky_fold f acc (compute_next n)
    | Choice cs -> aux acc cs


  let rec fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : 'a t) : 'b =
    match m with
    | End x -> f acc x
    | Vanish -> acc
    | Breakpoint (_, n) -> fold f acc (compute_next n)
    | Choice cs -> List.fold_left (fun acc (_, n) -> fold f acc (compute_next n)) acc cs
end

module Make_memoized (A : Args) = struct
  module T = Make (struct
      type 'n breakpoint = 'n A.breakpoint

      type case = A.case

      type 'a next = 'a Memo.t

      type nest_result = A.nest_result

      let map_next = Memo.map

      let compute_next = Memo.force
    end)

  include T

  let next' n = Memo.make A.compute_next n

  let next n = T.next (next' n)

  let breakpoint b n = breakpoint b (next' n)

  let poll_next (Nx n) = Memo.poll n

  let next_to_memo (Nx n) = n

  let next_of_memo = T.next
end
