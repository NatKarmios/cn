type ('result, 'breakpoint, 'choice_case, 'next) trace =
  | End of 'result
  | Vanish
  | Breakpoint of 'breakpoint * 'next
  | Choice of ('choice_case * 'next) list

module type Args = sig
  type breakpoint

  type case

  type 'a next

  type 'a t := ('a, breakpoint, case, 'a next) trace

  type ('a, 'b) bind := 'a t -> ('a -> 'b t) -> 'b t

  val bind_next : bind:('a, 'b) bind -> 'a next -> ('a -> 'b t) -> 'b next

  val compute_next : 'a next -> 'a t
end

module Make (A : Args) = struct
  open A

  type breakpoint = A.breakpoint

  type case = A.case

  type 'a next = 'a A.next

  type 'a t = ('a, breakpoint, case, 'a next) trace

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with
    | End x -> f x
    | Vanish -> Vanish
    | Breakpoint (d, n) -> Breakpoint (d, bind_next ~bind n f)
    | Choice cs -> Choice (List.map_snd (fun n -> bind_next ~bind n f) cs)


  let return x = End x

  let map m f = bind m (fun x -> return (f x))

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
