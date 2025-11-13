include Trace_intf

module Make (A : Args) = struct
  type case = A.case

  type 'a t = ('a, breakpoint, case, 'a next) trace

  and 'a next = Next of 'a t A.next

  and breakpoint = Bp of unit next A.breakpoint

  let next n = Next n

  let breakpoint b n = Breakpoint (Bp b, next n)

  let compute_next (Next n) = A.compute_next n

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with
    | End x -> f x
    | Vanish -> Vanish
    | Breakpoint (d, n) -> Breakpoint (d, bind_next n f)
    | Choice cs -> Choice (List.map_snd (fun n -> bind_next n f) cs)


  and bind_next (Next n : 'a next) (f : 'a -> 'b t) : 'b next =
    Next (A.map_next n (fun m -> bind m f))


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

module Memo = struct
  type 'a state =
    | Computed of 'a
    | Pending of
        { poll : unit -> 'a option;
          force : unit -> 'a
        }

  type 'a t = 'a state ref

  let poll (m : 'a t) : 'a option =
    match !m with
    | Pending { poll; _ } ->
      (match poll () with
       | Some x ->
         m := Computed x;
         Some x
       | None -> None)
    | Computed x -> Some x


  let force (m : 'a t) : 'a =
    match !m with
    | Pending { force; _ } ->
      let x = force () in
      m := Computed x;
      x
    | Computed x -> x


  let map (m : 'a t) (f : 'a -> 'b) : 'b t =
    let s =
      match poll m with
      | Some x -> Computed (f x)
      | None ->
        let poll () = Option.map f (poll m) in
        let force () = f (force m) in
        Pending { poll; force }
    in
    ref s


  let make (f : 'a -> 'b) (x : 'a) : 'b t =
    let r = ref None in
    let poll () = !r in
    let force () = match !r with None -> f x | Some y -> y in
    ref (Pending { poll; force })
end

module Make_memoized (A : Args) = struct
  module T = Make (struct
      type 'n breakpoint = 'n A.breakpoint

      type case = A.case

      type 'a next = 'a Memo.t

      let map_next = Memo.map

      let compute_next = Memo.force

      let get_nested = A.get_nested
    end)

  include T

  let next' n = Memo.make A.compute_next n

  let next n = T.next (next' n)

  let breakpoint b n = breakpoint b (next' n)

  let poll_next (Next n) = Memo.poll n
end
