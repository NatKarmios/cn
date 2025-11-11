include Trace_intf

module Make (A : Args) = struct
  include A

  type 'a next' = 'a A.next

  type 'a t = ('a, breakpoint, case, 'a next) trace

  and 'a next = Next of 'a t next'

  let next n = Next n

  let compute_next (Next n) = A.compute_next n

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    let map_next (Next n) = Next (A.map_next n (fun m' -> bind m' f)) in
    match m with
    | End x -> f x
    | Vanish -> Vanish
    | Breakpoint (d, n) -> Breakpoint (d, map_next n)
    | Choice cs -> Choice (List.map_snd map_next cs)


  let return x = End x

  let map m f = bind m (fun x -> return (f x))

  let rec flaky_fold'
            ?prev
            ~get_id
            ~append
            (f : 'b -> 'a -> 'b)
            (acc : 'b)
            (m : ('a, 'c) result t)
    : ('b, 'c) result
    =
    let id = get_id () in
    Option.iter (fun p -> append (Format.sprintf "%d -> %d" p id)) prev;
    let rec aux acc = function
      | [] -> Ok acc
      | (_, n) :: rest ->
        (match flaky_fold' ~prev:id ~get_id ~append f acc (compute_next n) with
         | Ok acc' -> aux acc' rest
         | Error e -> Error e)
    in
    match m with
    | End (Ok x) ->
      append (Format.sprintf "%d [label=\"%d (ok)\"]" id id);
      Ok (f acc x)
    | End (Error e) ->
      append (Format.sprintf "%d [label=\"%d (error)\"]" id id);
      Error e
    | Vanish ->
      append (Format.sprintf "%d [label=\"%d (vanish)\"]" id id);
      Ok acc
    | Breakpoint (_, n) -> flaky_fold' ~prev:id ~get_id ~append f acc (compute_next n)
    | Choice cs ->
      Format.printf "Folding choice!\n";
      aux acc cs


  let flaky_fold f acc m =
    let count = ref 0 in
    let get_id () =
      let id = !count in
      count := id + 1;
      id
    in
    let append s = Format.printf "%s\n" s in
    let r = flaky_fold' ~get_id ~append f acc m in
    r


  let rec fold (f : 'b -> 'a -> 'b) (acc : 'b) (m : 'a t) : 'b =
    match m with
    | End x -> f acc x
    | Vanish -> acc
    | Breakpoint (_, n) -> fold f acc (compute_next n)
    | Choice cs -> List.fold_left (fun acc (_, n) -> fold f acc (compute_next n)) acc cs
end

module Make_memoized (A : Args) = struct
  type 'a memo =
    | Pending of 'a A.next
    | Computed of 'a

  module T = Make (struct
      type breakpoint = A.breakpoint

      type case = A.case

      type 'a next = 'a memo ref

      let map_next (m : 'a next) (f : 'a -> 'b) : 'b next =
        match !m with
        | Pending n -> Pending (A.map_next n f) |> ref
        | Computed t -> Computed (f t) |> ref


      let compute_next (m : 'a next) : 'a =
        match !m with
        | Pending n ->
          let t = A.compute_next n in
          m := Computed t;
          t
        | Computed t -> t
    end)

  include T

  type 'a next' = 'a A.next

  let next n = T.next (ref (Pending n))
end
