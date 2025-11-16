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


let make' (x : 'a) : 'a t = ref (Computed x)
