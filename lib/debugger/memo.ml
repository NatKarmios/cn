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


let make ?(poll : ('a -> 'b option) option) (f : 'a -> 'b) (x : 'a) : 'b t =
  let r = ref None in
  let poll () =
    let () =
      match (!r, poll) with
      | Some _, _ | None, None -> ()
      | None, Some poll -> r := poll x
    in
    !r
  in
  let force () = match !r with None -> f x | Some y -> y in
  ref (Pending { poll; force })


let make' (x : 'a) : 'a t = ref (Computed x)

let of_list (memos : 'a t list) : 'a list t =
  let poll () = List.map_opt poll memos in
  let force () = List.map force memos in
  make ~poll force ()


let make_multi ?(poll : ('a -> 'b option) option) (f : 'a -> 'b) (xs : 'a list)
  : 'b list t
  =
  let memos = List.map (make ?poll f) xs in
  of_list memos


let make_multi_i
      ?(poll : (int -> 'a -> 'b option) option)
      (f : int -> 'a -> 'b)
      (xs : 'a list)
  : 'b list t
  =
  let memos =
    List.mapi
      (fun i x ->
         let poll = poll |> Option.map (fun p -> p i) in
         make ?poll (f i) x)
      xs
  in
  of_list memos


let compute = force
