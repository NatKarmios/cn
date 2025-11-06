type id = Int64.t

let get_id =
  let current = ref Int64.zero in
  fun () ->
    let c = !current in
    current := Int64.succ !current;
    c


type node =
  { log_entry : Explain.log_entry;
    nexts : Int64.t list ref
  }

type t =
  { roots : Int64.t list ref;
    nodes : (Int64.t, node) Hashtbl.t
  }

let make () = { roots = ref []; nodes = Hashtbl.create 0 }

let append r x = r := !r @ [ x ]

let insert ?prev log_entry trace =
  let id = get_id () in
  let node = { log_entry; nexts = ref [] } in
  let () = Hashtbl.replace trace.nodes id node in
  let () =
    match prev with
    | Some prev ->
      let prev_node = Hashtbl.find trace.nodes prev in
      append prev_node.nexts id
    | None -> append trace.roots id
  in
  id


let dump { roots; nodes } =
  let ( let- ) l f = List.iter f l in
  let rec aux depth id =
    let { log_entry; nexts; _ } = Hashtbl.find nodes id in
    let pad = String.make (depth * 2) ' ' in
    let kind =
      match log_entry with Explain.State _ -> "state" | Explain.Action _ -> "action"
    in
    let () = Format.printf "%s%Ld %s\n" pad id kind in
    let- next_id = !nexts in
    aux (depth + 1) next_id
  in
  let- root = !roots in
  Format.printf "\n";
  aux 0 root
