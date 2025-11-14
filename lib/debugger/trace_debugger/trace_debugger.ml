open struct
  include Sedap_types
  include Map_node_options
  include Map_node_extra
end

module Node = struct
  type computed_inner =
    { display : string;
      children : string list;
      next : (string * string) list;
      highlight : Highlight.t option;
      get_state : (unit -> Display_trace.state) option
    }

  type inner =
    | Pending of Display_trace.next
    | Computed of computed_inner

  let make_inner ?(children = []) ?(next = []) ?highlight ?get_state display =
    { display; children; next; highlight; get_state }


  let get_nexts_and_nests { next; children; _ } = List.map snd next @ children

  type t =
    { id : string;
      trace_id : string;
      prev : string option;
      parent : string option;
      mutable inner : inner
    }

  let make ~id ~trace_id ?prev ?parent next =
    { id; trace_id; prev; parent; inner = Pending next }


  let render node : Sedap_types.Map_node.t = failwith "TODO"
end

type trace =
  { id : string;
    name : string;
    mutable pending_nodes : string Hashset.t;
    mutable active_node : string;
    mutable root_sent : bool
  }

type t =
  { get_id : unit -> string;
    nodes : (string, Node.t) Hashtbl.t;
    changed_nodes : string Hashset.t;
    mutable traces : (string, trace) Hashtbl.t;
    mutable active_trace : string option
  }

let make () =
  let count = ref Int64.zero in
  let get_id () =
    let id = !count in
    count := Int64.succ id;
    Int64.to_string id
  in
  { get_id;
    nodes = Hashtbl.create 0;
    changed_nodes = Hashset.create 0;
    traces = Hashtbl.create 0;
    active_trace = None
  }


let get_node t id =
  match Hashtbl.find_opt t.nodes id with
  | Some node -> node
  | None -> failwith ("Couldn't find node '" ^ id ^ "'")


let get_trace t trace_id =
  match Hashtbl.find_opt t.traces trace_id with
  | Some trace -> trace
  | None -> failwith ("Couldn't find trace '" ^ trace_id ^ "'")


let get_active_trace t = Option.map (get_trace t) t.active_trace

let get_active_trace_exn t =
  match get_active_trace t with Some trace -> trace | None -> failwith "No active trace"


let get_trace_of_node t id = get_trace t (get_node t id).trace_id

let new_node t ~trace ~prev ?parent next =
  let id = t.get_id () in
  let node = Node.make ~id ~trace_id:trace.id ~prev ?parent next in
  Hashtbl.add t.nodes id node;
  Hashset.add t.changed_nodes id;
  Hashset.add trace.pending_nodes id;
  id


let inner_from_bp
      t
      ~trace
      ~prev
      ~parent
      (Bp { msg; nest; get_state } : Display_trace.T.breakpoint)
      next
  : Node.computed_inner
  =
  let children = List.map (fun n -> new_node t ~trace ~prev ?parent n) nest in
  Node.make_inner ~children ~next ~get_state msg


let apply_node t (node : Node.t) (dt : Display_trace.t) =
  let open Trace in
  let open Node in
  let { id; trace_id; parent; _ } = node in
  let trace = get_trace t trace_id in
  let inner =
    match dt with
    | End (Ok s) -> make_inner ~highlight:Highlight.Success s
    | End (Error s) -> make_inner ~highlight:Highlight.Error s
    | Vanish -> make_inner ~highlight:Highlight.Info "Vanish"
    | Breakpoint (b, n) ->
      let next_id = new_node t ~trace ~prev:id ?parent n in
      let next = [ ("", next_id) ] in
      inner_from_bp t ~trace ~prev:id ~parent b next
    | Choice (b, cs) ->
      let next =
        List.map
          (fun (i, n) ->
             let next_id = new_node t ~trace ~prev:id ?parent n in
             (Int.to_string i, next_id))
          cs
      in
      inner_from_bp t ~trace ~prev:id ~parent b next
  in
  node.inner <- Computed inner;
  Hashset.remove trace.pending_nodes id;
  Hashset.add t.changed_nodes id;
  Node.get_nexts_and_nests inner


let poll_node t node =
  match node.Node.inner with
  | Computed _ -> []
  | Pending n ->
    let dt = Display_trace.T.poll_next n in
    Option.fold ~some:(apply_node t node) ~none:[] dt


let force_node t node =
  match node.Node.inner with
  | Computed _ -> []
  | Pending n ->
    let dt = Display_trace.T.compute_next n in
    apply_node t node dt


let poll_nodes_deep t ids =
  let rec aux = function
    | [] -> ()
    | [] :: idss -> aux idss
    | (id :: ids) :: idss ->
      let node = get_node t id in
      aux @@ (poll_node t node :: ids :: idss)
  in
  aux [ ids ]


let poll_pending_nodes t =
  let pending_ids =
    Hashtbl.fold
      (fun _ trace acc -> Hashset.fold (fun id acc -> id :: acc) trace.pending_nodes acc)
      t.traces
      []
  in
  poll_nodes_deep t pending_ids


let launch_trace t trace = failwith "TODO"

let launch t traces =
  List.iter (launch_trace t) traces;
  failwith "TODO"


let jump t id =
  let node = get_node t id in
  (* Compute new nodes if necessary *)
  let new_ids = force_node t node in
  poll_nodes_deep t new_ids;
  let trace_id = node.trace_id in
  (* Set active *)
  t.active_trace <- Some trace_id;
  let trace = get_trace t trace_id in
  trace.active_node <- id


let step_over t = failwith "TODO"

let step_in = step_over (*TODO*)

let step_out = step_over (*TODO*)

let step_back = failwith "TODO"

let continue = failwith "TODO"

let continue_back = failwith "TODO"

let step_specific = failwith "TODO"

let terminate _ = ()

let get_frames = failwith "TODO"

let get_scopes = failwith "TODO"

let get_variables = failwith "TODO"

let set_breakpoints = failwith "TODO"

let get_changed_nodes ?(clear = false) t =
  let nodes =
    Hashset.fold
      (fun id acc ->
         let map_node = Some (Node.render (get_node t id)) in
         String_map.add id map_node acc)
      t.changed_nodes
      String_map.empty
  in
  if clear then Hashset.clear t.changed_nodes;
  nodes


let get_all_nodes t =
  Hashtbl.fold
    (fun id node acc ->
       let map_node = Some (Node.render node) in
       String_map.add id map_node acc)
    t.nodes
    String_map.empty


let get_roots t : Map_root.t list =
  Hashtbl.fold
    (fun _ { name; id; _ } acc ->
       let root = Map_root.{ name; id } in
       root :: acc)
    t.traces
    []


let get_current_steps t : Map_update_event_body.Current_steps.t option =
  let p, s =
    match get_active_trace t with
    | Some trace ->
      let node = get_node t trace.active_node in
      let rec aux = function
        | None -> []
        | Some parent_id ->
          let Node.{ parent; id; _ } = get_node t parent_id in
          id :: aux parent
      in
      let s = aux node.parent in
      ([ node.id ], s)
    | None -> ([], [])
  in
  Some { primary = Some p; secondary = Some s }


let get_map_update t =
  poll_pending_nodes t;
  let nodes = get_changed_nodes t in
  let roots = get_roots t in
  let current_steps = get_current_steps t in
  let ext = None in
  Map_update_event_body.make ~nodes ~roots ~current_steps ~ext ()


let get_full_map t =
  let nodes = get_all_nodes t in
  let roots = get_roots t in
  let current_steps = get_current_steps t in
  let ext = None in
  Map_update_event_body.make ~reset:true ~nodes ~roots ~current_steps ~ext ()
