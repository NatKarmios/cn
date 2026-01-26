open struct
  include Sedap_types
  include Map_node_options
  include Map_node_extra
  module Stop_reason = Stopped_event.Payload.Reason
end

let ( let* ) = Option.bind

let ( let+ ) o f = Option.map f o

let ( let/ ) x f = match f () with Some y -> y | None -> x

module Node = struct
  type computed_inner =
    { display : string;
      children : string list;
      next : (string * string) list;
      highlight : Highlight.t option;
      get_state : (unit -> Display_trace.state) option
    }

  type inner =
    | Root of
        { title : string;
          next : string
        }
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

  let is_pending { inner; _ } = match inner with Pending _ -> true | _ -> false

  let get_computed_inner { id; inner; _ } =
    match inner with
    | Computed c -> c
    | _ -> failwith ("Tried to get computed inner of " ^ id)


  let make ~id ~trace_id ?prev ?parent next =
    { id; trace_id; prev; parent; inner = Pending next }


  let render (node : t) : Sedap_types.Map_node.t =
    let submaps, next, options =
      match node.inner with
      | Root { title; next } ->
        let next = Map_node_next.Value.make ~label:None ~step_id:next () in
        let options =
          Map_node_options.Root
            { title; subtitle = None; zoomable = Some true; extras = None }
        in
        ([], [ next ], options)
      | Pending _ -> ([], [], Map_node_options.Pending)
      | Computed ({ display; highlight; _ } as c) ->
        let next =
          List.map
            (fun (l, step_id) -> Map_node_next.Value.make ~label:(Some l) ~step_id ())
            c.next
        in
        let options =
          Map_node_options.Basic
            { display; selectable = Some true; highlight; extras = None }
        in
        (c.children, next, options)
    in
    Sedap_types.Map_node.make ~step_id:node.id ~aliases:[] ~submaps ~next ~options ()
end

open Node
open Display_trace

type trace =
  { id : string;
    name : string;
    pending_nodes : string Hashset.t;
    mutable active_node : string
  }

type scopes = Sedap_types.Scope.t list

type vars = (int, Sedap_types.Variable.t list) Hashtbl.t

type t =
  { get_id : unit -> string;
    nodes : (string, Node.t) Hashtbl.t;
    changed_nodes : string Hashset.t;
    traces : (string, trace) Hashtbl.t;
    mutable active_trace : string option;
    mutable scopes_and_vars : (scopes * vars) option
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
    active_trace = None;
    scopes_and_vars = None
  }


let get_node t id =
  match Hashtbl.find_opt t.nodes id with
  | Some node -> node
  | None -> failwith ("Couldn't find node '" ^ id ^ "'")


let get_computed_node t id =
  let node = get_node t id in
  match node.inner with
  | Root _ -> failwith ("Expected node " ^ id ^ " to be computed, got root")
  | Pending _ -> failwith ("Expected node " ^ id ^ " to be computed, got pending")
  | Computed c -> (node, c)


let get_trace t trace_id =
  match Hashtbl.find_opt t.traces trace_id with
  | Some trace -> trace
  | None -> failwith ("Couldn't find trace '" ^ trace_id ^ "'")


let get_active_node t trace = get_node t trace.active_node

let get_active_trace t = Option.map (get_trace t) t.active_trace

(* let get_active_trace_exn t = *)
(*   match get_active_trace t with Some trace -> trace | None -> failwith "No active trace" *)

let get_active_computed_node t =
  let+ trace = get_active_trace t in
  let node, c = get_computed_node t trace.active_node in
  (trace, node, c)


let get_active_state t =
  let* trace, node, c = get_active_computed_node t in
  let+ f = c.get_state in
  ((trace, node, c), f ())


let get_active t =
  let* trace = get_active_trace t in
  let node = get_active_node t trace in
  Some (trace, node)


let new_node t ~trace ?prev ?parent next =
  let id = t.get_id () in
  let node = Node.make ~id ~trace_id:trace.id ?prev ?parent next in
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
    | Choice (b, c) ->
      let cs = Display_trace.T.Choice.compute c in
      let next = List.map_snd (fun n -> new_node t ~trace ~prev:id ?parent n) cs in
      inner_from_bp t ~trace ~prev:id ~parent b next
  in
  node.inner <- Computed inner;
  Hashset.remove trace.pending_nodes id;
  Hashset.add t.changed_nodes id;
  Node.get_nexts_and_nests inner


let poll_node t node =
  match node.Node.inner with
  | Root _ | Computed _ -> []
  | Pending n ->
    let dt = Display_trace.T.Next.poll n in
    Option.fold ~some:(apply_node t node) ~none:[] dt


let force_node t node =
  match node.Node.inner with
  | Root _ | Computed _ -> []
  | Pending n ->
    let dt = Display_trace.T.Next.compute n in
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


let trace_to_next dt = Display_trace.T.Next.of_memo (Memo.make' dt)

let jump ?(override_active_trace = true) t id =
  let node = get_node t id in
  (* Compute new nodes if necessary *)
  let new_ids = force_node t node in
  poll_nodes_deep t new_ids;
  let trace_id = node.trace_id in
  (* Set active *)
  let trace = get_trace t trace_id in
  trace.active_node <- id;
  if override_active_trace || Option.is_none t.active_trace then (
    t.active_trace <- Some trace_id;
    t.scopes_and_vars <- None)


let launch_trace t (title, dt) =
  let trace_id = t.get_id () in
  let trace =
    { id = trace_id; name = title; pending_nodes = Hashset.create 0; active_node = "" }
  in
  Hashtbl.replace t.traces trace_id trace;
  let next = trace_to_next dt in
  let first_node_id = new_node t ~trace next in
  let root_node =
    let inner = Root { title; next = first_node_id } in
    Node.{ id = trace_id; trace_id; prev = None; parent = None; inner }
  in
  Hashtbl.replace t.nodes trace_id root_node;
  Hashset.add t.changed_nodes trace_id;
  jump ~override_active_trace:false t first_node_id


let launch t traces = List.iter (launch_trace t) traces

(** Selects one of the given node IDs.
  If any of the given IDs point to a pending node, selects the first such ID.
  Otherwise, selects the first ID. *)
let select_id t ids =
  match ids with
  | [] -> None
  | [ id ] -> Some id
  | _ ->
    let pending_id = List.find_opt (fun id -> Node.is_pending (get_node t id)) ids in
    let id = match pending_id with Some id -> id | None -> List.hd ids in
    Some id


type step_over_result =
  | Stepped_to of string
  (* | Hit_breakpoint *)
  | Reached_end

let step_over' t id : step_over_result =
  let _, { next; children; _ } = get_computed_node t id in
  ignore children;
  (* TODO: handle children *)
  match next with
  | [] -> Reached_end
  | nexts ->
    let next_id = select_id t (List.map snd nexts) |> Option.get in
    jump t next_id;
    Stepped_to next_id


let step_over t =
  let/ () = Stop_reason.Step in
  let+ _, node = get_active t in
  let rec aux { id; parent; _ } =
    match step_over' t id with
    | Stepped_to _ -> Stop_reason.Step
    (* | Hit_breakpoint -> Stop_reason.Breakpoint *)
    | Reached_end ->
      (match parent with
       | Some parent_id -> aux (get_node t parent_id)
       | None -> Stop_reason.Step)
  in
  aux node


let step_in = step_over (*TODO*)

let step_out = step_over (*TODO*)

let step_back t =
  let/ () = Stop_reason.Step in
  let+ _, node = get_active t in
  match (node.prev, node.parent) with
  | None, None -> Stop_reason.Step
  | None, Some id | Some id, _ ->
    jump t id;
    Stop_reason.Step


let continue t =
  let rec aux acc = function
    | [] -> List.rev acc
    | [] :: idss -> aux acc idss
    | (id :: ids) :: idss ->
      Log.log_to_file ("Forcing " ^ id);
      let node = get_node t id in
      let _ = force_node t node in
      let acc, idss' =
        match List.map snd (get_computed_inner node).next with
        | [] -> (id :: acc, ids :: idss)
        | next -> (acc, next :: ids :: idss)
      in
      aux acc idss'
  in
  let/ () = Stop_reason.Step in
  Log.log_to_file "Continuing?";
  let* trace = get_active_trace t in
  Log.log_to_file "Yes";
  let id = trace.active_node in
  let+ end_id = match aux [] [ [ id ] ] with id :: _ -> Some id | [] -> None in
  jump t end_id;
  Stop_reason.Step


let continue_back = step_over (* TODO *)

let terminate _ = ()

let get_frames t =
  let/ () = [] in
  let+ _, { frames; _ } = get_active_state t in
  List.map
    (fun frame ->
       let source = Option.map (fun s -> Source.make ~path:(Some s) ()) frame.source in
       Stack_frame.make
         ~id:frame.index
         ~name:frame.name
         ~source
         ~line:frame.start_line
         ~column:frame.start_column
         ~end_line:frame.end_line
         ~end_column:frame.end_column
         ())
    frames


let compute_scopes_and_vars t =
  let/ () = ([], Hashtbl.create 0) in
  let+ _, { vars; _ } = get_active_state t in
  let variables = Hashtbl.create 0 in
  let add_var parent var =
    let vars = Option.value ~default:[] (Hashtbl.find_opt variables parent) in
    Hashtbl.replace variables parent (var :: vars)
  in
  let rec aux c = function
    | [] -> c
    | (_, []) :: varss -> aux c varss
    | (parent, var :: vars) :: varss ->
      let Variable.{ name; value; type_; children } = var in
      let variables_reference, c', varss' =
        match children with
        | [] -> (0, c, (parent, vars) :: varss)
        | _ -> (c, c + 1, (c, children) :: (parent, vars) :: varss)
      in
      let variable =
        Sedap_types.Variable.make ~name ~value ~type_ ~variables_reference ()
      in
      add_var parent variable;
      aux c' varss'
  in
  let _, scopes =
    List.fold_left
      (fun (c, acc) (name, vars) ->
         let s = Scope.make ~name ~variables_reference:c ~expensive:false () in
         Hashtbl.replace variables c [];
         let c' = aux (c + 1) [ (c, vars) ] in
         (c', s :: acc))
      (1, [])
      vars
  in
  (scopes, variables)


let get_scopes_and_vars t =
  match t.scopes_and_vars with
  | Some sv -> sv
  | None ->
    let sv = compute_scopes_and_vars t in
    t.scopes_and_vars <- Some sv;
    sv


let get_scopes t = fst (get_scopes_and_vars t)

let get_variables t var_ref =
  let _, vars = get_scopes_and_vars t in
  Hashtbl.find vars var_ref


let set_breakpoints t source lines = ignore (t, source, lines) (* TODO *)

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
       let root = Map_root.{ map_id = id; name } in
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


let jump t id = jump t id
