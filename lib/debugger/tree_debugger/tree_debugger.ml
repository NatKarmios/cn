open struct
  include Sedap_types
  include Map_node_options
  include Map_node_extra
  module Stop_reason = Stopped_event.Payload.Reason
  module StringSet = Set.Make (String)

  let show_vanish = true
end

let ( let> ) o f = o f

let ( let* ) = Option.bind

let ( let+ ) o f = Option.map f o

let ( let/ ) x f = match f () with Some y -> y | None -> x

module Node = struct
  type status =
    | Pending of Display_tree.next
    | Pending_children of StringSet.t
    | Completed

  type root_node =
    { title : string;
      next : string
    }
  [@@deriving to_yojson]

  module Next = struct
    type case =
      | Static of float
      | Dynamic of float list
    [@@deriving ord, to_yojson]

    type t =
      { label : string;
        next_node_id : string;
        case : case;
        ix : float;
        stepped_out : bool
      }
    [@@deriving to_yojson]

    let make ?(label = "") ~case ?(ix = 0.0) ~stepped_out node_id =
      { label; next_node_id = node_id; case; ix; stepped_out }


    let compare a b = compare_case a.case b.case

    let rec insert ?prev_ix new_next = function
      | [] ->
        let ix = Option.fold ~none:0.0 ~some:(( +. ) 1.0) prev_ix in
        ([ { new_next with ix } ], ix)
      | next :: rest ->
        if compare new_next next < 0 then (
          let ix =
            match prev_ix with
            | Some prev_ix -> (prev_ix +. next.ix) /. 2.0
            | None -> next.ix -. 1.0
          in
          ({ new_next with ix } :: next :: rest, ix))
        else (
          let rest', ix = insert ~prev_ix:next.ix new_next rest in
          (next :: rest', ix))
  end

  type step_node =
    { branch_path : float list;
      mutable status : status; [@to_yojson fun _ -> `Null]
      mutable nexts : Next.t list;
      mutable children : string list;
      display : string;
      get_state : (unit -> Display_tree.state) option; [@to_yojson fun _ -> `Null]
      highlight : Highlight.t option
    }
  [@@deriving to_yojson]

  let insert_next ?label ~case ~stepped_out node_id step =
    let next = Next.make ?label ~case ~stepped_out node_id in
    let nexts, ix = Next.insert next step.nexts in
    step.nexts <- nexts;
    ix


  type kind =
    | Root of root_node
    | Step of step_node
  [@@deriving to_yojson]

  type t =
    { node_id : string;
      tree_id : string;
      prev : string option;
      parent : string option;
      kind : kind
    }
  [@@deriving to_yojson]

  let mk_step
        ~branch_path
        ?(nexts = [])
        ?(children = [])
        ~display
        ?get_state
        ?highlight
        ?n
        ()
    =
    let status = match n with Some n -> Pending n | None -> Completed in
    { branch_path; status; nexts; children; display; get_state; highlight }


  let render ({ node_id; kind; _ } : t) : Sedap_types.Map_node.t =
    let submaps, next, options =
      match kind with
      | Root { title; next } ->
        let next = Map_node_next.Value.make ~label:None ~step_id:next () in
        let options =
          Map_node_options.Root
            { title; subtitle = None; zoomable = Some true; extras = None }
        in
        ([], [ next ], options)
      | Step { display; highlight; nexts; children; status; get_state; _ } ->
        let next =
          List.filter_map
            (fun Next.{ next_node_id; stepped_out; label; _ } ->
               if stepped_out then
                 None
               else (
                 let next =
                   Map_node_next.Value.make ~label:(Some label) ~step_id:next_node_id ()
                 in
                 Some next))
            nexts
        in
        let controls =
          let open Controls in
          let step =
            match status with
            | Pending _ -> [ Step_in; Step_over ]
            | Pending_children _ -> [ Step_over ]
            | Completed -> []
          in
          let jump = match get_state with Some _ -> [ Jump ] | None -> [] in
          Some (step @ jump)
        in
        let options =
          Map_node_options.Basic { display; controls; highlight; extras = None }
        in
        (children, next, options)
    in
    Sedap_types.Map_node.make ~step_id:node_id ~aliases:[] ~submaps ~next ~options ()
end

open Node
open Node.Next

type tree =
  { tree_id : string;
    name : string;
    mutable pending_nodes : StringSet.t;
    mutable active_node : string
  }

type scopes = Sedap_types.Scope.t list

type vars = (int, Sedap_types.Variable.t list) Hashtbl.t

type t =
  { fresh_id : unit -> string;
    nodes : (string, Node.t) Hashtbl.t;
    changed_nodes : string Hashset.t;
    trees : (string, tree) Hashtbl.t;
    mutable active_tree : string option;
    mutable scopes_and_vars : (scopes * vars) option
  }

let make () =
  let count = ref Int64.zero in
  let fresh_id () =
    let id = !count in
    count := Int64.succ id;
    Int64.to_string id
  in
  { fresh_id;
    nodes = Hashtbl.create 0;
    changed_nodes = Hashset.create 0;
    trees = Hashtbl.create 0;
    active_tree = None;
    scopes_and_vars = None
  }


let get_step_node node_id t =
  let node = Hashtbl.find t.nodes node_id in
  match node.kind with
  | Root _ -> failwith "Expected Step node, got Root"
  | Step step -> (node, step)


module Tree_processing = struct
  type computed_next =
    { node_id : string;
      ix : int;
      case : Node.Next.case;
      n : Display_tree.next option;
      step_dir : int;
      label : string;
      display : string;
      get_state : (unit -> Display_tree.state) option;
      highlight : Highlight.t option
    }

  let mk_computed_next ?n ~step_dir ?get_state ?highlight ~labels ~t msg ix =
    let label =
      labels
      |> List.rev
      |> List.map String.trim
      |> List.filter (function "" -> false | _ -> true)
      |> String.concat " / "
    in
    { node_id = t.fresh_id ();
      ix;
      case = Static (Float.of_int ix);
      n;
      step_dir;
      label;
      display = msg;
      get_state;
      highlight
    }


  type processed_tree =
    | Step_again of (int * float list * string list * Display_tree.next) list
    | Stop of (int -> computed_next) list

  let process_tree ~step_dir ~branch_path ~labels tree t =
    let open Tree in
    let open Display_tree.T in
    let mk_next = mk_computed_next ~step_dir ~t ~labels in
    match (tree, step_dir) with
    | Breakpoint (Bp Step_in, n), 0 -> Step_again [ (-1, branch_path, labels, n) ]
    | Breakpoint (Bp Step_out, n), x when x >= 0 ->
      Step_again [ (x + 1, branch_path, labels, n) ]
    | Breakpoint (Bp (Step_in | Step_out), _), _ -> failwith "Malformed tree"
    | Breakpoint (Bp (Nest _), _), _ -> failwith "TODO: nest"
    | Vanish, _ -> if show_vanish then Stop [ mk_next "Vanish" ] else Stop []
    | End ({ ok; msg; get_state } : Display_tree.t'), _ ->
      let highlight = Highlight.(if ok then Success else Error) in
      Stop [ mk_next ~highlight ~get_state msg ]
    | Breakpoint (Bp (Step { msg; get_state }), n), _ ->
      Stop [ mk_next ~n ~get_state msg ]
    | Choice c, _ ->
      let c =
        List.sort (fun ((_, case), _) ((_, case'), _) -> Int.compare case case') c
      in
      let steps =
        List.map
          (fun ((label, case), n) ->
             let branch_path' = Float.of_int case :: branch_path in
             let labels' = label :: labels in
             (step_dir, branch_path', labels', n))
          c
      in
      Step_again steps


  let compute_nexts n t : computed_next list =
    let rec aux (step_dir, branch_path, labels, n) =
      let tree = Display_tree.T.compute_next n in
      match process_tree ~step_dir ~branch_path ~labels tree t with
      | Stop nexts -> nexts
      | Step_again steps -> List.concat_map aux steps
    in
    aux (0, [], [], n) |> List.mapi (fun i mk_next -> mk_next i)


  (* let flaky_map = *)
  (*   let rec aux acc f = function *)
  (*     | [] -> Some (List.rev acc) *)
  (*     | x :: xs -> (match f x with Some y -> aux (y :: acc) f xs | None -> None) *)
  (*   in *)
  (*   aux [] *)
  (**)
  (* let poll_nexts n t : computed_next list option = *)
  (*   let rec aux (step_dir, branch_path, labels, n) = *)
  (*     Option.bind (Display_tree.T.poll_next n) (fun tree -> *)
  (*       match process_tree ~step_dir ~branch_path ~labels tree t with *)
  (*       | Stop nexts -> Some nexts *)
  (*       | Step_again steps -> flaky_map aux steps |> Option.map List.concat) *)
  (*   in *)
  (*   aux (0, [], [], n) |> Option.map (List.mapi (fun i mk_next -> mk_next i)) *)

  let new_step
        ~node_id
        ~display
        ?get_state
        ?n
        ~branch_path
        ?highlight
        ~tree_id
        ?prev
        ?parent
        t
    =
    let step = mk_step ~display ?get_state ~branch_path ?highlight ?n () in
    let node = Node.{ node_id; tree_id; prev; parent; kind = Step step } in
    let () =
      match parent with
      | None -> ()
      | Some parent ->
        let _, parent_step = get_step_node parent t in
        (match (parent_step.status, step.status) with
         | _, Completed -> ()
         | Pending_children children, _ ->
           let children' = StringSet.add node_id children in
           parent_step.status <- Pending_children children'
         | _ -> ())
    in
    Hashtbl.replace t.nodes node_id node;
    Hashset.add t.changed_nodes node_id;
    ()


  let insert_children parent_node (parent_step : Node.step_node) nexts t =
    let Node.{ node_id = parent_id; tree_id; _ } = parent_node in
    let children =
      List.filter_map
        (fun ({ node_id; display; get_state; ix; n; step_dir; highlight; _ } :
               computed_next) ->
           match step_dir with
           | -1 ->
             let is_pending = Option.is_some n in
             new_step
               ~node_id
               ~display
               ?get_state
               ?n
               ~branch_path:[ Float.of_int ix ]
               ~tree_id
               ~parent:parent_id
               ?highlight
               t;
             Some (node_id, is_pending)
           | _ -> None)
        nexts
    in
    let all_children = List.map fst children in
    let pending_children =
      List.filter_map (fun (a, b) -> if b then Some a else None) children
    in
    parent_step.children <- parent_step.children @ all_children;
    if not (List.is_empty all_children) then
      Hashset.add t.changed_nodes parent_id;
    Hashset.add t.changed_nodes parent_node.node_id;
    StringSet.of_list pending_children


  let rec insert_nexts' x { node_id = prev; tree_id; parent; _ } prev_step nexts t =
    let inserted = ref false in
    let nexts' =
      List.filter_map
        (fun ({ step_dir; node_id; n; case; display; get_state; label; highlight; _ } as
              next) ->
           if step_dir < x then
             None
           else (
             inserted := true;
             let current_level = step_dir = x in
             let ix =
               Node.insert_next
                 ~label
                 ~case
                 ~stepped_out:(not current_level)
                 node_id
                 prev_step
             in
             let branch_path = ix :: prev_step.branch_path in
             if current_level then (
               new_step
                 ~node_id
                 ?n
                 ~branch_path
                 ~tree_id
                 ~prev
                 ?parent
                 ~display
                 ?get_state
                 ?highlight
                 t;
               None)
             else (
               let case' = Dynamic (List.rev branch_path) in
               Some { next with case = case' })))
        nexts
    in
    if !inserted then
      Hashset.add t.changed_nodes prev;
    if not (List.is_empty nexts') then (
      let parent_node, parent_step = get_step_node (Option.get parent) t in
      insert_nexts' (x + 1) parent_node parent_step nexts' t)


  let insert_nexts = insert_nexts' 0

  let rec mark_completed (node : Node.t) (step : Node.step_node) t =
    step.status <- Completed;
    match node.parent with
    | None ->
      let tree = Hashtbl.find t.trees node.tree_id in
      tree.pending_nodes <- StringSet.remove node.node_id tree.pending_nodes
    | Some parent ->
      let parent_node, parent_step = get_step_node parent t in
      (match parent_step.status with
       | Pending_children children ->
         let children' = StringSet.remove node.node_id children in
         if StringSet.is_empty children' then
           mark_completed parent_node parent_step t
         else
           parent_step.status <- Pending_children children'
       | _ -> failwith "Malformed tree")


  let apply_nexts node step nexts t =
    let pending_children = insert_children node step nexts t in
    insert_nexts node step nexts t;
    if StringSet.is_empty pending_children then (
      let () = mark_completed node step t in
      (List.hd nexts).node_id)
    else (
      let () = step.status <- Pending_children pending_children in
      List.hd step.children)


  let step_in_at' node_id t =
    let node, step = get_step_node node_id t in
    match step.status with
    | Completed | Pending_children _ ->
      (match (step.children, step.nexts) with
       | child_id :: _, _ -> Some child_id
       | [], { next_node_id; _ } :: _ -> Some next_node_id
       | [], [] -> None)
    | Pending n ->
      let nexts = compute_nexts n t in
      let next_id = apply_nexts node step nexts t in
      Some next_id


  let rec step_over_at' ?(depth = 0) node_id t =
    let _, step = get_step_node node_id t in
    let indent = String.make (depth * 2) ' ' in
    let rec aux () =
      match step.status with
      | Completed ->
        Log.log_to_file (Fmt.str "%sDone." indent);
        (match step.nexts with
         | { next_node_id; _ } :: _ -> Some next_node_id
         | [] -> None)
      | Pending _ ->
        Log.log_to_file (Fmt.str "%sPending; stepping in" indent);
        let _ = step_in_at' node_id t in
        aux ()
      | Pending_children children ->
        let child_id = StringSet.choose children in
        Log.log_to_file (Fmt.str "%s%d children" indent (StringSet.cardinal children));
        let _ = step_over_at' ~depth:(depth + 1) child_id t in
        aux ()
    in
    aux ()
end

include Tree_processing

module Breakpoints = struct
  let set_breakpoints _t _source _lines = () (* TODO *)
end

include Breakpoints

module Steps = struct
  let try_active_tree t f =
    match t.active_tree with
    | None -> Stop_reason.Step
    | Some tree_id ->
      let tree = Hashtbl.find t.trees tree_id in
      f tree


  let jump t node_id =
    let node = Hashtbl.find t.nodes node_id in
    let tree = Hashtbl.find t.trees node.tree_id in
    tree.active_node <- node_id;
    t.active_tree <- Some node.tree_id;
    t.scopes_and_vars <- None


  let maybe_jump t = function Some node_id -> jump t node_id | None -> ()

  let step_in_at t id =
    step_in_at' id t |> maybe_jump t;
    Stop_reason.Step


  let step_over_at t id =
    step_over_at' id t |> maybe_jump t;
    Stop_reason.Step


  let step_in t =
    let> tree = try_active_tree t in
    step_in_at t tree.active_node


  let step_over t =
    let> tree = try_active_tree t in
    step_over_at t tree.active_node


  let continue'' t node_id step_out =
    let next_id = ref None in
    let rec aux = function
      | [] -> ()
      | node_id :: rest ->
        let _ = step_over_at' node_id t in
        let _, step = get_step_node node_id t in
        let nexts =
          List.filter_map
            (fun { next_node_id; stepped_out; _ } ->
               if step_out && stepped_out then (
                 let () =
                   if Option.is_none !next_id then
                     next_id := Some next_node_id
                 in
                 None)
               else
                 Some next_node_id)
            step.nexts
        in
        let () =
          match (nexts, !next_id) with [], None -> next_id := Some node_id | _ -> ()
        in
        aux (nexts @ rest)
    in
    aux [ node_id ];
    !next_id


  let continue' t step_out =
    let> tree = try_active_tree t in
    continue'' t tree.active_node step_out |> maybe_jump t;
    Stop_reason.Step


  let step_out t = continue' t true

  let continue t = continue' t false

  let step_back' t node_id =
    let node = Hashtbl.find t.nodes node_id in
    node.prev


  let step_back t =
    let> tree = try_active_tree t in
    step_back' t tree.active_node |> maybe_jump t;
    Stop_reason.Step


  let continue_back t =
    let> tree = try_active_tree t in
    let rec aux acc node_id =
      match step_back' t node_id with
      | Some next_id -> aux (Some next_id) next_id
      | None -> acc
    in
    aux None tree.active_node |> maybe_jump t;
    Stop_reason.Step
end

include Steps

module Inspect = struct
  let get_active_tree t =
    let+ tree_id = t.active_tree in
    Hashtbl.find t.trees tree_id


  let get_active_state t =
    let* tree = get_active_tree t in
    let node = Hashtbl.find t.nodes tree.active_node in
    let* step = match node.kind with Step s -> Some s | _ -> None in
    let* get_state = step.get_state in
    Some ((node, step), get_state ())


  let get_frames t =
    let/ () = [] in
    let+ _, { frames; _ } = get_active_state t in
    List.map
      (fun (frame : Display_tree.stack_frame) ->
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
    let add_var parent (var : Variable.t) =
      let vars = Option.value ~default:[] (Hashtbl.find_opt variables parent) in
      Hashtbl.replace variables parent (var :: vars)
    in
    let rec aux c = function
      | [] -> c
      | (_, []) :: varss -> aux c varss
      | (parent, var :: vars) :: varss ->
        let Display_tree.Variable.{ name; value; type_; children } = var in
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


  let get_all_nodes t =
    Hashtbl.fold
      (fun node_id node acc ->
         let map_node = Some (Node.render node) in
         String_map.add node_id map_node acc)
      t.nodes
      String_map.empty


  let get_changed_nodes ?(clear = false) t =
    let nodes =
      Hashset.fold
        (fun node_id acc ->
           let node = Hashtbl.find t.nodes node_id in
           let map_node = Some (Node.render node) in
           String_map.add node_id map_node acc)
        t.changed_nodes
        String_map.empty
    in
    if clear then Hashset.clear t.changed_nodes;
    nodes


  let get_roots t : Map_root.t list =
    Hashtbl.fold
      (fun _ { name; tree_id; _ } acc ->
         let root = Map_root.{ map_id = tree_id; name } in
         root :: acc)
      t.trees
      []


  let get_current_steps t : Map_update_event_body.Current_steps.t option =
    let p, s =
      match get_active_tree t with
      | Some tree ->
        let node = Hashtbl.find t.nodes tree.active_node in
        let rec aux = function
          | None -> []
          | Some parent_id ->
            let Node.{ parent; node_id; _ } = Hashtbl.find t.nodes parent_id in
            node_id :: aux parent
        in
        let s = aux node.parent in
        ([ node.node_id ], s)
      | None -> ([], [])
    in
    Some { primary = Some p; secondary = Some s }


  let get_map ~full t =
    let nodes =
      if full then
        get_all_nodes t
      else
        get_changed_nodes t
    in
    let roots = get_roots t in
    let current_steps = get_current_steps t in
    let ext = None in
    Map_update_event_body.make ~nodes ~roots ~current_steps ~ext ~reset:full ()


  let get_map_update t = get_map ~full:false t

  let get_full_map t = get_map ~full:true t
end

include Inspect

module Lifecycle = struct
  let launch_tree t (title, dt) =
    let tree_id = t.fresh_id () in
    let tree =
      { tree_id; name = title; pending_nodes = StringSet.empty; active_node = "" }
    in
    Hashtbl.replace t.trees tree_id tree;
    let first_node_id = t.fresh_id () in
    let root_node =
      let kind = Root { title; next = first_node_id } in
      Node.{ node_id = tree_id; tree_id; prev = None; parent = None; kind }
    in
    Hashtbl.replace t.nodes tree_id root_node;
    Hashset.add t.changed_nodes tree_id;
    match dt with
    | Tree.Breakpoint (Display_tree.T.Bp (Step { msg = display; get_state }), n) ->
      Tree_processing.new_step
        ~node_id:first_node_id
        ~n
        ~branch_path:[]
        ~tree_id
        ~display
        ~get_state
        t
    | _ -> failwith "Expected tree to start with Step breakpoint!"


  let launch t trees = List.iter (launch_tree t) trees

  let terminate _ = ()
end

include Lifecycle
