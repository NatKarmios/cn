open Typing.Tree
open Pp.Infix

open struct
  module DT = Debugger.Display_tree
  module Res = Resource
  module LC = LogicalConstraints
  module IT = IndexTerms
  module Memo = Debugger.Memo
  module CI = Cerb_backend.Cabs_index
  module Cabs = Cerb_frontend.Cabs

  let loc_of_where (w : Where.t) =
    let ( let/ ) o f = match o with Some _ -> o | _ -> f () in
    let/ () = w.expression in
    let/ () = w.statement in
    let/ () = match w.section with Some (Label { loc; _ }) -> Some loc | _ -> None in
    let open Cerb_frontend.Symbol in
    match w.fnction with
    | Some (Symbol (_, _, sd)) ->
      (match sd with SD_unnamed_tag loc | SD_FunArg (loc, _) -> Some loc | _ -> None)
    | None -> None


  let show_case : case -> string = function
    | Branch_Eif true -> "true"
    | Branch_Eif false -> "false"


  let make_variable ?(name = "") ?type_ ?(children = []) ?(value = "") () =
    DT.Variable.{ name; value; type_; children }


  let display_sym_mapping ((sym, (bv, _)) : Sym.t * (Context.basetype_or_value * 'a))
    : DT.Variable.t
    =
    let name = Pp.plain (Sym.pp sym) in
    let value, type_ =
      match bv with
      | BaseType b ->
        let t = Pp.plain (BaseTypes.pp b) in
        let v = Fmt.str "? (%s)" t in
        (v, t)
      | Value v ->
        let t = Pp.plain (BaseTypes.pp (IT.get_bt v)) in
        let v = Pp.plain (IT.pp v) in
        (v, t)
    in
    make_variable ~name ~value ~type_ ()


  let display_sym_map (map : (Context.basetype_or_value * 'a) Sym.Map.t)
    : DT.Variable.t list
    =
    map |> Sym.Map.to_seq |> Seq.map display_sym_mapping |> List.of_seq


  let display_resource ((req, Res.O output) : Res.t) : DT.Variable.t =
    let name = Pp.plain (IT.pp output) in
    let value = Pp.plain (Request.pp req) in
    let children =
      match req with
      | Request.Q q ->
        let iargs = q.Request.QPredicate.iargs in
        List.map (fun it -> make_variable ~value:(Pp.plain (IT.pp it)) ()) iargs
      | _ -> []
    in
    make_variable ~name ~value ~children ()


  let display_constraint (lc : LC.t) : DT.Variable.t option =
    if not (LogicalConstraints.is_interesting lc) then
      None
    else
      Option.some
      @@
      let name, value =
        match lc with
        | LC.T it -> ("", Pp.plain (IT.pp it))
        | LC.Forall ((s, bt), it) ->
          ( Pp.plain (Pp.c_app !^"forall" [ Sym.pp s; BaseTypes.pp bt ]),
            Pp.plain (IT.pp it) )
      in
      make_variable ~name ~value ()


  let display_constraints (lcs : LC.Set.t) : DT.Variable.t list =
    lcs |> LC.Set.to_seq |> Seq.filter_map display_constraint |> List.of_seq


  let make_stack_frame
        (loc : Cerb_location.t option)
        (w : Where.t)
        (backup_loc : Cerb_location.t option)
    : DT.stack_frame option
    =
    let ( let* ) = Option.bind in
    let* loc = List.find_map (fun x -> x) [ loc; loc_of_where w; backup_loc ] in
    let* start_pos = Locations.start_pos loc in
    let start_line = Cerb_position.line start_pos in
    let start_column = Cerb_position.column start_pos in
    let source = Cerb_position.file start_pos in
    let source = Some source in
    let end_pos = Locations.end_pos' loc in
    let end_line = Option.map Cerb_position.line end_pos in
    let end_column = Option.map Cerb_position.column end_pos in
    Some
      DT.
        { index = 0;
          name = "TODO stack frame name";
          source;
          start_line;
          start_column;
          end_line;
          end_column
        }


  let display_context' ?loc ?backup_loc (ctx : Context.t) : DT.state =
    let vars : DT.Variable.ts =
      [ ("Computational", display_sym_map ctx.computational);
        ("Logical", display_sym_map ctx.logical);
        ("Resources", List.map display_resource ctx.resources);
        ("Constraints", display_constraints ctx.constraints)
      ]
    in
    let frames =
      match make_stack_frame loc ctx.where backup_loc with Some f -> [ f ] | None -> []
    in
    { vars; frames }


  let display_context ?loc ?backup_loc ctx =
    let state = lazy (display_context' ?loc ?backup_loc ctx) in
    fun () -> Lazy.force state


  let empty_state : DT.state = { vars = []; frames = [] }
end

module Core_level = struct
  let rec display ~backup_loc : Context.t Or_TypeError.t t -> DT.t = function
    | End (Ok ctx) ->
      let get_state = display_context ~backup_loc ctx in
      let r : DT.t' = { ok = true; msg = "Ok"; get_state } in
      End r
    | End (Error e) ->
      let get_state =
        match TypeErrors.get_ctx e with
        | Some (ctx, _) -> display_context ~backup_loc ctx
        | None -> fun () -> empty_state
      in
      let msg = TypeErrors.to_string_short e in
      let r : DT.t' = { ok = false; msg; get_state } in
      End r
    | Vanish -> Vanish
    | Breakpoint (b, n) ->
      (match display_breakpoint ~backup_loc b with
       | None -> display ~backup_loc (compute_next n)
       | Some b' ->
         let n' = display_next ~backup_loc n in
         Breakpoint (b', n'))
    | Choice cs ->
      let cs' =
        List.mapi (fun i (c, n) -> ((show_case c, i), display_next ~backup_loc n)) cs
      in
      Choice cs'


  and display_next ~backup_loc (n : Context.t Or_TypeError.t next) : DT.next =
    let memo = next_to_memo n in
    let memo' = Memo.map memo (display ~backup_loc) in
    DT.T.next_of_memo memo'


  and display_breakpoint ~backup_loc = function
    | Bp (Core_step (expr, ctx)) ->
      let msg = Pp.plain @@ Pp_mucore.pp_expr_w (Some 3) expr in
      let get_state = display_context ~backup_loc ctx in
      Some (DT.step get_state msg)
    | Bp (Nest n) -> Some (DT.nest [ display_next ~backup_loc n ])
    | Bp Step_in -> Some DT.step_in
    | Bp Step_out -> Some DT.step_out
    | Bp (Proc_start _ | Proc_end _) -> None
end

module Lifted = struct
  let ( let* ) = Option.bind

  let pp_expr fmt (Cabs.CabsExpression (_, e)) =
    let open Cabs in
    match e with
    | CabsEident (Identifier (_, i)) -> Fmt.pf fmt "%s" i
    | CabsEconst
        (CabsInteger_const (c, _) | CabsFloating_const (c, _) | CabsCharacter_const (_, c))
      ->
      Fmt.pf fmt "%s" c
    | CabsEstring (_, strs) ->
      let strs = List.concat_map snd strs in
      Fmt.pf fmt "%a" Fmt.(list ~sep:nop string) strs
    | _ -> Fmt.pf fmt "<expr>"


  let pp_stmt fmt (Cabs.CabsStatement (_, _, s)) =
    let open Cabs in
    match s with
    | CabsSlabel (Identifier (_, i), _) -> Fmt.pf fmt "%s:" i
    | CabsScase (e, _) -> Fmt.pf fmt "case %a:" pp_expr e
    | CabsSdefault _ -> Fmt.pf fmt "default:"
    | CabsSblock _ -> Fmt.pf fmt "{}"
    | CabsSdecl _ -> Fmt.pf fmt "decl"
    | CabsSnull -> Fmt.pf fmt "null"
    | CabsSexpr e -> pp_expr fmt e
    | CabsSif (e, _, _) -> Fmt.pf fmt "if (%a)" pp_expr e
    | CabsSswitch (e, _) -> Fmt.pf fmt "switch (%a)" pp_expr e
    | CabsSwhile (e, _) -> Fmt.pf fmt "while (%a)" pp_expr e
    | CabsSdo (e, _) -> Fmt.pf fmt "do while (%a)" pp_expr e
    | CabsSfor _ -> Fmt.pf fmt "for"
    | CabsSgoto (Identifier (_, i)) -> Fmt.pf fmt "goto %s" i
    | CabsScontinue -> Fmt.pf fmt "continue"
    | CabsSbreak -> Fmt.pf fmt "break"
    | CabsSreturn None -> Fmt.pf fmt "return"
    | CabsSreturn (Some e) -> Fmt.pf fmt "return %a" pp_expr e
    | CabsSpar _ -> Fmt.pf fmt "... , ..."
    | CabsSasm _ -> Fmt.pf fmt "<asm>"
    | CabsScaseGNU (e1, e2, _) -> Fmt.pf fmt "case %a ... %a:" pp_expr e1 pp_expr e2
    | CabsSmarker _ -> Fmt.pf fmt "<marker>"


  let get_origin cabs_index (expr : BaseTypes.t Mucore.expr) =
    let open Cerb_frontend.Annot in
    let open Cerb_location in
    let (Mucore.Expr (_, annots, _, _)) = expr in
    let* id = List.find_map (function Aloc (CLoc (_, id)) -> id | _ -> None) annots in
    CI.find_opt cabs_index id


  let rec display dctx : Context.t Or_TypeError.t t -> DT.t =
    let _, backup_loc = dctx in
    function
    | End (Ok ctx) ->
      let get_state = display_context ?backup_loc ctx in
      let r : DT.t' = { ok = true; msg = "Ok"; get_state } in
      End r
    | End (Error e) ->
      let get_state =
        match TypeErrors.get_ctx e with
        | Some (ctx, _) -> display_context ?backup_loc ctx
        | None -> fun () -> empty_state
      in
      let msg = TypeErrors.to_string_short e in
      let r : DT.t' = { ok = false; msg; get_state } in
      End r
    | Vanish -> Vanish
    | Breakpoint (b, n) ->
      (match display_breakpoint dctx b with
       | None -> display dctx (compute_next n)
       | Some b' ->
         let n' = display_next dctx n in
         Breakpoint (b', n'))
    | Choice cs ->
      let cs' = List.mapi (fun i (c, n) -> ((show_case c, i), display_next dctx n)) cs in
      Choice cs'


  and display_next dctx (n : Context.t Or_TypeError.t next) : DT.next =
    let memo = next_to_memo n in
    let memo' = Memo.map memo (display dctx) in
    DT.T.next_of_memo memo'


  and display_breakpoint ((cabs_index, backup_loc) as dctx) (Bp bp) =
    match bp with
    | Core_step (expr, ctx) ->
      let* stmt =
        match get_origin cabs_index expr with
        | None | Some (CI.OExpr _) -> None
        | Some (CI.OStmt (CabsStatement (_, _, s) as cabs_stmt)) ->
          (match s with CabsSblock _ | CabsSnull -> None | _ -> Some cabs_stmt)
      in
      let msg = Fmt.str "%a" pp_stmt stmt in
      let (Cabs.CabsStatement (loc, _, _)) = stmt in
      let get_state = display_context ~loc ?backup_loc ctx in
      Some (DT.step get_state msg)
    | Proc_start ctx ->
      let get_state = display_context ?backup_loc ctx in
      Some (DT.step get_state "Setting up")
    | Proc_end ctx ->
      let get_state = display_context ?backup_loc ctx in
      Some (DT.step get_state "Final checks")
    | Nest n -> Some (DT.nest [ display_next dctx n ])
    | Step_in -> None
    | Step_out -> None


  let display ~cabs_index ?backup_loc = display (cabs_index, backup_loc)
end
