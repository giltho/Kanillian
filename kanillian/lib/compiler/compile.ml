open Gil_syntax
module GExpr = Goto_lib.Expr
module GType = Goto_lib.Type

(* let as_pure_string_literal (e : GExpr.t) =
   match e.value with
   | AddressOf

         value =
           Index
             {
               array = { value = StringConstant str; _ };
               index = { value = IntConstant z; _ };
             }
         _;
       } ->
       let idx = Z.to_int z in
       String.sub str (Z.to_int z) (String.length str - idx)
   | StringConstant str -> str
   | _ -> Error.unexpected ("not a pure string literal: " ^ GExpr.show e) *)

(** Gillian-C utils for compilation*)

open Helpers
open Compile_expr

let rec compile_statement ~ctx (stmt : Stmt.t) : Body_item.t list =
  let compile_statement_c = compile_statement ~ctx in
  let compile_expr_c = compile_expr ~ctx in
  let loc = Body_item.compile_location stmt.location in
  let id = stmt.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot x = List.map b x in
  let set_first_label_opt label stmts =
    Helpers.set_first_label_opt ~annot:(b ~loop:[]) label stmts
  in
  let set_first_label label stmts = set_first_label_opt (Some label) stmts in
  match stmt.body with
  | Skip -> [ b Skip ]
  | Block ss -> List.concat_map compile_statement_c ss
  | Label (s, ss) -> List.concat_map compile_statement_c ss |> set_first_label s
  | Goto lab -> [ b (Goto lab) ]
  | Assume { cond } ->
      let e, pre = compile_expr_c cond in
      let e = Val_repr.as_value ~msg:"Assume operand" e in
      (* FIXME: hack to avoid wrong compilation to be in the way.
         Remove that later. *)
      let e =
        Expr.subst_expr_for_expr ~to_subst:(Expr.Lit Nono)
          ~subst_with:(Expr.Lit (Bool true)) e
      in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assume f)) ]
  (* We can't output nothing, as a label might have to get attached *)
  | Assert { property_class = Some "cover"; _ } -> [ b Skip ]
  | Assert { cond; property_class = _ } ->
      let e, pre = compile_expr_c cond in
      let e = Val_repr.as_value ~msg:"Assert operand" e in
      let e =
        Expr.subst_expr_for_expr ~to_subst:(Expr.Lit Nono)
          ~subst_with:(Expr.Lit (Bool false)) e
      in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assert f)) ]
  | Return e ->
      let e, s =
        match e with
        | Some e -> (
            let open Cs.Syntax in
            let* e = compile_expr_c e in
            match e with
            | ByValue e -> Cs.return e
            | ByCopy { ptr; type_ } ->
                let dst =
                  Expr.PVar Kconstants.Kanillian_names.return_by_copy_name
                in
                let copy_cmd = Memory.memcpy ~ctx ~type_ ~src:ptr ~dst in
                Cs.return ~app:[ b copy_cmd ] (Expr.Lit Undefined)
            | ByCompositValue { writes; _ } ->
                let dst =
                  Expr.PVar Kconstants.Kanillian_names.return_by_copy_name
                in
                let cmds = Memory.write_composit ~ctx ~annot:b ~dst writes in
                Cs.return ~app:cmds (Expr.Lit Undefined)
            | Procedure _ -> Error.code_error "Return value is a procedure")
        | None -> Cs.return ~app:[] (Expr.Lit Undefined)
      in
      let variable = Utils.Names.return_variable in
      s @ add_annot [ Assignment (variable, e); ReturnNormal ]
  | Decl { lhs = glhs; value } ->
      (* TODO:
         I have too many if/elses for deciding how things should be done,
         and that's all over the compiler. I should probably have a variant
         and a unique decision procedure for that. *)
      let ty = glhs.type_ in
      let lhs = GExpr.as_symbol glhs in
      (* ZSTs are just (GIL) Null values *)
      if Ctx.is_zst_access ctx ty then
        let cmd = Cmd.Assignment (lhs, Lit Null) in
        [ b cmd ]
      else if not (Ctx.representable_in_store ctx ty) then
        let ptr, alloc_cmd = Memory.alloc_ptr ~ctx ty in
        let assign = Cmd.Assignment (lhs, ptr) in
        let write =
          match value with
          | None -> []
          | Some gv -> (
              let v, cmds = compile_expr_c gv in
              match v with
              | Val_repr.ByCompositValue { writes; _ } ->
                  cmds @ Memory.write_composit ~ctx ~annot:b ~dst:ptr writes
              | Val_repr.ByCopy { type_; ptr = src } ->
                  [ b (Memory.memcpy ~ctx ~type_ ~dst:ptr ~src) ]
              | _ ->
                  Error.code_error
                    "Declaring composit value, not writing a composit")
        in
        [ b alloc_cmd; b assign ] @ write
      else if Ctx.in_memory ctx lhs then
        let ptr, action_cmd = Memory.alloc_ptr ~ctx ty in
        let assign = Cmd.Assignment (lhs, ptr) in
        let write =
          match value with
          | None -> []
          | Some e ->
              let v, pre = compile_expr_c e in
              let v =
                Val_repr.as_value ~error:Error.code_error
                  ~msg:"declaration initial value for in-memory scalar access" v
              in
              let write = Memory.store_scalar ~ctx (Expr.PVar lhs) v ty in
              pre @ [ b write ]
        in
        [ b action_cmd; b assign ] @ write
      else
        let v, s =
          match value with
          | Some e ->
              let e, s = compile_expr_c e in
              let e =
                Val_repr.as_value ~error:Error.code_error
                  ~msg:"in memory scalar" e
              in
              (e, s)
          | None -> (Lit Undefined, [])
        in
        s @ [ b (Assignment (lhs, v)) ]
  | Assign { lhs; rhs } ->
      let _, body = compile_assign ~ctx ~annot:b ~lhs ~rhs in
      body
  | Expression e ->
      let _, s = compile_expr_c e in
      s
  | FunctionCall { lhs; func; args } -> (
      let v, pre1 = compile_call ~ctx ~add_annot:b func args in
      match lhs with
      | None -> pre1
      | Some lvalue ->
          let access, pre2 = lvalue_as_access ~ctx ~read:false lvalue in
          let write =
            match (access, v) with
            | ZST, _ -> []
            | Direct x, ByValue v -> [ b (Cmd.Assignment (x, v)) ]
            | InMemoryScalar { ptr; _ }, ByValue v ->
                [ b (Memory.store_scalar ~ctx ptr v lvalue.type_) ]
            | InMemoryComposit { ptr = dst; type_ }, ByCopy { ptr = src; _ } ->
                [ b (Memory.memcpy ~ctx ~type_ ~src ~dst) ]
            | _ ->
                Error.code_error
                  (Fmt.str
                     "Wrong mix of access and value kind for function call:\n\
                      %a = %a" pp_access access Val_repr.pp v)
          in
          pre1 @ pre2 @ write)
  | Switch { control; cases; default } ->
      let end_lab = Ctx.fresh_lab ctx in
      let next_lab = ref None in
      let control_ty = control.type_ in
      let control, control_s = compile_expr_c control in
      let rec compile_cases ~ctx ?(acc = []) = function
        | [] -> acc
        | case :: rest ->
            let cur_lab = !next_lab in
            let nlab = Ctx.fresh_lab ctx in
            next_lab := Some nlab;
            let guard = case.Stmt.case in
            let guard_e, guard_s = compile_expr ~ctx guard in
            let equal_v, comparison_calls =
              compile_binop ~ctx ~lty:control_ty ~rty:guard.type_ Equal control
                guard_e
            in
            let comparison_calls =
              List.map
                (Body_item.make_hloc ~loc:guard.location)
                comparison_calls
            in
            let block = compile_statement ~ctx case.sw_body in
            let block_lab, block = Body_item.get_or_set_fresh_lab ~ctx block in
            let goto_block = Cmd.GuardedGoto (equal_v, block_lab, nlab) in
            let total_block =
              set_first_label_opt cur_lab
                (guard_s @ comparison_calls @ [ b goto_block ] @ block)
            in
            let acc = acc @ total_block in
            compile_cases ~ctx ~acc rest
      in
      let compiled_cases =
        Ctx.with_break ctx end_lab (fun ctx -> compile_cases ~ctx cases)
      in
      let default_block =
        match default with
        | None -> [ b ?label:!next_lab Skip ]
        | Some default ->
            Ctx.with_break ctx end_lab (fun ctx ->
                set_first_label_opt !next_lab (compile_statement ~ctx default))
      in
      let end_ = [ b ~label:end_lab Skip ] in
      control_s @ compiled_cases @ default_block @ end_
  | Break -> (
      match ctx.break_lab with
      | None -> Error.unexpected "Break call outside of loop of switch"
      | Some break_lab -> [ b (Cmd.Goto break_lab) ])
  | Unhandled id -> [ b (assert_unhandled ~feature:(StmtIrep id) []) ]
  | Output _ ->
      let () = Stats.Unhandled.signal OutputStmt in
      [ b Skip ]

let set_global_function (fn : Program.Func.t) : Body_item.t Seq.t =
  let b =
    let loc = Body_item.compile_location fn.location in
    let id = fn.location.origin_id in
    Body_item.make ~loc ~id
  in
  let loc_expr, alloc_cmd = Memory.alloc ~loc_var:"ll" ~size:1 in
  let alloc_cmd = b alloc_cmd in
  let loc = "loc" in
  let assign_cmd = b @@ Cmd.Assignment (loc, loc_expr) in
  let loc = Expr.PVar loc in
  let drop_perm_cmd =
    let drom_perm = Cgil_lib.LActions.(str_ac (AMem DropPerm)) in
    let perm_string =
      Expr.Lit (String (Gcu.Vt.string_of_permission Nonempty))
    in
    b
    @@ Cmd.LAction
         ("u", drom_perm, [ loc; Expr.zero_i; Expr.int 1; perm_string ])
  in
  let symexpr = Expr.Lit (String fn.symbol) in
  let set_symbol_cmd =
    let set_symbol = Cgil_lib.LActions.(str_ac (AGEnv SetSymbol)) in
    b @@ Cmd.LAction ("u", set_symbol, [ symexpr; loc ])
  in
  let set_def_cmd =
    let set_def = Cgil_lib.LActions.(str_ac (AGEnv SetDef)) in
    b
    @@ Cmd.LAction
         ("u", set_def, [ loc; EList [ Lit (String "function"); symexpr ] ])
  in
  List.to_seq
    [ alloc_cmd; assign_cmd; drop_perm_cmd; set_symbol_cmd; set_def_cmd ]

(* This is to be used without a current body.
   Do not call fresh_v or fresh_lv inside *)
let set_global_var ~ctx (gv : Program.Global_var.t) : Body_item.t Seq.t =
  let b =
    let loc = Body_item.compile_location gv.location in
    let id = gv.location.origin_id in
    Body_item.make ~loc ~id
  in
  (* If the value is a ZST, we don't even put it in the global environment.
     I'm not sure if that is the correct behaviour. *)
  if Ctx.is_zst_access ctx gv.type_ then Seq.empty
  else
    (* We start by allocating the variable *)
    let size = Ctx.size_of ctx gv.type_ in
    let loc_expr, alloc_cmd = Memory.alloc ~loc_var:"ll" ~size in
    let alloc_cmd = b alloc_cmd in
    let size = Expr.int size in
    let loc = "loc" in
    let assign_cmd = b @@ Cmd.Assignment (loc, loc_expr) in
    let loc = Expr.PVar loc in
    let store_zeros_cmd =
      let store_zeros = Cgil_lib.CConstants.Internal_Functions.store_zeros in
      b @@ Cmd.Call ("u", Lit (String store_zeros), [ loc; size ], None, None)
    in

    let store_value_cmds =
      match gv.value with
      | None -> []
      | Some e ->
          let v, v_init_cmds = compile_expr ~ctx e in
          let dst = Expr.EList [ loc; Expr.zero_i ] in
          let store_value =
            match v with
            | ByValue v ->
                [ b (Memory.store_scalar ~ctx ~var:"u" dst v gv.type_) ]
            | ByCompositValue { writes; _ } ->
                Memory.write_composit ~ctx ~annot:b ~dst writes
            | _ -> Error.unexpected "compile_global_var: not by value"
          in
          v_init_cmds @ store_value
    in
    let drom_perm_cmd =
      let drom_perm = Cgil_lib.LActions.(str_ac (AMem DropPerm)) in
      let perm_string =
        Expr.Lit (String (Gcu.Vt.string_of_permission Writable))
      in
      b @@ Cmd.LAction ("u", drom_perm, [ loc; Expr.zero_i; size; perm_string ])
    in
    let symexpr = Expr.Lit (String gv.symbol) in
    let set_symbol_cmd =
      let set_symbol = Cgil_lib.LActions.(str_ac (AGEnv SetSymbol)) in
      b @@ Cmd.LAction ("u", set_symbol, [ symexpr; loc ])
    in
    let set_def_cmd =
      let set_def = Cgil_lib.LActions.(str_ac (AGEnv SetDef)) in
      b
      @@ Cmd.LAction
           ("u", set_def, [ loc; EList [ Lit (String "variable"); symexpr ] ])
    in
    [ alloc_cmd; assign_cmd; store_zeros_cmd ]
    @ store_value_cmds
    @ [ drom_perm_cmd; set_symbol_cmd; set_def_cmd ]
    |> List.to_seq

let set_global_env_proc (ctx : Ctx.t) =
  let ctx = Ctx.with_new_generators ctx in
  let variables = Hashtbl.to_seq_values ctx.prog.vars in
  let functions = Hashtbl.to_seq_values ctx.prog.funs in
  let set_variables = Seq.concat_map (set_global_var ~ctx) variables in
  let set_functions = Seq.concat_map set_global_function functions in
  let ret =
    let b = Body_item.make in
    let assign = b @@ Cmd.Assignment (Kutils.Names.return_variable, Lit Null) in
    let ret = b Cmd.ReturnNormal in
    Seq.cons assign (Seq.return ret)
  in
  let body = Seq.concat (List.to_seq [ set_variables; set_functions; ret ]) in
  let body = Array.of_seq body in
  Proc.
    {
      proc_name = Kconstants.CBMC_names.initialize;
      proc_source_path = None;
      proc_internal = true;
      proc_body = body;
      proc_params = [];
      proc_spec = None;
    }

let compile_free_locals (ctx : Ctx.t) =
  let open Kutils.Prelude in
  let locals = Hashtbl.copy ctx.locals in
  let () =
    Hashtbl.filter_map_inplace
      (fun symbol (local : Ctx.Local.t) ->
        if Ctx.in_memory ctx symbol then Some local else None)
      locals
  in
  Hashtbl.to_seq_values locals
  |> Seq.map (Memory.dealloc_local ~ctx)
  |> List.of_seq

let compile_alloc_params ~ctx params =
  List.concat_map
    (fun (param, type_) ->
      if
        (not (Ctx.is_zst_access ctx type_))
        && Ctx.representable_in_store ctx type_
        && Ctx.in_memory ctx param
      then
        let ptr, cmda = Memory.alloc_ptr ~ctx type_ in
        let cmdb = Memory.store_scalar ~ctx ptr (PVar param) type_ in
        let cmdc = Cmd.Assignment (param, ptr) in
        [ cmda; cmdb; cmdc ]
      else [])
    params

let compile_function ~ctx (func : Program.Func.t) : (Annot.t, string) Proc.t =
  let f_loc = Body_item.compile_location func.location in
  let body =
    (* If the function has no body, it's assumed to be just non-det *)
    match func.body with
    | Some b -> b
    | None ->
        let nondet =
          GExpr.
            {
              location = func.location;
              type_ = func.return_type;
              value = Nondet;
            }
        in
        Stmt.{ location = func.location; body = Return (Some nondet) }
  in

  (* Fmt.pr "FUNCTION %s:\n%a@?\n\n" func.symbol Stmt.pp body; *)
  let ctx =
    Ctx.with_entering_body ctx ~params:func.params ~body ~location:func.location
  in
  let proc_params =
    List.map
      (fun x ->
        match x.Param.identifier with
        | None -> (Ctx.fresh_v ctx, x.type_)
        | Some s -> (s, x.type_))
      func.params
  in
  let proc_spec = None in
  let free_locals = compile_free_locals ctx in
  (* We add a return undef in case the function has no return *)
  let b = Body_item.make ~loc:f_loc in
  let return_undef =
    [
      b (Assignment (Kutils.Names.return_variable, Lit Undefined));
      b ReturnNormal;
    ]
  in
  let alloc_params = compile_alloc_params ~ctx proc_params |> List.map b in
  let proc_body =
    Array.of_list
      (alloc_params @ compile_statement ~ctx body @ free_locals @ return_undef)
  in
  let proc_params =
    let identifiers = List.map fst proc_params in
    if Ctx.representable_in_store ctx func.return_type then identifiers
    else Kconstants.Kanillian_names.return_by_copy_name :: identifiers
  in
  Proc.
    {
      proc_name = func.symbol;
      proc_source_path = Some f_loc.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
    }

let start_for_harness harness =
  let init_call =
    let init_f = Kconstants.CBMC_names.initialize in
    Body_item.make (Cmd.Call ("u", Lit (String init_f), [], None, None))
  in
  let harness = Sanitize.sanitize_symbol harness in
  let harness_call =
    Body_item.make
      (Cmd.Call
         (Utils.Names.return_variable, Lit (String harness), [], None, None))
  in
  let return = Body_item.make Cmd.ReturnNormal in
  let body = [| init_call; harness_call; return |] in
  Proc.
    {
      proc_name = Kconstants.CBMC_names.start;
      proc_source_path = None;
      proc_internal = true;
      proc_body = body;
      proc_params = [];
      proc_spec = None;
    }

let compile (context : Ctx.t) : (Annot.t, string) Prog.t =
  let program = context.prog in
  let gil_prog = Prog.create () in
  let gil_prog =
    Program.fold_functions
      (fun _ f prog -> Prog.add_proc prog (compile_function ~ctx:context f))
      program gil_prog
  in
  let gil_prog = Prog.add_proc gil_prog (set_global_env_proc context) in
  let gil_prog =
    Option.fold ~none:gil_prog
      ~some:(fun harness ->
        let gil_prog = Prog.add_proc gil_prog (start_for_harness harness) in
        gil_prog)
      context.harness
  in
  assert (Machine_model.equal context.machine Machine_model.archi64);
  let imports =
    Kconstants.Imports.imports
    @ Cgil_lib.CConstants.Imports.imports Arch64
        !Gillian.Utils.Config.current_exec_mode
        Unallocated_functions
  in
  { gil_prog with imports }
