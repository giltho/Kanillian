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
             };
         _;
       } ->
       let idx = Z.to_int z in
       String.sub str (Z.to_int z) (String.length str - idx)
   | StringConstant str -> str
   | _ -> Error.unexpected ("not a pure string literal: " ^ GExpr.show e) *)

(** Gillian-C utils for compilation*)
module Gcu = struct
  include Compcert
  module Vt = Cgil_lib.ValueTranslation
end

let call_for_binop
    ~(ctx : Ctx.t)
    ~(lty : IntType.t)
    (binop : Ops.Binary.t)
    e1
    e2 : Expr.t Cs.with_cmds =
  (* For now, we assume we're on archi64 exactly, then we'll figure out a bit more *)
  assert (Machine_model.equal Machine_model.archi64 ctx.machine);
  let internal_function =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open! Kconstants.Comp_functions in
    match binop with
    | Notequal -> (
        match lty with
        | I_int | I_char -> cmp_ne
        | I_bool -> cmpu_ne
        | I_size_t -> cmplu_ne
        | I_ssize_t -> cmplu_ne)
    | Le -> (
        match lty with
        | I_int | I_char -> cmp_le
        | I_bool -> cmpu_le
        | I_size_t -> cmplu_le
        | I_ssize_t -> cmpl_le)
    | Lt -> (
        match lty with
        | I_int | I_char -> cmp_lt
        | I_bool -> cmpu_lt
        | I_size_t -> cmplu_lt
        | I_ssize_t -> cmpl_lt)
    | Ge -> (
        match lty with
        | I_int | I_char -> cmp_ge
        | I_bool -> cmpu_ge
        | I_size_t -> cmplu_ge
        | I_ssize_t -> cmpl_ge)
    | _ -> Error.unhandled "binary operator: " ^ Ops.Binary.show binop
  in
  let gvar = Ctx.fresh_v ctx in
  let call =
    Cmd.Call (gvar, Lit (String internal_function), [ e1; e2 ], None, None)
  in
  (Expr.PVar gvar, [ call ])

let assume_type ~ctx (type_ : GType.t) (lvar : string) : unit Cs.with_cmds =
  let llvar = Expr.LVar lvar in
  Cs.unit
  @@
  match type_ with
  | CInteger ty ->
      let open Cgil_lib.CConstants.VTypes in
      let str_constr =
        match ty with
        | I_int -> int_type
        | _ -> Error.unhandled ("nondet for int type: " ^ IntType.show ty)
      in
      let value = Ctx.fresh_lv ctx in
      let e_value = Expr.LVar value in
      let assume_list =
        let f =
          Formula.Eq (llvar, EList [ Lit (String str_constr); e_value ])
        in
        Cmd.Logic (Assume f)
      in
      let assume_int = Cmd.Logic (AssumeType (value, IntType)) in
      [ assume_list; assume_int ]
  | Double ->
      let open Cgil_lib.CConstants.VTypes in
      let value = Ctx.fresh_lv ctx in
      let e_value = Expr.LVar value in
      let assume_list =
        let f =
          Formula.Eq (llvar, EList [ Lit (String float_type); e_value ])
        in
        Cmd.Logic (Assume f)
      in
      let assume_num = Cmd.Logic (AssumeType (value, NumberType)) in
      [ assume_list; assume_num ]
  | Float ->
      let open Cgil_lib.CConstants.VTypes in
      let value = Ctx.fresh_lv ctx in
      let e_value = Expr.LVar value in
      let assume_list =
        let f =
          Formula.Eq (llvar, EList [ Lit (String single_type); e_value ])
        in
        Cmd.Logic (Assume f)
      in
      let assume_num = Cmd.Logic (AssumeType (value, NumberType)) in
      [ assume_list; assume_num ]
  | Pointer _ ->
      let loc = Ctx.fresh_lv ctx in
      let ofs = Ctx.fresh_lv ctx in
      let e_loc = Expr.LVar loc in
      let e_ofs = Expr.LVar ofs in
      let assume_list =
        let f = Formula.Eq (llvar, EList [ e_loc; e_ofs ]) in
        Cmd.Logic (Assume f)
      in
      let assume_obj = Cmd.Logic (AssumeType (loc, ObjectType)) in
      let assume_int = Cmd.Logic (AssumeType (ofs, IntType)) in
      [ assume_list; assume_obj; assume_int ]
  | Bool ->
      let v = Ctx.fresh_lv ctx in
      let assume_bool = Cmd.Logic (AssumeType (v, BooleanType)) in
      [ assume_bool ]
  | StructTag tag | UnionTag tag | Struct { tag; _ } | Union { tag; _ } ->
      (* TODO: Add a signal here for something unhandled! *)
      let fail = Cmd.Fail ("unhandled_nondet", [ Expr.Lit (String tag) ]) in
      [ fail ]
  | _ ->
      let ty_str = GType.show type_ in
      let fail = Cmd.Fail ("unhandled_nondet", [ Expr.Lit (String ty_str) ]) in
      [ fail ]

let nondet_expr ~ctx ~add_annot ~type_ () : Expr.t Cs.with_body =
  let open Cs.Syntax in
  let is_symbolic_exec =
    let open Gillian.Utils in
    ExecMode.symbolic_exec !Config.current_exec_mode
  in
  if not is_symbolic_exec then
    Error.user_error
      "Looks like you're compiling some nondet variables, but you're not in \
       symbolic execution mode"
  else
    let res =
      if Ctx.is_zst_access ctx type_ then Cs.return (Expr.Lit Null)
      else
        let* hash_x = Cs.return (Ctx.fresh_lv ctx) in
        let* () = Cs.unit [ Cmd.Logic (LCmd.SpecVar [ hash_x ]) ] in
        let+ () = assume_type ~ctx type_ hash_x in
        Expr.LVar hash_x
    in
    Cs.map_l add_annot res

let compile_cast ~(ctx : Ctx.t) ~(from : GType.t) ~(into : GType.t) e :
    Expr.t Cs.with_cmds =
  let function_name =
    match (from, into) with
    | Bool, CInteger (I_bool | I_char | I_int) ->
        Cgil_lib.CConstants.Internal_Functions.val_of_bool
    | CInteger I_int, CInteger (I_size_t | I_ssize_t) ->
        assert (ctx.machine.pointer_width == 64);
        Cgil_lib.CConstants.UnOp_Functions.longofint
    | CInteger I_int, Unsignedbv { width } when ctx.machine.int_width == width
      -> Kconstants.Cast_functions.unsign_int
    | CInteger I_ssize_t, CInteger I_size_t when ctx.machine.pointer_width == 64
      -> Kconstants.Cast_functions.unsign_long
    | CInteger I_ssize_t, CInteger I_size_t when ctx.machine.pointer_width == 32
      -> Kconstants.Cast_functions.unsign_int
    | _ ->
        Error.unhandled
          (Printf.sprintf "Cannot perform cast yet from %s into %s"
             (GType.show from) (GType.show into))
  in
  let function_name = Expr.Lit (String function_name) in
  let temp = Ctx.fresh_v ctx in
  let call = Cmd.Call (temp, function_name, [ e ], None, None) in
  Cs.return ~app:[ call ] (Expr.PVar temp)

let rec compile_expr ~(ctx : Ctx.t) (expr : GExpr.t) : Expr.t Cs.with_body =
  let open Cs.Syntax in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location expr.location in
  let id = expr.location.origin_id in
  let b = Body_item.make ~loc ~id in
  if Ctx.is_zst_access ctx expr.type_ then Cs.return (Expr.Lit Null)
  else
    match expr.value with
    | Symbol _ | Dereference _ | Index _ | Member _ -> (
        let* access = Lvalue.as_access ~ctx ~read:true expr in
        match access with
        | ZST -> Cs.return (Expr.Lit Null)
        | Direct x -> Cs.return (Expr.PVar x)
        | InMemoryScalar { loaded = Some e; _ } -> Cs.return e
        | InMemoryScalar { loaded = None; ptr } ->
            let+ var = Memory.load_scalar ~ctx ptr expr.type_ |> Cs.map_l b in
            Expr.PVar var)
    | AddressOf x -> (
        let* access = Lvalue.as_access ~ctx ~read:true x in
        match access with
        | ZST ->
            Cs.return
              (Expr.EList [ Lit (String "dangling"); Lit (String "pointer") ])
        | Direct x -> Error.code_error ("address of direct access to " ^ x)
        | InMemoryScalar { ptr; _ } -> Cs.return ptr)
    | BoolConstant b -> Cs.return (Expr.Lit (Bool b))
    | CBoolConstant b ->
        let i = if b then Gcu.Camlcoq.Z.one else Gcu.Camlcoq.Z.zero in
        let lit = Gcu.Vt.gil_of_compcert (Gcu.Values.Vint i) in
        Cs.return (Expr.Lit lit)
    | PointerConstant b ->
        let i = Gcu.Camlcoq.Z.of_sint b in
        let v =
          match ctx.machine.pointer_width with
          | 32 -> Gcu.Values.Vint i
          | 64 -> Gcu.Values.Vlong i
          | _ -> Error.unhandled "PointerConstant - unknown archi"
        in
        let lit = Gcu.Vt.gil_of_compcert v in
        Cs.return (Expr.Lit lit)
    | IntConstant z ->
        let int_ty = Goto_lib.Type.as_int_type expr.type_ in
        let cz = Gcu.Vt.z_of_int z in
        let ccert_value =
          let open Gcu.Values in
          match int_ty with
          | I_int | I_char -> Vint cz
          | I_size_t | I_ssize_t -> (
              match ctx.machine.pointer_width with
              | 32 -> Vint cz
              | 64 -> Vlong cz
              | _ ->
                  Error.unhandled
                    "Gillian only handles archi 32 and archi 64 for now")
          | I_bool -> Error.unexpected "IntConstant with type I_bool"
        in
        let lit = Gcu.Vt.gil_of_compcert ccert_value in
        Cs.return (Expr.Lit lit)
    | BinOp { op; lhs; rhs } ->
        let* e1 = compile_expr lhs in
        let* e2 = compile_expr rhs in
        let lty = lhs.type_ |> GType.as_int_type in
        call_for_binop ~ctx ~lty op e1 e2 |> Cs.map_l b
    | UnOp { op; e } -> (
        let+ e = compile_expr e in

        match op with
        | Not -> Expr.Infix.not e
        | _ -> Error.unhandled ("Unary operator: " ^ Ops.Unary.show op))
    | Nondet -> nondet_expr ~ctx ~add_annot:b ~type_:expr.type_ ()
    | TypeCast to_cast ->
        let* to_cast_e = compile_expr to_cast in
        compile_cast ~ctx ~from:to_cast.type_ ~into:expr.type_ to_cast_e
        |> Cs.map_l b
    | Assign { lhs; rhs } -> compile_assign ~ctx ~lhs ~rhs ~annot:b
    | FunctionCall { func; args } -> compile_call ~ctx ~add_annot:b func args
    | _ -> Error.unhandled ("Cannot compile expr yet: " ^ GExpr.show expr)

and compile_call ~ctx ~add_annot:b (func : GExpr.t) (args : GExpr.t list) =
  let open Cs.Syntax in
  match GExpr.as_symbol func with
  | "__CPROVER_assume" ->
      let to_assume =
        match args with
        | [ to_assume ] -> to_assume
        | _ -> Error.user_error "__CPROVER_assume not given 1 params"
      in
      (* I'm not sure it's always an int, I'll need to check, but there's a strong chance *)
      let cast_to_bool =
        match to_assume.type_ with
        | Bool -> false
        | _ -> true
      in
      let* to_assume = compile_expr ~ctx to_assume in
      let* to_assume =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Cgil_lib.CConstants.Internal_Functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assume ], None, None)
          in
          Cs.return ~app:[ b call ] (Expr.PVar temp)
        else Cs.return to_assume
      in
      let f =
        match Formula.lift_logic_expr to_assume with
        | None ->
            Error.code_error
              (Fmt.str "obtained weird expression that cannot be asserted: %a"
                 Expr.pp to_assume)
        | Some (f, _) -> f
      in
      Cs.return ~app:[ b (Logic (Assume f)) ] (Expr.Lit Null)
  | "__CPROVER_assert" ->
      (* The second argument of assert is a string that we may to keep
         alive for error messages. For now we're discarding it.
         In the future, we could change Gillian's assume to also have an error message *)
      (* I should still find a way to factor out the call to assume and assert *)
      let to_assert =
        match args with
        | [ to_assert; _msg ] -> to_assert
        | _ -> Error.user_error "__CPROVER_assert not given 2 params"
      in
      (* I'm not sure it's always an int, I'll need to check, but there's a strong chance *)
      let cast_to_bool =
        match to_assert.type_ with
        | Bool -> false
        | _ -> true
      in
      let* to_assert = compile_expr ~ctx to_assert in
      let* to_assert =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Cgil_lib.CConstants.Internal_Functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assert ], None, None)
          in
          Cs.return ~app:[ b call ] (Expr.PVar temp)
        else Cs.return to_assert
      in
      let f =
        match Formula.lift_logic_expr to_assert with
        | None ->
            Error.code_error
              (Fmt.str "obtained weird expression that cannot be asserted: %a"
                 Expr.pp to_assert)
        | Some (f, _) -> f
      in
      Cs.return ~app:[ b (Logic (Assert f)) ] (Expr.Lit Null)
  | _ ->
      let fname = GExpr.as_symbol func in
      let* args = Cs.many (compile_expr ~ctx) args in
      let ret_var = Ctx.fresh_v ctx in
      let gil_call = Cmd.Call (ret_var, Lit (String fname), args, None, None) in
      Cs.return ~app:[ b gil_call ] (Expr.PVar ret_var)

and compile_assign ~ctx ~annot ~lhs ~rhs =
  let v, pre1 = compile_expr ~ctx rhs in
  let access, pre2 = Lvalue.as_access ~ctx ~read:false lhs in
  let write =
    match access with
    | ZST -> []
    | Direct x -> [ annot (Cmd.Assignment (x, v)) ]
    | InMemoryScalar { ptr; _ } ->
        [ annot (Memory.store_scalar ~ctx ptr v lhs.type_) ]
  in
  (v, pre1 @ pre2 @ write)

let rec compile_statement ~ctx (stmt : Stmt.t) : Body_item.t list =
  let compile_statement = compile_statement ~ctx in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location stmt.location in
  let id = stmt.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot x = List.map b x in
  let set_first_label label stmts =
    match stmts with
    | [] -> [ b ~label Skip ]
    | (a, None, cmd) :: r -> (a, Some label, cmd) :: r
    | (_, Some _, _) :: _ -> Error.code_error "First label is already set!!"
  in
  match stmt.body with
  | Skip -> [ b Skip ]
  | Block ss -> List.concat_map compile_statement ss
  | Label (s, ss) -> List.concat_map compile_statement ss |> set_first_label s
  | Goto lab -> [ b (Goto lab) ]
  | Assume { cond } ->
      let e, pre = compile_expr cond in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assume f)) ]
  | Assert { property_class = Some "coverage_check"; _ } ->
      (* We can't output nothing, as a label might have to get attached *)
      [ b Skip ]
  | Assert { cond; property_class = _ } ->
      let e, pre = compile_expr cond in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assert f)) ]
  | Return e ->
      let e, s =
        match e with
        | Some e -> compile_expr e
        | None -> (Lit Null, [])
      in
      let variable = Utils.Names.return_variable in
      s @ add_annot [ Assignment (variable, e); ReturnNormal ]
  | Decl { lhs; value } ->
      let ty = lhs.type_ in
      let lhs = GExpr.as_symbol lhs in
      if Ctx.in_memory ctx lhs then
        let loc = Ctx.fresh_v ctx in
        let alloc = Cgil_lib.LActions.(str_ac (AMem Alloc)) in
        let size = Ctx.size_of ctx ty in
        let action_cmd =
          Cmd.LAction (loc, alloc, [ Expr.zero_i; Expr.int size ])
        in
        let loc = Expr.list_nth (Expr.PVar loc) 0 in
        let assign = Cmd.Assignment (lhs, Expr.EList [ loc; Expr.zero_i ]) in
        let write =
          match value with
          | None -> []
          | Some e ->
              let v, pre = compile_expr e in
              let write = Memory.store_scalar ~ctx (Expr.PVar lhs) v ty in
              pre @ [ b write ]
        in
        [ b action_cmd; b assign ] @ write
      else
        let v, s =
          match value with
          | Some e -> compile_expr e
          | None -> (Lit Undefined, [])
        in
        s @ [ b (Assignment (lhs, v)) ]
  | Assign { lhs; rhs } ->
      let _, body = compile_assign ~ctx ~annot:b ~lhs ~rhs in
      body
  | Expression e ->
      let _, s = compile_expr e in
      s
  | FunctionCall { lhs; func; args } -> (
      let v, pre1 = compile_call ~ctx ~add_annot:b func args in
      match lhs with
      | None -> pre1
      | Some lvalue ->
          let access, pre2 = Lvalue.as_access ~ctx ~read:false lvalue in
          let write =
            match access with
            | ZST -> []
            | Direct x -> [ b (Cmd.Assignment (x, v)) ]
            | InMemoryScalar { ptr; _ } ->
                [ b (Memory.store_scalar ~ctx ptr v lvalue.type_) ]
          in
          pre1 @ pre2 @ write)
  | Switch _ -> Error.unhandled "switch statement"

(* This is to be used without a current body.
   Do not call fresh_v or fresh_lv inside *)
let set_global_var ~ctx (gv : Program.Global_var.t) : Body_item.t Seq.t =
  let b =
    let loc = Body_item.compile_location gv.location in
    let id = gv.location.origin_id in
    Body_item.make ~loc ~id
  in
  (* We start by allocating the variable *)
  let sz = Ctx.size_of ctx gv.type_ |> Expr.int in
  let ll = "ll" in
  let alloc = Cgil_lib.LActions.(str_ac (AMem Alloc)) in
  let action_cmd = b @@ Cmd.LAction (ll, alloc, [ Expr.zero_i; sz ]) in
  let loc_expr = Expr.list_nth (Expr.PVar ll) 0 in
  let loc = "loc" in
  let assign_cmd = b @@ Cmd.Assignment (loc, loc_expr) in
  let loc = Expr.PVar loc in
  let store_zeros_cmd =
    let store_zeros = Cgil_lib.CConstants.Internal_Functions.store_zeros in
    b @@ Cmd.Call ("u", Lit (String store_zeros), [ loc; sz ], None, None)
  in

  let store_value_cmds =
    match gv.value with
    | None -> []
    | Some e ->
        let v, v_init_cmds = compile_expr ~ctx e in
        let store_value =
          Memory.store_scalar ~ctx ~var:"u"
            (Expr.EList [ loc; Expr.zero_i ])
            v gv.type_
        in
        v_init_cmds @ [ b store_value ]
  in
  let drom_perm_cmd =
    let drom_perm = Cgil_lib.LActions.(str_ac (AMem DropPerm)) in
    let perm_string =
      Expr.Lit (String (Gcu.Vt.string_of_permission Writable))
    in
    b @@ Cmd.LAction ("u", drom_perm, [ loc; Expr.zero_i; sz; perm_string ])
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
  [ action_cmd; assign_cmd; store_zeros_cmd ]
  @ store_value_cmds
  @ [ drom_perm_cmd; set_symbol_cmd; set_def_cmd ]
  |> List.to_seq

let set_global_env_proc (ctx : Ctx.t) =
  let ctx = Ctx.with_new_generators ctx in
  let variables = Hashtbl.to_seq_values ctx.prog.vars in
  let body = Seq.concat_map (set_global_var ~ctx) variables in
  let ret =
    let b = Body_item.make in
    let assign = b @@ Cmd.Assignment (Kutils.Names.return_variable, Lit Null) in
    let ret = b Cmd.ReturnNormal in
    Seq.cons assign (Seq.return ret)
  in
  let body = Seq.cons body (Seq.return ret) in
  let body = Array.of_seq (Seq.concat body) in
  Proc.
    {
      proc_name = Cgil_lib.CConstants.Internal_Functions.initialize_genv;
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
  let ctx = Ctx.with_entering_body ctx body in
  let proc_params =
    List.map
      (fun x ->
        match x.Param.identifier with
        | None -> Ctx.fresh_v ctx
        | Some s -> s)
      func.params
  in
  let proc_spec = None in
  let free_locals = compile_free_locals ctx in
  let init_call =
    if func.symbol = "main" then
      let init_f = Cgil_lib.CConstants.Internal_Functions.initialize_genv in
      [
        Body_item.make
          (Cmd.Call (Ctx.fresh_v ctx, Lit (String init_f), [], None, None));
      ]
    else []
  in
  let proc_body =
    Array.of_list (init_call @ compile_statement ~ctx body @ free_locals)
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

let compile (context : Ctx.t) : (Annot.t, string) Prog.t =
  let program = context.prog in
  let gil_prog = Prog.create () in
  let gil_prog =
    Program.fold_functions
      (fun _ f prog -> Prog.add_proc prog (compile_function ~ctx:context f))
      program gil_prog
  in

  let gil_prog = Prog.add_proc gil_prog (set_global_env_proc context) in
  assert (Machine_model.equal context.machine Machine_model.archi64);
  let imports =
    Kconstants.Imports.imports
    @ Cgil_lib.CConstants.Imports.imports Arch64
        !Gillian.Utils.Config.current_exec_mode
        Unallocated_functions
  in
  { gil_prog with imports }
