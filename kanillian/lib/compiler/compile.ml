open Gil_syntax
module GExpr = Goto_lib.Expr
module GType = Goto_lib.Type
open Gillian.Utils.Prelude

let symbols_in_memory ~prog =
  let representable_in_store (type_ : GType.t) =
    match type_ with
    | Bool
    | CInteger _
    | Float
    | Double
    | Signedbv _
    | Unsignedbv _
    | Pointer _
    | Empty -> true
    | _ -> Program.is_zst ~prog type_
  in
  let symbol_collector =
    object
      inherit [string Hashset.t] Goto_lib.Visitors.iter as super

      method! visit_expr_value ~ctx ~type_ (e : GExpr.value) =
        match e with
        | Symbol s -> Hashset.add ctx s
        | _ -> super#visit_expr_value ~ctx ~type_ e
    end
  in
  let addressed_visitor =
    object
      inherit [string Hashset.t] Goto_lib.Visitors.iter as super

      method! visit_expr_value ~ctx ~type_ (e : GExpr.value) =
        match e with
        | Member { lhs = e; _ } | Index { array = e; _ } | AddressOf e ->
            symbol_collector#visit_expr ~ctx e;
            super#visit_expr ~ctx e
        | Symbol x when not (representable_in_store type_) -> Hashset.add ctx x
        | _ -> super#visit_expr_value ~ctx ~type_ e
    end
  in
  fun ?(set = Hashset.empty ()) e -> addressed_visitor#visit_expr ~ctx:set e

let as_pure_string_literal (e : GExpr.t) =
  match e.value with
  | AddressOf
      {
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
  | _ -> Error.unexpected ("not a pure string literal: " ^ GExpr.show e)

(** Gillian-C utils for compilation*)
module Gcu = struct
  include Compcert
  module Vt = Cgil_lib.ValueTranslation
end

let sanitize_symbol s = Str.global_replace (Str.regexp "[:]") "_" s
let as_gil_variable e = GExpr.as_symbol e |> sanitize_symbol

let compile_location (loc : Goto_lib.Location.t) =
  match loc.source with
  | None -> Location.none
  | Some source ->
      let pos_line = Option.value ~default:0 loc.line in
      let pos_column = Option.value ~default:0 loc.col in
      let loc_start = Location.{ pos_line; pos_column } in
      let loc_end = Location.{ pos_line; pos_column = pos_column + 2 } in
      Location.{ loc_source = source; loc_start; loc_end }

let find_main symtab = Hashtbl.find symtab "main"

module Body_item = struct
  type t = Annot.t * string option * string Cmd.t

  let make ?loop ?label ?loc ?id cmd : t =
    let annot = Annot.make ?origin_loc:loc ?origin_id:id ?loop_info:loop () in
    (annot, label, cmd)
end

let call_for_binop
    ~(ctx : Ctx.t)
    ~(lty : IntType.t)
    (binop : Ops.Binary.t)
    e1
    e2 =
  (* For now, we assume we're on archi64 exactly, then we'll figure out a bit more *)
  assert (Machine_model.equal Machine_model.archi64 ctx.machine);
  let internal_function =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open! Kconstants.Comp_functions in
    match binop with
    | Notequal -> (
        match lty with
        | I_int | I_char -> cmp_eq
        | I_bool -> cmpu_eq
        | I_size_t -> cmplu_eq
        | I_ssize_t -> cmplu_eq)
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
  (call, Expr.PVar gvar)

let is_assume_call f = String.equal (GExpr.as_symbol f) "__CPROVER_assume"
let is_assert_call f = String.equal (GExpr.as_symbol f) "__CPROVER_assert"
let is_nondet_call f = String.equal (GExpr.as_symbol f) "__nondet"

let assume_type ~ctx (type_ : GType.t) (lvar : string) =
  let llvar = Expr.LVar lvar in
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
  | StructTag tag | UnionTag tag | Struct { tag; _ } | Union { tag; _ } ->
      (* TODO: Add a signal here for something unhandled! *)
      let fail = Cmd.Fail ("unhandled_nondet", [ Expr.Lit (String tag) ]) in
      [ fail ]
  | _ ->
      let fail =
        Cmd.Fail ("unhandled_nondet", [ Expr.Lit (String "Something else") ])
      in
      [ fail ]

let nondet_expr ~ctx ~add_annot ~type_ () =
  let is_symbolic_exec =
    let open Gillian.Utils in
    ExecMode.symbolic_exec !Config.current_exec_mode
  in
  if not is_symbolic_exec then
    Error.user_error
      "Looks like you're compiling some nondet variables, but you're not in \
       symbolic execution mode"
  else
    let hash_x = Ctx.fresh_lv ctx in
    let spec_var_cmd = Cmd.Logic (LCmd.SpecVar [ hash_x ]) in
    let cmds = assume_type ~ctx type_ hash_x in
    let cmds = add_annot (spec_var_cmd :: cmds) in
    (cmds, Expr.LVar hash_x)

let compile_cast ~ctx ~(from : GType.t) ~(into : GType.t) e =
  match (from, into) with
  | Bool, CInteger (I_bool | I_char | I_int) ->
      let temp = Ctx.fresh_v ctx in
      let value_of_bool =
        Expr.Lit (String Cgil_lib.CConstants.Internal_Functions.val_of_bool)
      in
      let call = Cmd.Call (temp, value_of_bool, [ e ], None, None) in
      (call, Expr.PVar temp)
  | _ ->
      Error.unhandled
        (Printf.sprintf "Cannot perform cast yet from %s into %s"
           (GType.show from) (GType.show into))

let rec compile_expr ~(ctx : Ctx.t) (expr : GExpr.t) : Body_item.t list * Expr.t
    =
  let compile_expr = compile_expr ~ctx in
  let loc = compile_location expr.location in
  let id = expr.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot = List.map b in
  match expr.value with
  | Symbol s -> ([], PVar (sanitize_symbol s))
  | BoolConstant b -> ([], Lit (Bool b))
  | CBoolConstant b ->
      let i = if b then Gcu.Camlcoq.Z.one else Gcu.Camlcoq.Z.zero in
      let lit = Gcu.Vt.gil_of_compcert (Gcu.Values.Vint i) in
      ([], Lit lit)
  | IntConstant z ->
      let int_ty = Goto_lib.Type.as_int_type expr.type_ in
      let cz = Gcu.Vt.z_of_int z in
      let ccert_value =
        let open Gcu.Values in
        match int_ty with
        | I_int | I_char -> Vint cz
        | I_size_t -> (
            match ctx.machine.pointer_width with
            | 32 -> Vint cz
            | 64 -> Vlong cz
            | _ ->
                Error.unhandled
                  "Gillian only handles archi 32 and archi 64 for now")
        | I_ssize_t ->
            Error.unhandled "Lookup in compcert what kind of value ssize_t is"
        | I_bool -> Error.unexpected "IntConstant with type I_bool"
      in
      let lit = Gcu.Vt.gil_of_compcert ccert_value in
      ([], Lit lit)
  | BinOp { op; lhs; rhs } ->
      let s1, e1 = compile_expr lhs in
      let s2, e2 = compile_expr rhs in
      let lty = lhs.type_ |> GType.as_int_type in
      let call, e = call_for_binop ~ctx ~lty op e1 e2 in
      (s1 @ s2 @ [ b call ], e)
  | UnOp { op; e } ->
      let s, e = compile_expr e in
      let ne =
        match op with
        | Not -> Expr.Infix.not e
        | _ -> Error.unhandled ("Unary operator: " ^ Ops.Unary.show op)
      in
      (s, ne)
      (* TODO: the following is not correct and might crash, I'm assuming the symbol name is the function name.
         This could be a function pointer, in which case, it should go through the
         global env thing *)
  | FunctionCall { func; args } when is_assert_call func ->
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
      let pre, to_assert = compile_expr to_assert in
      let cast_call, to_assert =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Cgil_lib.CConstants.Internal_Functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assert ], None, None)
          in
          ([ b call ], Expr.PVar temp)
        else ([], to_assert)
      in
      let f =
        match Formula.lift_logic_expr to_assert with
        | None ->
            Error.code_error
              (Fmt.str "obtained weird expression that cannot be asserted: %a"
                 Expr.pp to_assert)
        | Some (f, _) -> f
      in
      (pre @ cast_call @ [ b (Logic (Assert f)) ], Lit Null)
  | FunctionCall { func; args } when is_assume_call func ->
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
      let pre, to_assume = compile_expr to_assume in
      let cast_call, to_assume =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Cgil_lib.CConstants.Internal_Functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assume ], None, None)
          in
          ([ b call ], Expr.PVar temp)
        else ([], to_assume)
      in
      let f =
        match Formula.lift_logic_expr to_assume with
        | None ->
            Error.code_error
              (Fmt.str "obtained weird expression that cannot be assumed: %a"
                 Expr.pp to_assume)
        | Some (f, _) -> f
      in
      (pre @ cast_call @ [ b (Logic (Assume f)) ], Lit Null)
  | FunctionCall { func; args } when is_nondet_call func ->
      let () =
        match args with
        | [] -> ()
        | _ ->
            Error.user_error "You should not be passing arguments to __nondet!"
      in
      nondet_expr ~ctx ~add_annot ~type_:expr.type_ ()
  | Nondet -> nondet_expr ~ctx ~add_annot ~type_:expr.type_ ()
  | FunctionCall { func; args } ->
      let fname = GExpr.as_symbol func in

      let ss, args = List.map compile_expr args |> List.split in
      let pre_cmd = List.concat ss in
      let ret_var = Ctx.fresh_v ctx in
      let gil_call = Cmd.Call (ret_var, Lit (String fname), args, None, None) in
      (pre_cmd @ [ b gil_call ], PVar ret_var)
  | TypeCast to_cast ->
      let pre, to_cast_e = compile_expr to_cast in
      let perform_cast, out =
        compile_cast ~ctx ~from:to_cast.type_ ~into:expr.type_ to_cast_e
      in
      (pre @ [ b perform_cast ], out)
  | _ -> Error.unhandled ("Cannot compile expr yet: " ^ GExpr.show expr)

let rec compile_statement ~ctx (stmt : Stmt.t) : Body_item.t list =
  let compile_statement = compile_statement ~ctx in
  let compile_expr = compile_expr ~ctx in
  let loc = compile_location stmt.location in
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
      let pre, e = compile_expr cond in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assume f)) ]
  | Assert { cond } ->
      let pre, e = compile_expr cond in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assert f)) ]
  | Return e ->
      let s, e =
        match e with
        | Some e -> compile_expr e
        | None -> ([], Lit Null)
      in
      let variable = Utils.Names.return_variable in
      s @ add_annot [ Assignment (variable, e); ReturnNormal ]
  | Decl { lhs; value } ->
      let lhs = as_gil_variable lhs in
      let s, v =
        match value with
        | Some e -> compile_expr e
        | None -> ([], Lit Undefined)
      in
      s @ [ b (Assignment (lhs, v)) ]
  | Assign { lhs; rhs } ->
      let lhs = as_gil_variable lhs in
      let s, v = compile_expr rhs in
      s @ [ b (Assignment (lhs, v)) ]
  | Expression e ->
      let s, _ = compile_expr e in
      s
  | Switch _ -> Error.unhandled "switch statement"

let compile_function ~ctx (func : Program.Func.t) : (Annot.t, string) Proc.t =
  let ctx = Ctx.with_new_generators ctx in
  let f_loc = compile_location func.location in
  let body =
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
  let proc_params =
    List.map
      (fun x ->
        match x.Param.identifier with
        | None -> Ctx.fresh_v ctx
        | Some s -> s)
      func.params
  in
  let proc_spec = None in
  let proc_body = Array.of_list (compile_statement ~ctx body) in
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
  Printf.printf "%d types; %d global variables; %d functions\n"
    (Hashtbl.length program.types)
    (Hashtbl.length program.vars)
    (Hashtbl.length program.funs);
  let to_compile = Queue.create () in
  Queue.push "main" to_compile;
  let gil_prog = Prog.create () in
  let gil_prog =
    Program.fold_functions
      (fun _ f prog -> Prog.add_proc prog (compile_function ~ctx:context f))
      program gil_prog
  in
  assert (Machine_model.equal context.machine Machine_model.archi64);
  let imports =
    Kconstants.Imports.imports
    @ Cgil_lib.CConstants.Imports.imports Arch64
        !Gillian.Utils.Config.current_exec_mode
        Unallocated_functions
  in
  { gil_prog with imports }
