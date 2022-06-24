open Gil_syntax
module GExpr = Goto_lib.Expr
module GType = Goto_lib.Type

exception CompilationError of string

(* TODO: Extract that and pass a context to every compile function
   with the current current counter of variables etc... *)
let temp_var =
  let id = ref 0 in
  fun () ->
    let c = !id in
    let ret = "temp__" ^ string_of_int c in
    incr id;
    ret

let temp_lvar =
  let id = ref 0 in
  fun () ->
    let c = !id in
    let ret = "#lvar_" ^ string_of_int c in
    incr id;
    ret

let () =
  Printexc.register_printer (function
    | CompilationError msg ->
        Some ("Error happened while compiling from GotoC to Gil:\n\n" ^ msg)
    | _ -> None)

let failwith msg = raise (CompilationError msg)

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
  | _ -> failwith ("not a pure string literal: " ^ GExpr.show e)

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

let call_for_binop ~(lty : IntType.t) (binop : Ops.Binary.t) e1 e2 =
  (* For now, we assume we're on archi64 exactly, then we'll figure out a bit more *)
  assert (Machine_model.equal Machine_model.archi64 !Kconfig.machine_model);
  let internal_function =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open! Kconstants.Comp_functions in
    match binop with
    | Le -> (
        match lty with
        | I_int | I_char -> cmp_le
        | I_bool -> cmpu_le
        | I_size_t -> cmpl_le
        | I_ssize_t -> cmplu_le)
    | Ge -> (
        match lty with
        | I_int | I_char -> cmp_ge
        | I_bool -> cmpu_ge
        | I_size_t -> cmpl_ge
        | I_ssize_t -> cmplu_ge)
    | _ -> failwith "Unhandled binary operator: " ^ Ops.Binary.show binop
  in
  let gvar = temp_var () in
  let call =
    Cmd.Call (gvar, Lit (String internal_function), [ e1; e2 ], None, None)
  in
  (call, Expr.PVar gvar)

let is_assume_call f = String.equal (GExpr.as_symbol f) "__CPROVER_assume"
let is_assert_call f = String.equal (GExpr.as_symbol f) "__CPROVER_assert"
let is_nondet_call f = String.equal (GExpr.as_symbol f) "__nondet"

let assume_type (type_ : GType.t) (lvar : string) =
  let llvar = Expr.LVar lvar in
  match type_ with
  | CInteger ty ->
      let open Cgil_lib.CConstants.VTypes in
      let str_constr =
        match ty with
        | I_int -> int_type
        | _ -> failwith ("unhandled nondet for int type: " ^ IntType.show ty)
      in
      let value = temp_lvar () in
      let e_value = Expr.LVar value in
      let assume_list =
        let f =
          Formula.Eq (llvar, EList [ Lit (String str_constr); e_value ])
        in
        Cmd.Logic (Assume f)
      in
      let assume_int = Cmd.Logic (AssumeType (value, IntType)) in
      [ assume_list; assume_int ]
  | _ -> failwith ("unhandled nondet for type: " ^ GType.show type_)

let rec compile_expr (expr : GExpr.t) : Body_item.t list * Expr.t =
  let loc = compile_location expr.location in
  let id = expr.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot = List.map b in
  match expr.value with
  | Symbol s -> ([], PVar (sanitize_symbol s))
  | BoolConstant b ->
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
            match !Kconfig.machine_model.pointer_width with
            | 32 -> Vint cz
            | 64 -> Vlong cz
            | _ -> failwith "Gillian only handles archi 32 and archi 64 for now"
            )
        | I_ssize_t ->
            failwith "Lookup in compcert what kind of value ssize_t is"
        | I_bool -> failwith "IntConstant with type I_bool"
      in
      let lit = Gcu.Vt.gil_of_compcert ccert_value in
      ([], Lit lit)
  | BinOp { op; lhs; rhs } ->
      let s1, e1 = compile_expr lhs in
      let s2, e2 = compile_expr rhs in
      let lty = lhs.type_ |> GType.as_int_type in
      let call, e = call_for_binop ~lty op e1 e2 in
      (s1 @ s2 @ [ b call ], e)
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
        | _ -> failwith "__CPROVER_assert not given 2 params"
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
          let temp = temp_var () in
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
            failwith
              (Fmt.str "obtained weird expression that cannot be asserted: %a"
                 Expr.pp to_assert)
        | Some (f, _) -> f
      in
      (pre @ cast_call @ [ b (Logic (Assert f)) ], Lit Null)
  | FunctionCall { func; args } when is_assume_call func ->
      let to_assume =
        match args with
        | [ to_assume ] -> to_assume
        | _ -> failwith "__CPROVER_assume not given 1 params"
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
          let temp = temp_var () in
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
            failwith
              (Fmt.str "obtained weird expression that cannot be assumed: %a"
                 Expr.pp to_assume)
        | Some (f, _) -> f
      in
      (pre @ cast_call @ [ b (Logic (Assume f)) ], Lit Null)
  | FunctionCall { func; args } when is_nondet_call func ->
      let is_symbolic_exec =
        let open Gillian.Utils in
        ExecMode.symbolic_exec !Config.current_exec_mode
      in
      if not is_symbolic_exec then
        failwith
          "Looks like you're compiling some nondet variables, but you're not \
           in concrete execution mode"
      else
        let () =
          match args with
          | [] -> ()
          | _ -> failwith "You should not be passing arguments to __nondet!"
        in
        let hash_x = temp_lvar () in
        let spec_var_cmd = Cmd.Logic (LCmd.SpecVar [ hash_x ]) in
        let cmds = assume_type expr.type_ hash_x in
        let cmds = add_annot (spec_var_cmd :: cmds) in
        (cmds, LVar hash_x)
  | FunctionCall { func; args } ->
      let fname = GExpr.as_symbol func in

      let ss, args = List.map compile_expr args |> List.split in
      let pre_cmd = List.concat ss in
      let ret_var = temp_var () in
      let gil_call = Cmd.Call (ret_var, Lit (String fname), args, None, None) in
      (pre_cmd @ [ b gil_call ], PVar ret_var)
  | _ -> failwith ("Cannot compile expr yet: " ^ GExpr.show expr)

let rec compile_statement (stmt : Stmt.t) : Body_item.t list =
  let loc = compile_location stmt.location in
  let id = stmt.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot x = List.map b x in
  let set_first_label label stmts =
    match stmts with
    | [] -> [ b ~label Skip ]
    | (a, None, cmd) :: r -> (a, Some label, cmd) :: r
    | (_, Some _, _) :: _ -> failwith "First label is already set!!"
  in
  match stmt.body with
  | Skip -> [ b Skip ]
  | Block ss -> List.concat_map compile_statement ss
  | Label (s, ss) -> List.concat_map compile_statement ss |> set_first_label s
  | Goto lab -> [ b (Goto lab) ]
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
(* | _ -> failwith "Cannot compile statement yet" *)

let compile_function ~(params : Param.t list) (sym : Gsymbol.t) :
    (Annot.t, string) Proc.t =
  if sym.is_volatile || sym.is_weak then
    failwith "Cannot handled volatile or weak data yet";
  let f_loc = compile_location sym.location in
  let proc_params = List.map (fun x -> Option.get x.Param.identifier) params in
  let proc_spec = None in
  let stmt =
    match sym.value with
    | Stmt s -> s
    | _ -> failwith "symbol value of function is not a statement"
  in
  let proc_body = Array.of_list (compile_statement stmt) in
  Proc.
    {
      proc_name = sym.name;
      proc_source_path = Some f_loc.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
    }

let add_symbol prog (symbol : Gsymbol.t) =
  match symbol.type_ with
  | Code { params; _ } -> Prog.add_proc prog (compile_function ~params symbol)
  | _ ->
      failwith
        ("Cannot compile other symbols than functions for now: " ^ symbol.name)

let compile (symtab : Gsymtab.t) : (Annot.t, string) Prog.t =
  let prog = Prog.create () in
  let prog =
    Hashtbl.fold
      (fun _ (s : Gsymbol.t) p -> if s.is_file_local then p else add_symbol p s)
      symtab prog
  in
  assert (Machine_model.equal !Kconfig.machine_model Machine_model.archi64);

  let imports =
    Kconstants.Imports.imports
    @ Cgil_lib.CConstants.Imports.imports Arch64
        !Gillian.Utils.Config.current_exec_mode
        Unallocated_functions
  in
  { prog with imports }
