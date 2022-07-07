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

let assert_unhandled ~feature args =
  let open Stats.Unhandled in
  let () = signal feature in
  let feature_string = Expr.string (show_feature feature) in
  Cmd.Fail ("unhandled", feature_string :: args)

module ValRepr = struct
  type t = ByCopy of { ptr : Expr.t; type_ : GType.t } | ByValue of Expr.t
  [@@deriving show { with_path = false }]

  let as_value ?(error = Error.unexpected) ~msg = function
    | ByValue e -> e
    | ByCopy _ -> error ("Expected ByValue expressions for " ^ msg)

  let as_value_or_unhandled ~feature e =
    match e with
    | ByValue e -> Cs.return e
    | ByCopy _ ->
        let cmd = assert_unhandled ~feature [] in
        Cs.return ~app:[ cmd ] (Expr.Lit Nono)
end

let call_for_binop
    ~(ctx : Ctx.t)
    ~(lty : GType.t)
    (binop : Ops.Binary.t)
    (e1 : ValRepr.t)
    (e2 : ValRepr.t) : Expr.t Cs.with_cmds =
  (* For now, we assume we're on archi64 exactly,
     then we'll figure out a bit more.
     This is for size_t and pointer operations. *)
  assert (Machine_model.equal Machine_model.archi64 ctx.machine);
  let internal_function =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open Kconstants.Comp_functions in
    let open Kconstants.Binop_functions in
    match binop with
    | Equal -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_eq
        | CInteger I_bool -> cmpu_eq
        | CInteger I_size_t -> cmplu_eq
        | CInteger I_ssize_t -> cmpl_eq
        | _ -> Error.unhandled "binop == for type " ^ GType.show lty)
    | Notequal -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_ne
        | CInteger I_bool -> cmpu_ne
        | CInteger I_size_t -> cmplu_ne
        | CInteger I_ssize_t -> cmpl_ne
        | _ -> Error.unhandled "binop != for type " ^ GType.show lty)
    | Le -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_le
        | CInteger I_bool -> cmpu_le
        | CInteger I_size_t -> cmplu_le
        | CInteger I_ssize_t -> cmpl_le
        | _ -> Error.unhandled "binop <= for type " ^ GType.show lty)
    | Lt -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_lt
        | CInteger I_bool -> cmpu_lt
        | CInteger I_size_t -> cmplu_lt
        | CInteger I_ssize_t -> cmpl_lt
        | _ -> Error.unhandled "binop < for type " ^ GType.show lty)
    | Gt -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_gt
        | CInteger I_bool -> cmpu_gt
        | CInteger I_size_t -> cmplu_gt
        | CInteger I_ssize_t -> cmpl_gt
        | _ -> Error.unhandled "binop < for type " ^ GType.show lty)
    | Ge -> (
        match lty with
        | CInteger (I_int | I_char) -> cmp_ge
        | CInteger I_bool -> cmpu_ge
        | CInteger I_size_t -> cmplu_ge
        | CInteger I_ssize_t -> cmpl_ge
        | _ -> Error.unhandled "binop >= for type " ^ GType.show lty)
    | Plus -> (
        match lty with
        | CInteger (I_int | I_char) -> add
        | CInteger I_bool -> add
        | CInteger I_size_t -> addl
        | CInteger I_ssize_t -> addl
        | _ -> Error.unhandled "binop + for type " ^ GType.show lty)
    | _ -> Error.unhandled (Fmt.str "binary operator: %a" Ops.Binary.pp binop)
  in
  let e1 = ValRepr.as_value ~msg:"Binary operand" e1 in
  let e2 = ValRepr.as_value ~msg:"Binary operand" e2 in
  let gvar = Ctx.fresh_v ctx in
  let call =
    Cmd.Call (gvar, Lit (String internal_function), [ e1; e2 ], None, None)
  in
  Cs.return ~app:[ call ] (Expr.PVar gvar)

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
      let fail =
        assert_unhandled ~feature:CompositNondet [ Lit (String tag) ]
      in
      [ fail ]
  | _ ->
      let ty_str = GType.show type_ in
      let fail =
        assert_unhandled ~feature:CompositNondet [ Lit (String ty_str) ]
      in
      [ fail ]

let nondet_expr ~ctx ~add_annot ~type_ () : ValRepr.t Cs.with_body =
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
      if Ctx.is_zst_access ctx type_ then Cs.return (ValRepr.ByValue (Lit Null))
      else if Ctx.representable_in_store ctx type_ then
        let* hash_x = Cs.return (Ctx.fresh_lv ctx) in
        let* () = Cs.unit [ Cmd.Logic (LCmd.SpecVar [ hash_x ]) ] in
        let+ () = assume_type ~ctx type_ hash_x in
        ValRepr.ByValue (LVar hash_x)
      else
        let fail_cmd =
          assert_unhandled ~feature:CompositNondet
            [ Expr.string (GType.show type_) ]
        in
        let+ () = Cs.unit [ fail_cmd ] in
        ValRepr.ByCopy { ptr = Lit Nono; type_ }
    in
    Cs.map_l add_annot res

let compile_cast ~(ctx : Ctx.t) ~(from : GType.t) ~(into : GType.t) e :
    ValRepr.t Cs.with_cmds =
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
  let e = ValRepr.as_value ~msg:"TypeCast operand" e in
  let temp = Ctx.fresh_v ctx in
  let call = Cmd.Call (temp, function_name, [ e ], None, None) in
  Cs.return ~app:[ call ] (ValRepr.ByValue (PVar temp))

let rec compile_expr ~(ctx : Ctx.t) (expr : GExpr.t) : ValRepr.t Cs.with_body =
  let open Cs.Syntax in
  let by_value ?app t = Cs.return ?app (ValRepr.ByValue t) in
  let by_copy ?app ptr type_ = Cs.return ?app (ValRepr.ByCopy { ptr; type_ }) in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location expr.location in
  let id = expr.location.origin_id in
  let b = Body_item.make ~loc ~id in
  if Ctx.is_zst_access ctx expr.type_ then by_value (Lit Null)
  else
    match expr.value with
    | Symbol _ | Dereference _ | Index _ | Member _ -> (
        let* access = Lvalue.as_access ~ctx ~read:true expr in
        match access with
        | ZST -> by_value (Lit Null)
        | Direct x -> by_value (Expr.PVar x)
        | InMemoryScalar { loaded = Some e; _ } -> by_value e
        | InMemoryScalar { loaded = None; ptr } ->
            let* var = Memory.load_scalar ~ctx ptr expr.type_ |> Cs.map_l b in
            by_value (PVar var)
        | InMemoryComposit { ptr; type_ } -> by_copy ptr type_)
    | AddressOf x -> (
        let* access = Lvalue.as_access ~ctx ~read:true x in
        match access with
        | ZST ->
            by_value
              (Expr.EList [ Lit (String "dangling"); Lit (String "pointer") ])
        | InMemoryScalar { ptr; _ } | InMemoryComposit { ptr; _ } ->
            by_value ptr
        | Direct x -> Error.code_error ("address of direct access to " ^ x))
    | BoolConstant b -> by_value (Lit (Bool b))
    | CBoolConstant b ->
        let i = if b then Gcu.Camlcoq.Z.one else Gcu.Camlcoq.Z.zero in
        let lit = Gcu.Vt.gil_of_compcert (Gcu.Values.Vint i) in
        by_value (Lit lit)
    | PointerConstant b ->
        let i = Gcu.Camlcoq.Z.of_sint b in
        let v =
          match ctx.machine.pointer_width with
          | 32 -> Gcu.Values.Vint i
          | 64 -> Gcu.Values.Vlong i
          | _ -> Error.unhandled "PointerConstant - unknown archi"
        in
        let lit = Gcu.Vt.gil_of_compcert v in
        by_value (Lit lit)
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
        by_value (Lit lit)
    | BinOp { op; lhs; rhs } ->
        let* e1 = compile_expr lhs in
        let* e2 = compile_expr rhs in
        let lty = lhs.type_ in
        let+ e = call_for_binop ~ctx ~lty op e1 e2 |> Cs.map_l b in
        ValRepr.ByValue e
    | UnOp { op; e } -> (
        let* e = compile_expr e in
        let e = ValRepr.as_value ~msg:"Unary operand" e in
        match op with
        | Not -> by_value (Expr.Infix.not e)
        | _ -> Error.unhandled ("Unary operator: " ^ Ops.Unary.show op))
    | Nondet -> nondet_expr ~ctx ~add_annot:b ~type_:expr.type_ ()
    | TypeCast to_cast ->
        let* to_cast_e = compile_expr to_cast in
        compile_cast ~ctx ~from:to_cast.type_ ~into:expr.type_ to_cast_e
        |> Cs.map_l b
    | Assign { lhs; rhs } -> compile_assign ~ctx ~lhs ~rhs ~annot:b
    | FunctionCall { func; args } -> compile_call ~ctx ~add_annot:b func args
    | ByteExtract _ ->
        let cmd = assert_unhandled ~feature:ByteExtract [] in
        if Ctx.representable_in_store ctx expr.type_ then
          by_value ~app:[ b cmd ] (Lit Nono)
        else by_copy ~app:[ b cmd ] (Lit Nono) expr.type_
    | Struct _ ->
        let cmd = assert_unhandled ~feature:StructConstant [] in
        by_copy ~app:[ b cmd ] (Lit Nono) expr.type_
    | _ -> Error.unhandled ("Cannot compile expr yet: " ^ GExpr.show expr)

and compile_call ~ctx ~add_annot:b (func : GExpr.t) (args : GExpr.t list) =
  let open Cs.Syntax in
  let return_type =
    match func.type_ with
    | Code { return_type; _ } -> return_type
    | _ ->
        Error.unexpected
          "function call where the function doesn't have type code"
  in
  let location = func.location in
  let by_value ?app e = Cs.return ?app (ValRepr.ByValue e) in
  let by_copy ?app ptr type_ = Cs.return ?app (ValRepr.ByCopy { ptr; type_ }) in
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
      let to_assume =
        ValRepr.as_value ~msg:"__CPROVER_assume operand" to_assume
      in
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
      by_value ~app:[ b (Logic (Assume f)) ] (Lit Null)
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
      let to_assert =
        ValRepr.as_value ~msg:"__CPROVER_assert operand" to_assert
      in
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
      by_value ~app:[ b (Logic (Assert f)) ] (Expr.Lit Null)
  | _ ->
      let fname = GExpr.as_symbol func in
      let* args = Cs.many (compile_expr ~ctx) args in
      let* args =
        Cs.many (ValRepr.as_value_or_unhandled ~feature:CallArgumentByCopy) args
        |> Cs.map_l b
      in
      (* If the return value has to be passed by copy,
         we add an argument before every other in which the return value will be stored.
         It's a trick of CompCert.*)
      if Ctx.representable_in_store ctx return_type then
        let ret_var = Ctx.fresh_v ctx in
        let gil_call =
          Cmd.Call (ret_var, Lit (String fname), args, None, None)
        in
        by_value ~app:[ b gil_call ] (Expr.PVar ret_var)
      else
        let* temp_arg =
          Memory.alloc_temp ~ctx ~location return_type |> Cs.map_l b
        in
        let unused_temp = Ctx.fresh_v ctx in
        let gil_call =
          Cmd.Call
            (unused_temp, Lit (String fname), temp_arg :: args, None, None)
        in
        by_copy ~app:[ b gil_call ] temp_arg return_type

and compile_assign ~ctx ~annot ~lhs ~rhs =
  let v, pre1 = compile_expr ~ctx rhs in
  let access, pre2 = Lvalue.as_access ~ctx ~read:false lhs in
  let write =
    match (access, v) with
    | ZST, _ ->
        [ annot Skip ]
        (* We need a command in case we try want to add a label *)
    | Direct x, ByValue v -> [ annot (Cmd.Assignment (x, v)) ]
    | InMemoryScalar { ptr; _ }, ByValue v ->
        [ annot (Memory.store_scalar ~ctx ptr v lhs.type_) ]
    | ( InMemoryComposit { ptr = ptr_access; type_ = type_access },
        ByCopy { ptr = ptr_v; type_ = type_v } ) ->
        if not (GType.equal type_v type_access) then
          Error.unexpected "ByCopy assignment with different types on each side"
        else
          let temp = Ctx.fresh_v ctx in
          let size = Ctx.size_of ctx type_access in
          let memcpy = Cgil_lib.CConstants.Internal_Functions.ef_memcpy in
          (* TODO: emit a signal that alignment check is not performed correctly *)
          let copy_cmd =
            Cmd.Call
              ( temp,
                Lit (String memcpy),
                [ Expr.int size; Expr.zero_i; ptr_access; ptr_v ],
                None,
                None )
          in
          [ annot copy_cmd ]
    | _ ->
        Error.code_error
          (Fmt.str
             "Invalid assignement, wrong mix of ByCopy and Direct assignments:\n\
              %a = %a.\n\
              Originally: %a = %a\n\n\
             \              Left type is %a and right type is %a"
             Lvalue.pp_access access ValRepr.pp v GExpr.pp lhs GExpr.pp rhs
             GType.pp lhs.type_ GType.pp rhs.type_)
  in
  (v, pre1 @ pre2 @ write)

let rec compile_statement ~ctx (stmt : Stmt.t) : Body_item.t list =
  let compile_statement = compile_statement ~ctx in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location stmt.location in
  let id = stmt.location.origin_id in
  let b = Body_item.make ~loc ~id in
  let add_annot x = List.map b x in
  let set_first_label_opt label stmts =
    match stmts with
    | [] -> [ b ?label Skip ]
    | (a, None, cmd) :: r -> (a, label, cmd) :: r
    | (_, Some _, _) :: _ -> Error.code_error "First label is already set!!"
  in
  let set_first_label label stmts = set_first_label_opt (Some label) stmts in
  match stmt.body with
  | Skip -> [ b Skip ]
  | Block ss -> List.concat_map compile_statement ss
  | Label (s, ss) -> List.concat_map compile_statement ss |> set_first_label s
  | Goto lab -> [ b (Goto lab) ]
  | Assume { cond } ->
      let e, pre = compile_expr cond in
      let e = ValRepr.as_value ~msg:"Assume operand" e in
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
      let e = ValRepr.as_value ~msg:"Assert operand" e in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assert f)) ]
  | Return e ->
      let e, s =
        match e with
        | Some e ->
            let open Cs.Syntax in
            let* e = compile_expr e in
            (* Return_by_copy should copy the value in the last first of the function,
               which contains the ret value.
               For now, I don't have that, I need to also change how functions are compiled *)
            ValRepr.as_value_or_unhandled ~feature:ReturnByCopy e |> Cs.map_l b
        | None -> (Lit Null, [])
      in
      let variable = Utils.Names.return_variable in
      s @ add_annot [ Assignment (variable, e); ReturnNormal ]
  | Decl { lhs; value } ->
      let ty = lhs.type_ in
      let lhs = GExpr.as_symbol lhs in
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
          | Some _ -> [ b (assert_unhandled ~feature:DeclCompositValue []) ]
        in
        [ b alloc_cmd; b assign ] @ write
      else if Ctx.in_memory ctx lhs then
        let ptr, action_cmd = Memory.alloc_ptr ~ctx ty in
        let assign = Cmd.Assignment (lhs, ptr) in
        let write =
          match value with
          | None -> []
          | Some e ->
              let v, pre = compile_expr e in
              let v =
                ValRepr.as_value ~error:Error.code_error
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
              let e, s = compile_expr e in
              let e =
                ValRepr.as_value ~error:Error.code_error ~msg:"in memory scalar"
                  e
              in
              (e, s)
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
            match (access, v) with
            | ZST, _ -> []
            | Direct x, ByValue v -> [ b (Cmd.Assignment (x, v)) ]
            | InMemoryScalar { ptr; _ }, ByValue v ->
                [ b (Memory.store_scalar ~ctx ptr v lvalue.type_) ]
            | InMemoryComposit _, ByCopy _ ->
                Error.unhandled "ByCopy function call destination"
            | _ ->
                Error.code_error
                  (Fmt.str
                     "Wrong mix of access and value kind for function call:\n\
                      %a = %a" Lvalue.pp_access access ValRepr.pp v)
          in
          pre1 @ pre2 @ write)
  | Switch { control; cases; default } ->
      let end_lab = Ctx.fresh_lab ctx in
      let goto_end = Cmd.Goto end_lab in
      let next_lab = ref None in
      let control_ty = control.type_ in
      let control, control_s = compile_expr control in
      let rec compile_cases ?(acc = []) = function
        | [] -> acc
        | case :: rest ->
            let cur_lab = !next_lab in
            let nlab = Ctx.fresh_lab ctx in
            let () = next_lab := Some nlab in
            let guard = case.Stmt.case in
            let guard_e, guard_s = compile_expr guard in
            let equal_v, comparison_calls =
              call_for_binop ~ctx ~lty:control_ty Equal control guard_e
            in
            let comparison_calls =
              List.map
                (Body_item.make_hloc ~loc:guard.location)
                comparison_calls
            in
            let block = compile_statement case.sw_body in
            let block_lab, block = Body_item.get_or_set_fresh_lab ~ctx block in
            let goto_block = Cmd.GuardedGoto (equal_v, block_lab, nlab) in
            let total_block =
              set_first_label_opt cur_lab
                (guard_s @ comparison_calls @ block
                @ [ b goto_block; b goto_end ])
            in
            let acc = total_block @ acc in
            compile_cases ~acc rest
      in
      let compiled_cases = compile_cases cases in
      let default_block =
        match default with
        | None -> [ b ?label:!next_lab Skip ]
        | Some default ->
            set_first_label_opt !next_lab (compile_statement default)
      in
      let end_ = [ b ~label:end_lab Skip ] in
      control_s @ compiled_cases @ default_block @ end_

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
          let v =
            ValRepr.as_value ~error:Error.unhandled
              ~msg:"global variable init value" v
          in
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

  (* Fmt.pr "FUNCTION %s:\n%a@?\n\n" func.symbol Stmt.pp body; *)
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
