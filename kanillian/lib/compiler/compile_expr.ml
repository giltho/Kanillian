open Helpers
open Gil_syntax
module GType = Goto_lib.Type
module GExpr = Goto_lib.Expr

type access =
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | InMemoryComposit of { ptr : Expr.t; type_ : GType.t }
      (** For copy, we just need the size, not the type.
          However, in the future, we might copy fields
          one-by-one to preserve the correct semantics of C *)
  | Direct of string
  | ZST
[@@deriving show { with_path = false }]

(* This file is a mess of mutually-recursive functions *)

let compile_binop
    ~(ctx : Ctx.t)
    ~(lty : GType.t)
    ~(rty : GType.t)
    (binop : Ops.Binary.t)
    (e1 : Val_repr.t)
    (e2 : Val_repr.t) : Expr.t Cs.with_cmds =
  (* For now, we assume we're on archi64 exactly,
     then we'll figure out a bit more.
     This is for size_t and pointer operations. *)
  assert (Machine_model.equal Machine_model.archi64 ctx.machine);
  let compile_with =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open Kconstants.Comp_functions in
    let open Kconstants.Binop_functions in
    match binop with
    | Equal -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_eq
        | CInteger I_bool -> `Proc cmpu_eq
        | CInteger I_size_t -> `Proc cmplu_eq
        | CInteger I_ssize_t -> `Proc cmpl_eq
        | _ -> Error.unhandled (Fmt.str "binop == for type %a" GType.pp lty))
    | Notequal -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_ne
        | CInteger I_bool -> `Proc cmpu_ne
        | CInteger I_size_t -> `Proc cmplu_ne
        | CInteger I_ssize_t -> `Proc cmpl_ne
        | _ -> Error.unhandled (Fmt.str "binop != for type %a" GType.pp lty))
    | Le -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_le
        | CInteger I_bool -> `Proc cmpu_le
        | CInteger I_size_t -> `Proc cmplu_le
        | CInteger I_ssize_t -> `Proc cmpl_le
        | _ -> Error.unhandled (Fmt.str "binop <= for type %a" GType.pp lty))
    | Lt -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_lt
        | CInteger I_bool -> `Proc cmpu_lt
        | CInteger I_size_t -> `Proc cmplu_lt
        | CInteger I_ssize_t -> `Proc cmpl_lt
        | _ -> Error.unhandled (Fmt.str "binop < for type %a" GType.pp lty))
    | Gt -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_gt
        | CInteger I_bool -> `Proc cmpu_gt
        | CInteger I_size_t -> `Proc cmplu_gt
        | CInteger I_ssize_t -> `Proc cmpl_gt
        | _ -> Error.unhandled (Fmt.str "binop > for type %a" GType.pp lty))
    | Ge -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_ge
        | CInteger I_bool -> `Proc cmpu_ge
        | CInteger I_size_t -> `Proc cmplu_ge
        | CInteger I_ssize_t -> `Proc cmpl_ge
        | _ -> Error.unhandled (Fmt.str "binop >= for type %a" GType.pp lty))
    | Plus -> (
        match (lty, rty) with
        | CInteger I_int, CInteger I_int -> `Proc add
        | CInteger I_bool, CInteger I_bool -> `Proc add
        | CInteger I_size_t, CInteger I_size_t -> `Proc addl
        | CInteger I_ssize_t, CInteger I_ssize_t -> `Proc addl
        (* For pointer addition, I'm not checking that the pointer is not null.
           That's a bug we've caught in the past... *)
        | CInteger (I_size_t | I_ssize_t), Pointer _ ->
            `App (fun num ptr -> Memory.ptr_add_v ptr num)
        | Pointer _, CInteger (I_size_t | I_ssize_t) -> `App Memory.ptr_add_v
        | _ -> Error.unhandled (Fmt.str "binop + for type %a" GType.pp lty))
    | _ -> Error.unhandled (Fmt.str "binary operator: %a" Ops.Binary.pp binop)
  in
  let e1 = Val_repr.as_value ~msg:"Binary operand" e1 in
  let e2 = Val_repr.as_value ~msg:"Binary operand" e2 in
  match compile_with with
  | `Proc internal_function ->
      let gvar = Ctx.fresh_v ctx in
      let call =
        Cmd.Call (gvar, Lit (String internal_function), [ e1; e2 ], None, None)
      in
      Cs.return ~app:[ call ] (Expr.PVar gvar)
  | `App f -> Cs.return (f e1 e2)

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

let nondet_expr ~ctx ~add_annot ~type_ () : Val_repr.t Cs.with_body =
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
      if Ctx.is_zst_access ctx type_ then
        Cs.return (Val_repr.ByValue (Lit Null))
      else if Ctx.representable_in_store ctx type_ then
        let* hash_x = Cs.return (Ctx.fresh_lv ctx) in
        let* () = Cs.unit [ Cmd.Logic (LCmd.SpecVar [ hash_x ]) ] in
        let+ () = assume_type ~ctx type_ hash_x in
        Val_repr.ByValue (LVar hash_x)
      else
        let fail_cmd =
          assert_unhandled ~feature:CompositNondet
            [ Expr.string (GType.show type_) ]
        in
        let+ () = Cs.unit [ fail_cmd ] in
        Val_repr.ByCopy { ptr = Lit Nono; type_ }
    in
    Cs.map_l add_annot res

let compile_cast ~(ctx : Ctx.t) ~(from : GType.t) ~(into : GType.t) e :
    Val_repr.t Cs.with_cmds =
  let cast_with =
    match (from, into) with
    | Bool, CInteger (I_bool | I_char | I_int) ->
        `Proc Cgil_lib.CConstants.Internal_Functions.val_of_bool
    | CInteger I_int, CInteger (I_size_t | I_ssize_t) ->
        assert (ctx.machine.pointer_width == 64);
        `Proc Cgil_lib.CConstants.UnOp_Functions.longofint
    | CInteger I_int, Unsignedbv { width } when ctx.machine.int_width == width
      -> `Proc Kconstants.Cast_functions.unsign_int
    | CInteger I_ssize_t, CInteger I_size_t when ctx.machine.pointer_width == 64
      -> `Proc Kconstants.Cast_functions.unsign_long
    | CInteger I_ssize_t, CInteger I_size_t when ctx.machine.pointer_width == 32
      -> `Proc Kconstants.Cast_functions.unsign_int
    | Pointer _, Pointer _ -> `Nop
    | _ ->
        Error.unhandled
          (Printf.sprintf "Cannot perform cast yet from %s into %s"
             (GType.show from) (GType.show into))
  in
  match cast_with with
  | `Proc function_name ->
      let function_name = Expr.Lit (String function_name) in
      let e = Val_repr.as_value ~msg:"TypeCast operand" e in
      let temp = Ctx.fresh_v ctx in
      let call = Cmd.Call (temp, function_name, [ e ], None, None) in
      Cs.return ~app:[ call ] (Val_repr.ByValue (PVar temp))
  | `Nop -> Cs.return e

let rec lvalue_as_access ~ctx ~read (lvalue : GExpr.t) : access Cs.with_body =
  if Ctx.is_zst_access ctx lvalue.type_ then Cs.return ZST
  else
    let open Cs.Syntax in
    let b =
      Body_item.make
        ~loc:(Body_item.compile_location lvalue.location)
        ~id:lvalue.location.origin_id
    in
    let as_access = lvalue_as_access ~ctx ~read in
    match lvalue.value with
    | Symbol x ->
        if Ctx.is_local ctx x then
          if not (Ctx.representable_in_store ctx lvalue.type_) then
            Cs.return (InMemoryComposit { ptr = PVar x; type_ = lvalue.type_ })
          else if Ctx.in_memory ctx x then
            Cs.return (InMemoryScalar { ptr = PVar x; loaded = None })
          else (Direct x, [])
        else
          let genvlookup = Cgil_lib.LActions.(str_ac (AGEnv GetSymbol)) in
          let sym_and_loc = Ctx.fresh_v ctx in
          let act = Cmd.LAction (sym_and_loc, genvlookup, [ Lit (String x) ]) in
          let ptr = Ctx.fresh_v ctx in
          let assign =
            Cmd.Assignment
              (ptr, EList [ Expr.list_nth (PVar sym_and_loc) 1; Expr.zero_i ])
          in
          if Ctx.representable_in_store ctx lvalue.type_ then
            Cs.return
              ~app:[ b act; b assign ]
              (InMemoryScalar { ptr = PVar ptr; loaded = None })
          else
            Cs.return
              (InMemoryComposit { ptr = PVar ptr; type_ = lvalue.type_ })
    | Dereference e -> (
        let* ge = compile_expr ~ctx e in
        match ge with
        | ByValue ge ->
            (* We do read the memory, but it might be a "fake read".
               We don't necessarily need the value if we're going get its address
                (i.e &*p). Therefore, we keep both the value and the pointer around. *)
            if not (Ctx.representable_in_store ctx lvalue.type_) then
              (* In the read case, some validity should be checked *)
              Cs.return (InMemoryComposit { ptr = ge; type_ = lvalue.type_ })
            else if read then
              let+ v = Memory.load_scalar ~ctx ge e.type_ |> Cs.map_l b in
              InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
            else Cs.return (InMemoryScalar { ptr = ge; loaded = None })
        | ByCopy _ ->
            Error.unexpected
              "Dereferencing a composit value, pointers should be passed by \
               value")
    (* Not sure about those 2 actually.
       The parameter in there, is it a pointer? *)
    | Index _ ->
        (* Oh no! this is trivial to implement, but I have to
           call compile_expr, that's circular dependencies... *)
        Error.unhandled "array index lvalue"
    | Member { lhs; field } ->
        let* lhs_access = as_access lhs in
        let ptr, lhs_ty =
          match lhs_access with
          | InMemoryComposit { ptr; type_ } -> (ptr, type_)
          | _ -> Error.code_error "Structure access is not in-memory-composit"
        in
        (* TODO:
           I have the function to compute the offset in bits,
           I just need to do the pointer arithmetics and return the right valu
           either composit or scalar, but in memory in any case *)
        let field_offset = Ctx.offset_struct_field ctx lhs_ty field in
        let ptr = Memory.ptr_add ptr field_offset in
        if Ctx.representable_in_store ctx lvalue.type_ then
          (* I might have to perform the read here,
             in case fake-read is necessary *)
          Cs.return (InMemoryScalar { ptr; loaded = None })
        else Cs.return (InMemoryComposit { ptr; type_ = lvalue.type_ })
    | _ ->
        Error.code_error
          (Fmt.str "lvalue_as_access for something that isn't an lvalue: %a"
             GExpr.pp_full lvalue)

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
  let by_value ?app e = Cs.return ?app (Val_repr.ByValue e) in
  let by_copy ?app ptr type_ =
    Cs.return ?app (Val_repr.ByCopy { ptr; type_ })
  in
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
        Val_repr.as_value ~msg:"__CPROVER_assume operand" to_assume
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
        Val_repr.as_value ~msg:"__CPROVER_assert operand" to_assert
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
        Cs.many
          (Val_repr.as_value_or_unhandled ~feature:CallArgumentByCopy)
          args
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
  let access, pre2 = lvalue_as_access ~ctx ~read:false lhs in
  let write =
    match (access, v) with
    | ZST, _ ->
        [ annot Cmd.Skip ]
        (* We need a command in case we try want to add a label *)
    | Direct x, ByValue v -> [ annot (Assignment (x, v)) ]
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
             \              Left type is %a and right type is %a" pp_access
             access Val_repr.pp v GExpr.pp lhs GExpr.pp rhs GType.pp lhs.type_
             GType.pp rhs.type_)
  in
  (v, pre1 @ pre2 @ write)

and compile_expr ~(ctx : Ctx.t) (expr : GExpr.t) : Val_repr.t Cs.with_body =
  let open Cs.Syntax in
  let by_value ?app t = Cs.return ?app (Val_repr.ByValue t) in
  let by_copy ?app ptr type_ =
    Cs.return ?app (Val_repr.ByCopy { ptr; type_ })
  in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location expr.location in
  let id = expr.location.origin_id in
  let b = Body_item.make ~loc ~id in
  if Ctx.is_zst_access ctx expr.type_ then by_value (Lit Null)
  else
    match expr.value with
    | Symbol _ | Dereference _ | Index _ | Member _ -> (
        let* access = lvalue_as_access ~ctx ~read:true expr in
        match access with
        | ZST -> by_value (Lit Null)
        | Direct x -> by_value (Expr.PVar x)
        | InMemoryScalar { loaded = Some e; _ } -> by_value e
        | InMemoryScalar { loaded = None; ptr } ->
            let* var = Memory.load_scalar ~ctx ptr expr.type_ |> Cs.map_l b in
            by_value (PVar var)
        | InMemoryComposit { ptr; type_ } -> by_copy ptr type_)
    | AddressOf x -> (
        let* access = lvalue_as_access ~ctx ~read:true x in
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
        let rty = rhs.type_ in
        let+ e = compile_binop ~ctx ~lty ~rty op e1 e2 |> Cs.map_l b in
        Val_repr.ByValue e
    | UnOp { op; e } -> (
        let* e = compile_expr e in
        let e = Val_repr.as_value ~msg:"Unary operand" e in
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
