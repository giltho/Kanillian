open Helpers
open Gil_syntax
module GType = Goto_lib.Type
module GExpr = Goto_lib.Expr

type access =
  | InMemoryFunction of { ptr : Expr.t; symbol : string option }
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | InMemoryComposit of { ptr : Expr.t; type_ : GType.t }
      (** For copy, we just need the size, not the type.
          However, in the future, we might copy fields
          one-by-one to preserve the correct semantics of C *)
  | Direct of string
  | DirectFunction of string
  | ZST
[@@deriving show { with_path = false }]

let dummy_access ~ctx type_ =
  if Ctx.is_zst_access ctx type_ then ZST
  else if Ctx.representable_in_store ctx type_ then
    InMemoryScalar { ptr = Lit Nono; loaded = None }
  else InMemoryComposit { ptr = Lit Nono; type_ }

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
        | _ -> `Unhandled `With_type)
    | Notequal -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_ne
        | CInteger I_bool -> `Proc cmpu_ne
        | CInteger I_size_t -> `Proc cmplu_ne
        | CInteger I_ssize_t -> `Proc cmpl_ne
        | _ -> `Unhandled `With_type)
    | IeeeFloatEqual -> (
        match lty with
        | Float -> `Proc cmpfs_eq
        | Double -> `Proc cmpf_eq
        | _ -> `Unhandled `With_type)
    | IeeeFloatNotequal -> (
        match lty with
        | Float -> `Proc cmpfs_ne
        | Double -> `Proc cmpf_ne
        | _ -> `Unhandled `With_type)
    | Le -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_le
        | CInteger I_bool -> `Proc cmpu_le
        | CInteger I_size_t -> `Proc cmplu_le
        | CInteger I_ssize_t -> `Proc cmpl_le
        | _ -> `Unhandled `With_type)
    | Lt -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_lt
        | CInteger I_bool -> `Proc cmpu_lt
        | CInteger I_size_t -> `Proc cmplu_lt
        | CInteger I_ssize_t -> `Proc cmpl_lt
        | _ -> `Unhandled `With_type)
    | Gt -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_gt
        | CInteger I_bool -> `Proc cmpu_gt
        | CInteger I_size_t -> `Proc cmplu_gt
        | CInteger I_ssize_t -> `Proc cmpl_gt
        | _ -> `Unhandled `With_type)
    | Ge -> (
        match lty with
        | CInteger (I_int | I_char) -> `Proc cmp_ge
        | CInteger I_bool -> `Proc cmpu_ge
        | CInteger I_size_t -> `Proc cmplu_ge
        | CInteger I_ssize_t -> `Proc cmpl_ge
        | _ -> `Unhandled `With_type)
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
        | _ -> `Unhandled `With_type)
    | Mult -> (
        match (lty, rty) with
        | CInteger I_int, CInteger I_int -> `Proc mul
        | CInteger I_char, CInteger I_char -> `Proc mul
        | CInteger I_size_t, CInteger I_size_t -> `Proc mull
        | CInteger I_ssize_t, CInteger I_ssize_t -> `Proc mull
        | _ -> `Unhandled `With_type)
    | Mod -> (
        match (lty, rty) with
        | CInteger I_int, CInteger I_int | CInteger I_char, CInteger I_char ->
            `Proc mod_
        | CInteger I_size_t, CInteger I_size_t -> `Proc modlu
        | CInteger I_ssize_t, CInteger I_ssize_t -> `Proc modl
        | Unsignedbv { width = width_a }, Unsignedbv { width = width_b }
          when width_a == width_b
               && (width_a == 8 || width_a == 16 || width_a == 32) -> `Proc modu
        | _ -> `Unhandled `With_type)
    | Or -> `GilBinop BinOp.BOr
    | _ -> `Unhandled `No_type
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
  | `GilBinop b -> Cs.return (Expr.BinOp (e1, b, e2))
  | `Unhandled wt ->
      let type_info =
        match wt with
        | `With_type -> Some (lty, rty)
        | `No_type -> None
      in
      let cmd =
        Helpers.assert_unhandled ~feature:(BinOp (binop, type_info)) []
      in
      Cs.return ~app:[ cmd ] (Expr.Lit Nono)

let fresh_sv ctx =
  let v = Ctx.fresh_v ctx in
  let cmd = Cmd.Logic (FreshSVar v) in
  Cs.return ~app:[ cmd ] v

let assume_type ~ctx (type_ : GType.t) (expr : Expr.t) : unit Cs.with_cmds =
  let open Cs.Syntax in
  match type_ with
  | CInteger ty ->
      let open Cgil_lib.CConstants.VTypes in
      let str_constr =
        match ty with
        | I_int | I_char | I_bool -> int_type
        | I_size_t | I_ssize_t ->
            if Ctx.archi ctx == `Archi64 then long_type else int_type
      in
      let* value = fresh_sv ctx in
      let value = Expr.PVar value in
      let assume_list =
        let f = Formula.Eq (expr, EList [ Lit (String str_constr); value ]) in
        Cmd.Logic (Assume f)
      in
      let assume_int = Cmd.Logic (AssumeType (value, IntType)) in
      Cs.unit [ assume_list; assume_int ]
  | (Signedbv { width } | Unsignedbv { width })
    when width == 8 || width == 16 || width == 32 ->
      let str_constr = Cgil_lib.CConstants.VTypes.int_type in
      let* value = fresh_sv ctx in
      let value = Expr.PVar value in
      let assume_list =
        let f = Formula.Eq (expr, EList [ Lit (String str_constr); value ]) in
        Cmd.Logic (Assume f)
      in
      let assume_int = Cmd.Logic (AssumeType (value, IntType)) in
      Cs.unit [ assume_list; assume_int ]
  | (Signedbv { width } | Unsignedbv { width }) when width == 64 ->
      let str_constr = Cgil_lib.CConstants.VTypes.long_type in
      let* value = fresh_sv ctx in
      let value = Expr.PVar value in
      let assume_list =
        let f = Formula.Eq (expr, EList [ Lit (String str_constr); value ]) in
        Cmd.Logic (Assume f)
      in
      let assume_int = Cmd.Logic (AssumeType (value, IntType)) in
      Cs.unit [ assume_list; assume_int ]
  | Double ->
      let open Cgil_lib.CConstants.VTypes in
      let* value = fresh_sv ctx in
      let value = Expr.PVar value in
      let assume_list =
        let f = Formula.Eq (expr, EList [ Lit (String float_type); value ]) in
        Cmd.Logic (Assume f)
      in
      let assume_num = Cmd.Logic (AssumeType (value, NumberType)) in
      Cs.unit [ assume_list; assume_num ]
  | Float ->
      let open Cgil_lib.CConstants.VTypes in
      let* value = fresh_sv ctx in
      let value = Expr.PVar value in
      let assume_list =
        let f = Formula.Eq (expr, EList [ Lit (String single_type); value ]) in
        Cmd.Logic (Assume f)
      in
      let assume_num = Cmd.Logic (AssumeType (value, NumberType)) in
      Cs.unit [ assume_list; assume_num ]
  | Pointer _ ->
      let* loc = fresh_sv ctx in
      let* ofs = fresh_sv ctx in
      let e_loc = Expr.PVar loc in
      let e_ofs = Expr.PVar ofs in
      let assume_list =
        let f = Formula.Eq (expr, EList [ e_loc; e_ofs ]) in
        Cmd.Logic (Assume f)
      in
      let assume_obj = Cmd.Logic (AssumeType (e_loc, ObjectType)) in
      let assume_int = Cmd.Logic (AssumeType (e_ofs, IntType)) in
      Cs.unit [ assume_list; assume_obj; assume_int ]
  | Bool ->
      let assume_bool = Cmd.Logic (AssumeType (expr, BooleanType)) in
      Cs.unit [ assume_bool ]
  | StructTag tag | UnionTag tag | Struct { tag; _ } | Union { tag; _ } ->
      (* TODO: Add a signal here for something unhandled! *)
      let fail =
        assert_unhandled ~feature:(CompositNondet type_) [ Lit (String tag) ]
      in
      Cs.unit [ fail ]
  | _ ->
      let ty_str = GType.show type_ in
      let fail =
        assert_unhandled ~feature:(CompositNondet type_) [ Lit (String ty_str) ]
      in
      Cs.unit [ fail ]

let rec nondet_expr ~ctx ~loc ~type_ () : Val_repr.t Cs.with_body =
  let b = Body_item.make_hloc ~loc in
  let open Cs.Syntax in
  let is_symbolic_exec =
    let open Gillian.Utils in
    ExecMode.symbolic_exec !Config.current_exec_mode
  in
  if not is_symbolic_exec then
    Error.user_error
      "Looks like you're compiling some nondet variables, but you're not in \
       symbolic execution mode"
  else if Ctx.is_zst_access ctx type_ then
    Cs.return (Val_repr.ByValue (Lit Null))
  else if Ctx.representable_in_store ctx type_ then
    let* fresh = fresh_sv ctx |> Cs.map_l b in
    let fresh = Expr.PVar fresh in
    let+ () = assume_type ~ctx type_ fresh |> Cs.map_l b in
    Val_repr.ByValue fresh
  else
    match type_ with
    | StructTag tag | UnionTag tag ->
        nondet_expr ~ctx ~loc ~type_:(Ctx.tag_lookup ctx tag) ()
    | Struct { components; _ } ->
        let rec writes curr components () =
          match components with
          | [] -> Seq.Nil
          | Datatype_component.Padding { bits; _ } :: fields ->
              (* Ignoring what's written in the padding, probably a nondet, in any case it should be poison. *)
              let width = bits / 8 in
              Cons
                ((curr, Val_repr.Poison { width }), writes (curr + width) fields)
          | Field { type_; _ } :: fields ->
              let rest = writes (curr + Ctx.size_of ctx type_) fields in
              if Ctx.is_zst_access ctx type_ then rest ()
              else
                Cons
                  ( ( curr,
                      Val_repr.V
                        { type_; value = nondet_expr ~ctx ~loc ~type_ () } ),
                    rest )
        in
        let writes = writes 0 components in
        Cs.return (Val_repr.ByCompositValue { type_; writes })
    | Array (ty, sz) ->
        let rec writes cur () =
          if cur == sz then Seq.Nil
          else
            Cons
              ( ( cur,
                  Val_repr.V
                    { type_ = ty; value = nondet_expr ~ctx ~loc ~type_:ty () }
                ),
                writes (cur + 1) )
        in
        let writes = writes 0 in
        Cs.return (Val_repr.ByCompositValue { type_; writes })
    | Union { components; _ } ->
        (* Ok this one is tricky, because each variant will have
           a different layout, and that doesn't work well with my memory.
           So we'll have to branch *)
        let variant_amount = List.length components in
        let* variant =
          Cs.map_l b
          @@ let* variant = fresh_sv ctx in
             let variant = Expr.PVar variant in
             let variant_number = Expr.int variant_amount in
             let variant_int = LCmd.AssumeType (variant, IntType) in
             let variant_constraint =
               let open Formula.Infix in
               Expr.zero_i #<= variant #&& (variant #< variant_number)
             in
             let variant_value = LCmd.Assume variant_constraint in
             Cs.return
               ~app:[ Cmd.Logic variant_int; Logic variant_value ]
               variant
        in
        let* ret_ptr =
          Memory.alloc_temp ~ctx ~location:loc type_ |> Cs.map_l b
        in
        let end_lab = Ctx.fresh_lab ctx in
        let goto_end = Cmd.Goto end_lab in
        let next_lab = ref None in
        let rec gen i components =
          match components with
          | [] -> []
          | Datatype_component.Padding _ :: _ ->
              Error.unexpected "Padding in union fields"
          | Field { type_; _ } :: rest ->
              let curr_lab = !next_lab in
              let nlab = Ctx.fresh_lab ctx in
              let () = next_lab := Some nlab in
              let block_lab = Ctx.fresh_lab ctx in
              let goto =
                b ?label:curr_lab
                  (Cmd.GuardedGoto
                     (Expr.BinOp (variant, Equal, Expr.int i), block_lab, nlab))
              in
              let v, cmds = nondet_expr ~ctx ~loc ~type_ () in
              let write =
                Memory.write ~ctx ~type_ ~annot:b ~dst:ret_ptr ~src:v
              in
              let block =
                set_first_label ~annot:(b ~loop:[]) block_lab
                  (cmds @ write @ [ b goto_end ])
              in
              (goto :: block) @ gen (i + 1) rest
        in
        let* () = Cs.unit (gen 0 components) in
        let* () =
          Cs.unit
            [
              b ?label:!next_lab (Fail ("Unreachable", []));
              b ~label:end_lab Skip;
            ]
        in
        Cs.return (Val_repr.ByCopy { type_; ptr = ret_ptr })
    | _ ->
        let fail_cmd =
          assert_unhandled ~feature:(CompositNondet type_)
            [ Expr.string (GType.show type_) ]
        in
        let+ () = Cs.unit [ b fail_cmd ] in
        Val_repr.ByCopy { ptr = Lit Nono; type_ }

let compile_cast ~(ctx : Ctx.t) ~(from : GType.t) ~(into : GType.t) e :
    Val_repr.t Cs.with_cmds =
  let cast_with =
    match (from, into) with
    | x, y when GType.equal x y -> `Nop
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
    | CInteger I_size_t, CInteger I_ssize_t when ctx.machine.pointer_width == 64
      -> `Proc Kconstants.Cast_functions.sign_long
    | CInteger I_size_t, CInteger I_ssize_t when ctx.machine.pointer_width == 32
      -> `Proc Kconstants.Cast_functions.sign_int
    | Pointer _, Pointer _ -> `Nop
    | _ -> `Unhandled
  in
  match cast_with with
  | `Proc function_name ->
      let function_name = Expr.Lit (String function_name) in
      let e = Val_repr.as_value ~msg:"TypeCast operand" e in
      let temp = Ctx.fresh_v ctx in
      let call = Cmd.Call (temp, function_name, [ e ], None, None) in
      Cs.return ~app:[ call ] (Val_repr.ByValue (PVar temp))
  | `Nop -> Cs.return e
  | `Unhandled ->
      let cmd = Helpers.assert_unhandled ~feature:(Cast (from, into)) [] in
      Cs.return ~app:[ cmd ] (Val_repr.dummy ~ctx into)

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
    | Struct _ ->
        let cmd = b (assert_unhandled ~feature:(ConstantLValue "Struct") []) in
        Cs.return ~app:[ cmd ]
          (InMemoryComposit { ptr = Lit Nono; type_ = lvalue.type_ })
    | Array _ ->
        let cmd = b (assert_unhandled ~feature:(ConstantLValue "Array") []) in
        Cs.return ~app:[ cmd ]
          (InMemoryComposit { ptr = Lit Nono; type_ = lvalue.type_ })
    | StringConstant _ ->
        let cmd =
          b (assert_unhandled ~feature:(ConstantLValue "StringConstant") [])
        in
        Cs.return ~app:[ cmd ]
          (InMemoryComposit { ptr = Lit Nono; type_ = lvalue.type_ })
    | Symbol x ->
        if Ctx.is_local ctx x then
          if not (Ctx.representable_in_store ctx lvalue.type_) then
            Cs.return (InMemoryComposit { ptr = PVar x; type_ = lvalue.type_ })
          else if Ctx.in_memory ctx x then
            Cs.return (InMemoryScalar { ptr = PVar x; loaded = None })
          else (Direct x, [])
        else if Ctx.is_function_symbol ctx x then Cs.return (DirectFunction x)
        else
          let+ ptr = Genv.lookup_symbol ~ctx x |> Cs.map_l b in
          if GType.is_function lvalue.type_ then
            InMemoryFunction { ptr; symbol = Some x }
          else if Ctx.representable_in_store ctx lvalue.type_ then
            InMemoryScalar { ptr; loaded = None }
          else InMemoryComposit { ptr; type_ = lvalue.type_ }
    | Dereference e -> (
        let* ge = compile_expr ~ctx e in
        match ge with
        | ByValue ge ->
            (* We do read the memory, but it might be a "fake read".
               We don't necessarily need the value if we're going get its address
                (i.e &*p). Therefore, we keep both the value and the pointer around. *)
            if GType.is_function lvalue.type_ then
              Cs.return (InMemoryFunction { ptr = ge; symbol = None })
            else if not (Ctx.representable_in_store ctx lvalue.type_) then
              (* In the read case, some validity should be checked *)
              Cs.return (InMemoryComposit { ptr = ge; type_ = lvalue.type_ })
            else if read then
              let+ v = Memory.load_scalar ~ctx ge e.type_ |> Cs.map_l b in
              InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
            else Cs.return (InMemoryScalar { ptr = ge; loaded = None })
        | ByCopy _ | ByCompositValue _ ->
            Error.unexpected "Pointers should be scalars passed by value"
        | Procedure _ -> Error.unexpected "Dereferencing a procedure")
    | Index { array; index } ->
        let* index = compile_expr ~ctx index in
        let index =
          match index with
          | ByValue index -> index
          | _ -> Error.unexpected "Indexing with non-scalar value"
        in
        let* lhs_access = as_access array in
        let* ptr =
          match lhs_access with
          | InMemoryComposit { ptr; _ } -> Cs.return ptr
          | ZST ->
              let cmd = b (assert_unhandled ~feature:FlexibleArrayMember []) in
              Cs.return ~app:[ cmd ] (Expr.Lit Nono)
          | _ -> Error.code_error "Array access is not in-memory-composit"
        in
        let sz =
          let sz_i = Ctx.size_of ctx lvalue.type_ in
          let ty =
            let open Cgil_lib.CConstants.VTypes in
            match Ctx.archi ctx with
            | `Archi32 -> int_type
            | `Archi64 -> long_type
          in
          Expr.EList [ Lit (String ty); Expr.int sz_i ]
        in
        let* offset =
          let mult =
            match Ctx.archi ctx with
            | `Archi32 -> Kconstants.Binop_functions.mul
            | `Archi64 -> Kconstants.Binop_functions.mull
          in
          let res = Ctx.fresh_v ctx in
          let call =
            Cmd.Call (res, Lit (String mult), [ index; sz ], None, None)
          in
          Cs.return ~app:[ b call ] (Expr.list_nth (Expr.PVar res) 1)
        in
        let ptr = Memory.ptr_add_e ptr offset in
        if Ctx.representable_in_store ctx lvalue.type_ then
          Cs.return (InMemoryScalar { ptr; loaded = None })
        else Cs.return (InMemoryComposit { ptr; type_ = lvalue.type_ })
    | Member { lhs; field } ->
        let* lhs_access = as_access lhs in
        let ptr, lhs_ty =
          match lhs_access with
          | InMemoryComposit { ptr; type_ } -> (ptr, type_)
          | _ -> Error.code_error "Structure access is not in-memory-composit"
        in
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
  match func.value with
  | Symbol "__CPROVER_assume" ->
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
            Logging.normal ~severity:Warning (fun m ->
                m "Cannot assume %a, assuming False instead" Expr.pp to_assume);
            Formula.False
        | Some (f, _) -> f
      in
      by_value ~app:[ b (Logic (Assume f)) ] (Lit Null)
  | Symbol "__CPROVER_assert" ->
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
            Logging.normal ~severity:Warning (fun m ->
                m "Cannot assert %a, asserting False instead" Expr.pp to_assert);
            Formula.False
        | Some (f, _) -> f
      in
      by_value ~app:[ b (Logic (Assert f)) ] (Expr.Lit Null)
  | _ ->
      let* e = compile_expr ~ctx func in
      let fname =
        match e with
        | Procedure e -> e
        | _ ->
            Error.code_error "function call of something that isn't a procedure"
      in
      let* args =
        Cs.many
          (fun arg ->
            let* c_arg = compile_expr ~ctx arg in
            match c_arg with
            | Val_repr.ByValue e -> Cs.return e
            | ByCopy { ptr; _ } ->
                (* When passing by copy, we trust the receiving function to copy the value when it receives it. *)
                Cs.return ptr
            | ByCompositValue { writes; type_ } ->
                let* temp =
                  Memory.alloc_temp ~ctx ~location:arg.location type_
                  |> Cs.map_l b
                in
                let+ () =
                  Cs.unit (Memory.write_composit ~ctx ~annot:b ~dst:temp writes)
                in
                temp
            | Procedure _ -> Error.unexpected "passing a procedure as argument")
          args
      in
      (* If the return value has to be passed by copy,
         we add an argument before every other in which the return value will be stored.
         It's a trick of CompCert.*)
      if Ctx.representable_in_store ctx return_type then
        let ret_var = Ctx.fresh_v ctx in
        let gil_call = Cmd.Call (ret_var, fname, args, None, None) in
        by_value ~app:[ b gil_call ] (Expr.PVar ret_var)
      else
        (* If the function returns by copy, we add first parameter
           that will contain the result. *)
        let* temp_arg =
          Memory.alloc_temp ~ctx ~location return_type |> Cs.map_l b
        in
        let unused_temp = Ctx.fresh_v ctx in
        let gil_call =
          Cmd.Call (unused_temp, fname, temp_arg :: args, None, None)
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
        if not (Ctx.type_equal ctx type_v type_access) then
          Error.unexpected "ByCopy assignment with different types on each side"
        else
          let copy_cmd =
            Memory.memcpy ~ctx ~type_:type_access ~dst:ptr_access ~src:ptr_v
          in
          [ annot copy_cmd ]
    | InMemoryComposit { ptr = dst; _ }, ByCompositValue { writes; _ } ->
        Memory.write_composit ~ctx ~annot ~dst writes
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
  let unhandled feature =
    let cmd = assert_unhandled ~feature [] in
    let v = Val_repr.dummy ~ctx expr.type_ in
    Cs.return ~app:[ b cmd ] v
  in
  match expr.value with
  | Symbol _ | Dereference _ | Index _ | Member _ -> (
      if Ctx.is_zst_access ctx expr.type_ then by_value (Lit Null)
      else
        let* access = lvalue_as_access ~ctx ~read:true expr in
        match access with
        | ZST -> by_value (Lit Null)
        | Direct x -> by_value (Expr.PVar x)
        | InMemoryScalar { loaded = Some e; _ } -> by_value e
        | InMemoryScalar { loaded = None; ptr } ->
            let* var = Memory.load_scalar ~ctx ptr expr.type_ |> Cs.map_l b in
            by_value (PVar var)
        | InMemoryComposit { ptr; type_ } -> by_copy ptr type_
        | InMemoryFunction { symbol = Some sym; _ } ->
            Cs.return (Val_repr.Procedure (Expr.string sym))
        | InMemoryFunction { ptr; symbol = None } ->
            let symbol = Ctx.fresh_v ctx in
            let get_name =
              Cgil_lib.CConstants.Internal_Functions.get_function_name
            in
            let call =
              Cmd.Call (symbol, Lit (String get_name), [ ptr ], None, None)
            in
            Cs.return ~app:[ b call ] (Val_repr.Procedure (Expr.PVar symbol))
        | DirectFunction symbol ->
            Cs.return (Val_repr.Procedure (Lit (String symbol))))
  | AddressOf x -> (
      let* access = lvalue_as_access ~ctx ~read:true x in
      match access with
      | ZST ->
          unhandled ZstAddress
          (* Should probably just return a long, with a nondet value that has the right offset *)
      | InMemoryScalar { ptr; _ }
      | InMemoryComposit { ptr; _ }
      | InMemoryFunction { ptr; _ } -> by_value ptr
      | DirectFunction symbol ->
          let+ ptr = Genv.lookup_symbol ~ctx symbol |> Cs.map_l b in
          Val_repr.ByValue ptr
      | Direct x -> Error.code_error ("address of direct access to " ^ x))
  | BoolConstant b -> by_value (Lit (Bool b))
  | CBoolConstant b ->
      let i = if b then Gcu.Camlcoq.Z.one else Gcu.Camlcoq.Z.zero in
      let lit = Gcu.Vt.gil_of_compcert (Gcu.Values.Vint i) in
      by_value (Lit lit)
  | PointerConstant b ->
      let i = Gcu.Camlcoq.Z.of_sint b in
      let v =
        match Ctx.archi ctx with
        | `Archi32 -> Gcu.Values.Vint i
        | `Archi64 -> Gcu.Values.Vlong i
      in
      let lit = Gcu.Vt.gil_of_compcert v in
      by_value (Lit lit)
  | IntConstant z ->
      let cz = Gcu.Vt.z_of_int z in
      let ccert_value =
        let open Gcu.Values in
        match expr.type_ with
        | CInteger (I_int | I_char | I_bool) -> Vint cz
        | CInteger (I_size_t | I_ssize_t) -> (
            match Ctx.archi ctx with
            | `Archi32 -> Vint cz
            | `Archi64 -> Vlong cz)
        | Unsignedbv { width } | Signedbv { width } ->
            if width <= 32 then Vint cz else Vlong cz
        | _ -> Error.unexpected "IntConstant with non-int type"
      in
      let lit = Gcu.Vt.gil_of_compcert ccert_value in
      by_value (Lit lit)
  | DoubleConstant f ->
      let typ = Cgil_lib.CConstants.VTypes.float_type in
      by_value (Lit (LList [ String typ; Num f ]))
  | FloatConstant f ->
      let typ = Cgil_lib.CConstants.VTypes.single_type in
      by_value (Lit (LList [ String typ; Num f ]))
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
      | op ->
          let cmd = b (Helpers.assert_unhandled ~feature:(UnOp op) []) in
          Cs.return ~app:[ cmd ] (Val_repr.ByValue (Lit Nono)))
  | Nondet -> nondet_expr ~ctx ~loc:expr.location ~type_:expr.type_ ()
  | TypeCast to_cast ->
      let* to_cast_e = compile_expr to_cast in
      compile_cast ~ctx ~from:to_cast.type_ ~into:expr.type_ to_cast_e
      |> Cs.map_l b
  | Assign { lhs; rhs } -> compile_assign ~ctx ~lhs ~rhs ~annot:b
  | FunctionCall { func; args } -> compile_call ~ctx ~add_annot:b func args
  | If { cond; then_; else_ } ->
      let* cond_e = compile_expr cond in
      let then_lab = Ctx.fresh_lab ctx in
      let else_lab = Ctx.fresh_lab ctx in
      let cond_e = Val_repr.as_value ~msg:"Expr If condition" cond_e in
      let* () = Cs.unit [ b (GuardedGoto (cond_e, then_lab, else_lab)) ] in
      let res = Ctx.fresh_v ctx in
      let end_lab = Ctx.fresh_lab ctx in
      (* Ok the following is going to be an interesting trick.
         We assign the expr, even if it's by-copy.
         We don't copy! So we're kinda hacking inside
         the type abstraction. *)
      let* res_then =
        let res_then =
          let* t = compile_expr then_ in
          let* res = Val_repr.copy_into t res |> Cs.map_l b in
          let cmd = b (Cmd.Goto end_lab) in
          Cs.return ~app:[ cmd ] res
        in
        res_then |> Cs.with_label ~annot:(b ~loop:[]) then_lab
      in
      let* res_else =
        let res_else =
          let* t = compile_expr else_ in
          Val_repr.copy_into t res |> Cs.map_l b
        in
        res_else |> Cs.with_label ~annot:(b ~loop:[]) else_lab
      in
      Error.assert_
        (Val_repr.equal res_then res_else)
        "if branche exprs must be equal";
      Cs.return ~app:[ b ~label:end_lab Skip ] res_then
  | ByteExtract _ -> unhandled ByteExtract
  | Struct elems ->
      let fields = Ctx.resolve_struct_components ctx expr.type_ in
      (* We start by getting the offsets where we need to write,
         we do that on the type. *)
      let rec writes curr fields elems () =
        match (fields, elems) with
        | [], [] -> Seq.Nil
        | Datatype_component.Padding { bits; _ } :: fields, _ :: elems
        (* Ignoring what's written in the padding, probably a nondet, in any case it should be poison. *)
          ->
            let width = bits / 8 in
            Cons
              ( (curr, Val_repr.Poison { width }),
                writes (curr + width) fields elems )
        | Field { type_; _ } :: fields, _ :: elems
          when Ctx.is_zst_access ctx type_ ->
            (* If the field is a ZST, we simply ignore it *)
            writes (curr + Ctx.size_of ctx type_) fields elems ()
        | Field { type_; _ } :: fields, v :: elems ->
            Cons
              ( (curr, Val_repr.V { type_; value = compile_expr v }),
                writes (curr + Ctx.size_of ctx type_) fields elems )
        | _ ->
            Error.unexpected
              "Struct type fields not maching struct constant fields"
      in
      let writes = writes 0 fields elems in
      Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes })
  | Array elems ->
      let elem_type =
        match expr.type_ with
        | Array (elem_type, _) -> elem_type
        | _ -> Error.unexpected "Array is not of Array type"
      in
      let elem_size = Ctx.size_of ctx elem_type in
      let rec writes count elems () =
        match elems with
        | [] -> Seq.Nil
        | v :: elems ->
            Cons
              ( ( count * elem_size,
                  Val_repr.V { type_ = elem_type; value = compile_expr v } ),
                writes (count + 1) elems )
      in
      let writes = writes 0 elems in
      Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes })
  | StringConstant str ->
      let char_type = GType.CInteger I_char in
      (* Size could be different from 1 n some architecture,
         might as well anticipate here *)
      let char_size = Ctx.size_of ctx char_type in
      let writes =
        String.to_seq str
        |> Seq.mapi (fun i b ->
               ( i * char_size,
                 Val_repr.V
                   {
                     type_ = char_type;
                     value = (Val_repr.ByValue (Expr.int (Char.code b)), []);
                   } ))
      in
      Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes })
  | Unhandled (id, msg) -> unhandled (ExprIrep (id, msg))
