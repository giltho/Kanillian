module GExpr = Goto_lib.Expr
module GType = Goto_lib.Type
open Gil_syntax

(** Represents how a value is accessed.
    Given the CompCert memory model, I might actually need
    a 3rd variant, representing in-memory access for values that
    have to be copied, not just read.
    Moreover, InMemoryScalar should probably directly be aware of
    the chunk *)
type access =
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | InMemoryComposit of { ptr : Expr.t; type_ : GType.t }
      (** For copy, we just need the size, not the type.
          However, in the future, we might copy fields
          one-by-one to preserve the correct semantics of C *)
  | Direct of string
  | ZST
[@@deriving show { with_path = false }]

let rec as_access ~ctx ~read (lvalue : GExpr.t) : access Cs.with_body =
  if Ctx.is_zst_access ctx lvalue.type_ then Cs.return ZST
  else
    let open Cs.Syntax in
    let b =
      Body_item.make
        ~loc:(Body_item.compile_location lvalue.location)
        ~id:lvalue.location.origin_id
    in
    let lift x = Cs.map_l b x in
    let as_access = as_access ~ctx ~read in
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
        let* ge = as_access e in
        match ge with
        | Direct x ->
            (* We do read the memory, but it might be a "fake read".
               We don't necessarily need the value if we're going get its address
                (i.e &*p). Therefore, we keep both the value and the pointer around. *)
            let ge = Expr.PVar x in
            if read then
              let+ v = Memory.load_scalar ~ctx ge e.type_ |> Cs.map_l b in
              InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
            else Cs.return (InMemoryScalar { ptr = ge; loaded = None })
        | InMemoryScalar { ptr; loaded } ->
            let* loaded =
              lift
              @@
              match loaded with
              | None ->
                  let+ v = Memory.load_scalar ~ctx ptr e.type_ in
                  Expr.PVar v
              | Some loaded -> Cs.return loaded
            in
            if read then
              let+ v = Memory.load_scalar ~ctx loaded e.type_ |> lift in
              InMemoryScalar { ptr = loaded; loaded = Some (PVar v) }
            else Cs.return (InMemoryScalar { ptr = loaded; loaded = None })
        | InMemoryComposit _ ->
            Error.code_error
              "Dereferencing a composit values. Pointers are scalars, that \
               should never happen."
        | ZST -> Error.code_error "Dereferencing a ZST?")
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
          (Fmt.str "Lvalue.as_access for something that isn't an lvalue: %a"
             GExpr.pp_full lvalue)
