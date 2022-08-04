open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

type t =
  | Sptr of string * Expr.t
  | SVint of Expr.t * Chunk.t
  | SVfloat of Expr.t * [ `Single | `Double ]
[@@deriving yojson, eq]

let pp ft v =
  let open Fmt in
  let se = Expr.pp in
  match v with
  | Sptr (l, ofs) -> pf ft "Ptr(%s, %a)" l se ofs
  | SVint (i, c) -> pf ft "(%a: %s)" se i (Chunk.to_string c)
  | SVfloat (f, c) ->
      pf ft "(%a: %s)" se f
        (match c with
        | `Single -> "f32"
        | `Double -> "f64")

let substitution ~le_subst sv =
  match sv with
  | SVint (v, c) -> SVint (le_subst v, c)
  | SVfloat (v, c) -> SVfloat (le_subst v, c)
  | Sptr (loc, offs) -> (
      let loc_e = Expr.loc_from_loc_name loc in
      match le_subst loc_e with
      | Expr.ALoc nloc | Lit (Loc nloc) -> Sptr (nloc, le_subst offs)
      | e ->
          failwith
            (Format.asprintf "Heap substitution fail for loc: %a" Expr.pp e))

exception Not_a_C_value of Expr.t

let zero_of_chunk (chunk : Chunk.t) =
  match chunk with
  | I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 ->
      SVint (Expr.zero_i, chunk)
  | F32 -> SVfloat (Lit (Num 0.), `Single)
  | F64 -> SVfloat (Lit (Num 0.), `Double)

let any_of_chunk (chunk : Chunk.t) : t Delayed.t =
  let lvar = LVar.alloc () in
  let lvar_e = Expr.LVar lvar in
  match chunk with
  | I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 ->
      let learned_types = [ (lvar, Type.IntType) ] in
      let learned =
        match Chunk.bounds chunk with
        | Some (low, high) ->
            let open Formula.Infix in
            [ lvar_e #>= (Expr.int_z low); lvar_e #<= (Expr.int_z high) ]
        | None -> []
      in
      Delayed.return ~learned_types ~learned (SVint (lvar_e, chunk))
  | F32 ->
      let learned_types = [ (lvar, Type.NumberType) ] in
      let learned = [] in
      Delayed.return ~learned_types ~learned (SVfloat (lvar_e, `Single))
  | F64 ->
      let learned_types = [ (lvar, Type.NumberType) ] in
      let learned = [] in
      Delayed.return ~learned_types ~learned (SVfloat (lvar_e, `Double))

let lvars =
  let open Utils.Containers in
  function
  | Sptr (_, e) -> Expr.lvars e
  | SVint (e, _) | SVfloat (e, _) -> Expr.lvars e

let alocs =
  let open Utils.Containers in
  function
  | Sptr (l, e) ->
      let alocs_e = Expr.alocs e in
      if Utils.Names.is_aloc_name l then SS.add l alocs_e else alocs_e
  | SVint (e, _) | SVfloat (e, _) -> Expr.alocs e

module Patterns = struct
  open Formula.Infix

  let number e =
    let open Expr in
    (typeof e) #== (type_ NumberType)

  let integer e =
    let open Expr in
    (typeof e) #== (type_ IntType)

  let undefined x = x #== (Expr.Lit Undefined)

  let obj x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (int 2))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ IntType))
end

let of_chunk_and_expr (chunk : Chunk.t) sval_e =
  let open Patterns in
  let return = Delayed.return in
  let* sval_e = Delayed.reduce sval_e in
  let assert_type ty = Delayed.assert_type sval_e ty (Not_a_C_value sval_e) in
  let svint ?learned () = return ?learned (SVint (sval_e, chunk)) in
  let sptr ?learned l o = return ?learned (Sptr (l, o)) in
  match (chunk, !Kconfig.archi) with
  | U64, Arch64 | U32, Arch32 ->
      let* is_int = Delayed.has_type sval_e IntType in
      if is_int then svint ()
      else
        let* () = Delayed.assert_ (obj sval_e) (Not_a_C_value sval_e) in
        let loc_expr = Expr.list_nth sval_e 0 in
        let ofs = Expr.list_nth sval_e 1 in
        let* ofs = Delayed.reduce ofs in
        let* loc_opt = Delayed.resolve_loc loc_expr in
        let loc, learned =
          match loc_opt with
          | Some l -> (l, [])
          | None ->
              let aloc = ALoc.alloc () in
              let learned =
                let open Formula.Infix in
                [ loc_expr #== (ALoc aloc) ]
              in
              (aloc, learned)
        in
        sptr ~learned loc ofs
  | (U8 | I8 | U16 | I16 | I32 | U32 | I64 | U64 | I128 | U128), _ ->
      let* () = assert_type IntType in
      let open Formula.Infix in
      let i k = Expr.int k in
      let learned =
        match chunk with
        | U8 -> [ (i 0) #<= sval_e; sval_e #<= (i 255) ]
        | _ -> []
      in
      svint ~learned ()
  | F64, _ ->
      let* () = assert_type NumberType in
      return (SVfloat (sval_e, `Double))
  | F32, _ ->
      let* () = assert_type NumberType in
      return (SVfloat (sval_e, `Single))

let to_gil_expr_undelayed sval =
  let open Expr in
  match sval with
  | Sptr (loc_name, offset) ->
      let loc = loc_from_loc_name loc_name in
      (EList [ loc; offset ], [ (loc, Type.ObjectType); (offset, Type.IntType) ])
  | SVint (n, _) -> (n, [ (n, Type.IntType) ])
  | SVfloat (n, _) -> (n, [ (n, Type.NumberType) ])

let to_gil_expr sval =
  let exp, typings = to_gil_expr_undelayed sval in
  let typing_pfs =
    List.map
      (fun (e, t) ->
        let open Expr in
        let open Formula.Infix in
        (typeof e) #== (type_ t))
      typings
  in
  Delayed.return ~learned:typing_pfs exp

let sure_is_zero = function
  | SVint (Lit (Int z), _) when Z.equal z Z.zero -> true
  | SVfloat (Lit (Num 0.), _) -> true
  | _ -> false

module SVArray = struct
  type sval = t

  type t =
    | Arr of Expr.t
        (** the parameter should be a list representing a *NON-EMPTY* list *)
    | AllUndef
    | AllZeros
  [@@deriving yojson]

  let reduce t =
    let open Delayed.Syntax in
    match t with
    | Arr e ->
        let+ reduced = Delayed.reduce e in
        Arr reduced
    | _ -> Delayed.return t

  let pp fmt = function
    | Arr e -> Expr.pp fmt e
    | AllUndef -> Fmt.string fmt "AllUndef"
    | AllZeros -> Fmt.string fmt "AllZeros"

  let empty = Arr (EList [])

  let is_empty =
    let open Formula.Infix in
    function
    | Arr e -> (Expr.list_length e) #== (Expr.int 0)
    | _ -> False

  let sure_is_all_zeros = function
    | Arr (EList l) ->
        List.for_all
          (function
            | Expr.Lit (Int z) when Z.equal z Z.zero -> true
            | _ -> false)
          l
    | AllZeros -> true
    | _ -> false

  let equal arr_a arr_b =
    match (arr_a, arr_b) with
    | Arr a, Arr b -> Expr.equal a b
    | AllUndef, AllUndef | AllZeros, AllZeros -> true
    | _ -> false

  let conc_to_abst_undelayed conc =
    let rev_l, gamma =
      List.fold_left
        (fun (acc, gamma) sval ->
          let new_el, new_gamma = to_gil_expr_undelayed sval in
          (new_el :: acc, new_gamma @ gamma))
        ([], []) conc
    in
    let learned =
      List.map
        (let open Formula.Infix in
        fun (e, t) -> (Expr.typeof e) #== (Expr.type_ t))
        gamma
    in
    (Expr.EList (List.rev rev_l), learned)

  let conc_to_abst conc =
    let e, learned = conc_to_abst_undelayed conc in
    Delayed.return ~learned e

  let undefined_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let zero = Expr.int 0 in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: Concrete: %a" Expr.pp size);
        let undefs =
          Expr.Lit (LList (List.init (Z.to_int x) (fun _ -> Literal.Undefined)))
        in
        arr_exp #== undefs
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: not as concrete: %a" Expr.pp size);
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> ((Expr.list_nth_e arr_exp i_e) #== (Lit Undefined))

  let zeros_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt -> fmt "Zeros pf: Concrete: %a" Expr.pp size);
        let zeros =
          Expr.Lit
            (LList (List.init (Z.to_int x) (fun _ -> Literal.Int Z.zero)))
        in
        arr_exp #== zeros
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Zeros pf: not as concrete: %a" Expr.pp size);
        let is_zero e = e #== (Expr.int 0) in
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        let zero = Expr.int 0 in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> (is_zero (Expr.list_nth_e arr_exp i_e))

  let to_arr_with_size arr s =
    let open Formula.Infix in
    let allocate_array_lvar (descr : ?size:Expr.t -> Expr.t -> Formula.t) =
      let x = LVar.alloc () in
      let learned_types = [ (x, Gil_syntax.Type.ListType) ] in
      let x = Expr.LVar x in
      let learned = [ (Expr.list_length x) #== s; descr ~size:s x ] in
      Delayed.return ~learned ~learned_types x
    in
    match arr with
    | Arr e -> Delayed.return e
    | AllUndef -> allocate_array_lvar undefined_pf
    | AllZeros -> allocate_array_lvar zeros_pf

  let concat_knowing_size (left, left_size) (right, right_size) =
    let open Delayed in
    let open Delayed.Syntax in
    match (left, right) with
    | Arr a, Arr b -> return (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> return AllUndef
    | AllZeros, AllZeros -> return AllZeros
    | left, right ->
        let* left = to_arr_with_size left left_size in
        let+ right = to_arr_with_size right right_size in
        Arr (Expr.list_cat left right)

  let concat left right =
    match (left, right) with
    | Arr a, Arr b -> Some (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> Some AllUndef
    | AllZeros, AllZeros -> Some AllZeros
    | _ -> None

  (** This already assumes the value is a number and not a pointer *)
  let to_single_value ~chunk = function
    | Arr (EList [ a ]) ->
        let+ v = of_chunk_and_expr chunk a in
        Some v
    | AllZeros -> DO.some (zero_of_chunk chunk)
    | _ -> DO.none ()

  let singleton = function
    (* Assuming that the chunk is correct already *)
    | SVfloat (e, _) | SVint (e, _) -> Arr (Expr.EList [ e ])
    | Sptr _ as ptr ->
        let e_ptr, _ = to_gil_expr_undelayed ptr in
        Arr (Expr.EList [ e_ptr ])

  let array_sub arr o len : t =
    match arr with
    | AllZeros -> AllZeros
    | AllUndef -> AllUndef
    | Arr e -> Arr (Expr.list_sub ~lst:e ~start:o ~size:len)

  (** This assumes chunks are properly respected outside of the call of this function *)
  let array_cons (el : sval) arr = concat (singleton el) arr

  let array_append arr el = concat arr (singleton el)

  let to_gil_expr_undelayed ~chunk ~range svarr =
    let chunk_size = Expr.int (Chunk.size chunk) in
    let size =
      let open Expr.Infix in
      let low, high = range in
      (high - low) / chunk_size
    in
    let f_of_all_same ~describing_pf ~concrete_single =
      match size with
      | Lit (Int n) ->
          (Expr.EList (Utils.List_utils.make (Z.to_int n) concrete_single), [])
      | _ ->
          let open Formula.Infix in
          let arr = LVar.alloc () in
          let arr_e = Expr.LVar arr in
          let learned =
            let open Expr in
            [
              (typeof arr_e) #== (type_ ListType);
              (list_length arr_e) #== size;
              describing_pf arr_e;
            ]
          in
          (arr_e, learned)
    in
    match svarr with
    | Arr e ->
        let open Formula.Infix in
        let learned =
          [
            (Expr.typeof e) #== (Expr.type_ ListType);
            (Expr.list_length e) #== size;
          ]
        in
        (e, learned)
    | AllZeros ->
        f_of_all_same ~concrete_single:Expr.zero_i ~describing_pf:zeros_pf
    | AllUndef ->
        f_of_all_same ~concrete_single:(Expr.Lit Undefined)
          ~describing_pf:undefined_pf

  let to_gil_expr ~chunk ~range (svarr : t) : Expr.t Delayed.t =
    let e, learned = to_gil_expr_undelayed ~chunk ~range svarr in
    Delayed.return ~learned e

  let of_gil_expr_exn expr = Arr expr

  (** Only call on Mint8Unsigned arrays *)
  let learn_chunk ~chunk ~size arr =
    let bounds = Chunk.bounds chunk in
    let* size = Delayed.reduce size in
    match bounds with
    | None -> Delayed.return ()
    | Some (low, high) -> (
        match arr with
        | Arr (EList e) ->
            let i k = Expr.int_z k in
            let learned =
              List.concat_map
                (function
                  | Expr.Lit Undefined -> []
                  | x ->
                      let open Formula.Infix in
                      [ (i low) #<= x; x #<= (i high) ])
                e
            in
            Delayed.return ~learned ()
        | Arr e -> (
            match size with
            | Expr.Lit (Int n) ->
                let i k = Expr.int_z k in
                let learned =
                  List.concat
                    (List.init (Z.to_int n) (fun k ->
                         let x = Expr.list_nth e k in
                         let open Formula.Infix in
                         [ (i low) #<= x; x #<= (i high) ]))
                in
                Delayed.return ~learned ()
            | _ -> Delayed.return ())
        | _ -> Delayed.return ())

  (* type nonrec t = Conc of t list | Abst of Expr.t | AllUndef | AllZeros *)

  let subst ~le_subst t =
    match t with
    | Arr e ->
        let s = le_subst e in
        if s == e then t else Arr s
    | AllUndef -> AllUndef
    | AllZeros -> AllZeros
end

module Infix = struct
  let ( @: ) = SVArray.concat
  let ( ^: ) = SVArray.array_cons
  let ( ^:? ) a b = Option.bind b (fun b -> a ^: b)
end
