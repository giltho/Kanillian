open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

(* A symbolic value in memory is just an expr, but
   we maintain an abstraction around it to make sure
   we properly do sanity checks at the boundaries *)

type t = Expr.t [@@deriving yojson]

let alocs e = Expr.alocs e
let lvars e = Expr.lvars e
let substitution ~le_subst e = le_subst e
let equal a b = Expr.equal a b
let pp ft t = Expr.pp ft t
let to_gil_expr t = t

let zero_of_chunk (chunk : Chunk.t) =
  match chunk with
  | I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 -> Expr.zero_i
  | F32 -> Lit (Num 0.)
  | F64 -> Lit (Num 0.)

let any_of_chunk (chunk : Chunk.t) : Expr.t Delayed.t =
  let lvar = LVar.alloc () in
  let lvar_e = Expr.LVar lvar in
  let learned_types, learned =
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
        (learned_types, learned)
    | F32 | F64 ->
        let learned_types = [ (lvar, Type.NumberType) ] in
        let learned = [] in
        (learned_types, learned)
  in
  Delayed.return ~learned_types ~learned lvar_e

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

(* Raise this exception when the right types
   haven't been correctly assumed. *)
exception Not_a_C_value of Expr.t

let () =
  Printexc.register_printer (function
    | Not_a_C_value e ->
        Some
          (Format.asprintf "Storing this value, it's not a C value: %a" Expr.pp
             e)
    | _ -> None)

let of_chunk_and_expr (chunk : Chunk.t) sval_e =
  let open Patterns in
  let return = Delayed.return in
  let* sval_e = Delayed.reduce sval_e in
  let exn = Not_a_C_value sval_e in
  let assert_type ty = Delayed.assert_type sval_e ty exn in
  let error () = raise exn in
  let in_bounds e =
    match Chunk.bounds chunk with
    | Some (low, high) ->
        let open Formula.Infix in
        e #>= (Expr.int_z low) #&& (e #<= (Expr.int_z high))
    | None -> True
  in
  match (chunk, !Kconfig.archi) with
  | U64, Arch64 | U32, Arch32 ->
      (* It has to be either an integer or a pointer *)
      if%ent integer sval_e then
        if%ent in_bounds sval_e then return sval_e else error ()
      else
        if%ent obj sval_e then
          let loc_expr = Expr.list_nth sval_e 0 in
          let ofs = Expr.list_nth sval_e 1 in
          let* ofs = Delayed.reduce ofs in
          let* loc_opt = Delayed.resolve_loc loc_expr in
          let loc, learned =
            match loc_opt with
            | Some l -> (Expr.loc_from_loc_name l, [])
            | None ->
                let aloc = Expr.ALoc (ALoc.alloc ()) in
                let learned =
                  let open Formula.Infix in
                  [ loc_expr #== aloc ]
                in
                (aloc, learned)
          in
          return ~learned (Expr.EList [ loc; ofs ])
        else raise (Not_a_C_value sval_e)
  | (U8 | I8 | U16 | I16 | I32 | U32 | I64 | U64 | I128 | U128), _ ->
      let* () = assert_type IntType in
      let+ () = Delayed.assert_ (in_bounds sval_e) exn in
      sval_e
  | F64, _ ->
      let+ () = assert_type NumberType in
      sval_e
  | F32, _ ->
      let+ () = assert_type NumberType in
      sval_e

let sure_is_zero = function
  | Expr.Lit (Int z) when Z.equal z Z.zero -> true
  | Lit (Num 0.) -> true
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
        (* What's inside is technically already an sval,
           be we don't check sometimes so we might as well here.. *)
        let+ v = of_chunk_and_expr chunk a in
        Some v
    | AllZeros -> DO.some (zero_of_chunk chunk)
    | _ -> DO.none ()

  let singleton e = Arr (Expr.EList [ e ])

  let array_sub arr o len : t =
    match arr with
    | AllZeros -> AllZeros
    | AllUndef -> AllUndef
    | Arr e -> Arr (Expr.list_sub ~lst:e ~start:o ~size:len)

  (** This assumes chunks are properly respected outside of the call of this function *)
  let array_cons (el : Expr.t) arr = concat (singleton el) arr

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

  let of_gil_expr_exn expr = (* TODO: at type sanity check here *) Arr expr

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
