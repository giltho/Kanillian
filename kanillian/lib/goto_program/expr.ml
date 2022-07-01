type value =
  | Array of t list
  | IntConstant of Z.t [@printer Z.pp_print]
  | CBoolConstant of bool
  | BoolConstant of bool
  | PointerConstant of int
  | Symbol of string
  | FunctionCall of { func : t; args : t list }
  | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
  | ByteExtract of { e : t; offset : int }
  | Dereference of t
  | Assign of { lhs : t; rhs : t }
  | UnOp of { op : Ops.Unary.t; e : t }
  | Struct of t list
  | Member of { lhs : t; field : string }
  | AddressOf of t
  | Index of { array : t; index : t }
  | StringConstant of string
  | TypeCast of t
  | Nondet

and t = { value : value; type_ : Type.t; location : Location.t }
[@@deriving show { with_path = false }]

let pp ft t =
  let rec pp ft t =
    let open Fmt in
    match t.value with
    | Array x -> pf ft "%a" (list ~sep:comma pp) x
    | Assign { lhs; rhs } -> pf ft "%a = %a" pp lhs pp rhs
    | IntConstant z -> pf ft "%a" Z.pp_print z
    | CBoolConstant b -> pf ft "%d" (if b then 1 else 0)
    | PointerConstant 0 -> pf ft "NULL"
    | PointerConstant k -> pf ft "POINTER(%d)" k
    | Symbol s -> pf ft "%s" s
    | FunctionCall { func; args } ->
        pf ft "%a(%a)" pp func (list ~sep:comma pp) args
    | BinOp { op; lhs; rhs } ->
        pf ft "(%a %a %a)" pp lhs Ops.Binary.pp op pp rhs
    | UnOp { op; e } -> pf ft "(%a %a)" Ops.Unary.pp op pp e
    | ByteExtract { e; offset } ->
        pf ft "EXTRACT(%a, %a, %d)" pp e Type.pp t.type_ offset
    | Struct xs -> pf ft "{ %a }" (list ~sep:semi pp) xs
    | Member { lhs; field } -> pf ft "%a.%s" pp lhs field
    | Index { array; index } -> pf ft "%a[%a]" pp array pp index
    | StringConstant s -> pf ft "\"%s\"" s
    | TypeCast value -> pf ft "((%a) %a)" Type.pp t.type_ pp value
    | Nondet -> pf ft "NONDET"
    | BoolConstant b -> pf ft "%b" b
    | AddressOf e -> pf ft "&%a" pp e
    | Dereference e -> pf ft "*%a" pp e
  in

  (Fmt.hbox pp) ft t

let show = Fmt.to_to_string pp

let as_symbol e =
  match e.value with
  | Symbol s -> s
  | _ -> Gerror.unexpected "Expected a symbol, got something else!"

open Irep.Infix
open Lift_utils

(** Lifting from Irep *)
let rec byte_extract_of_irep ~(machine : Machine_model.t) irep =
  let e, offset = exactly_two irep in
  let offset =
    offset $ Value
    |> Irep.as_just_bitpattern ~width:machine.pointer_width ~signed:true
    |> Z.to_int
  in
  ByteExtract { e = of_irep ~machine e; offset }

and side_effecting_of_irep ~(machine : Machine_model.t) (irep : Irep.t) =
  let of_irep = of_irep ~machine in
  match (irep $ Statement).id with
  | FunctionCall ->
      let func, args =
        match irep.sub with
        | [ fsym; args ] -> (of_irep fsym, List.map of_irep args.sub)
        | _ -> Gerror.unexpected ~irep "function call with not exactly 2 subs"
      in
      FunctionCall { func; args }
  | Nondet -> Nondet
  | Assign ->
      let lhs, rhs = exactly_two irep in
      Assign { lhs = of_irep lhs; rhs = of_irep rhs }
  | id ->
      Gerror.unhandled ~irep ("unknown side-effecting irep: " ^ Id.to_string id)

and lift_binop ~(machine : Machine_model.t) (irep : Irep.t) (op : Ops.Binary.t)
    =
  let of_irep = of_irep ~machine in
  match irep.sub with
  | [ a; b ] -> BinOp { op; lhs = of_irep a; rhs = of_irep b }
  | _ ->
      Gerror.unexpected ~irep
        "Binary operator doesn't have exactly two operands"

and lift_unop ~(machine : Machine_model.t) (irep : Irep.t) (op : Ops.Unary.t) =
  let of_irep = of_irep ~machine in
  match irep.sub with
  | [ a ] -> UnOp { op; e = of_irep a }
  | _ ->
      Gerror.unexpected ~irep "Unary operator doesn't have exactly one operand"

and value_of_irep ~(machine : Machine_model.t) ~(type_ : Type.t) (irep : Irep.t)
    =
  let unexpected = Gerror.unexpected ~irep in
  let unhandled = Gerror.unhandled ~irep in
  let of_irep = of_irep ~machine in
  let lift_binop = lift_binop ~machine irep in
  let lift_unop = lift_unop ~machine irep in
  match irep.id with
  | Array -> Array (List.map of_irep irep.sub)
  | Constant -> (
      match type_ with
      | CInteger I_bool -> (
          let v =
            irep $ Value
            |> Irep.as_just_bitpattern ~width:machine.bool_width ~signed:false
          in
          match Z.to_int v with
          | 1 -> CBoolConstant true
          | 0 -> CBoolConstant false
          | _ -> unexpected "Invalid bool constant")
      | CInteger int_ty ->
          (* Importantly, int_ty cannot be bool *)
          let enc = IntType.Bv_encoding.encode ~machine int_ty in
          let v =
            irep $ Value
            |> Irep.as_just_bitpattern ~width:enc.width ~signed:enc.signed
          in
          IntConstant v
      | Bool -> (
          match (irep $ Value).id with
          | True -> BoolConstant true
          | False -> BoolConstant false
          | _ -> unexpected "invalid boolean value")
      | Unsignedbv { width } ->
          let v =
            irep $ Value |> Irep.as_just_bitpattern ~width ~signed:false
          in
          IntConstant v
      | Signedbv { width } ->
          let v = irep $ Value |> Irep.as_just_bitpattern ~width ~signed:true in
          IntConstant v
      | Pointer _ -> (
          match (irep $ Value).id with
          | NULL -> PointerConstant 0
          | _ -> unhandled "Pointer constant that is not NULL")
      | _ -> unhandled "Cannot handle this constant of this type yet")
  | StringConstant -> StringConstant (irep $ Value |> Irep.as_just_string)
  | ByteExtractBigEndian when machine.is_big_endian ->
      byte_extract_of_irep ~machine irep
  | ByteExtractLittleEndian when not machine.is_big_endian ->
      byte_extract_of_irep ~machine irep
  | Symbol ->
      let name = irep $ Identifier |> Irep.as_just_string in
      Symbol name
  | Dereference -> Dereference (of_irep (exactly_one irep))
  | SideEffect -> side_effecting_of_irep ~machine irep
  | AddressOf ->
      let pointee = exactly_one ~msg:"AddressOf" irep in
      AddressOf (of_irep pointee)
  | Struct ->
      let fields = List.map of_irep irep.sub in
      Struct fields
  | Member ->
      let lhs = exactly_one ~msg:"Member" irep |> of_irep in
      let field = irep $ ComponentName |> Irep.as_just_string in
      Member { lhs; field }
  | Index ->
      let array, index = exactly_two ~msg:"Array Indexing" irep in
      Index { array = of_irep array; index = of_irep index }
  | Typecast ->
      let value = exactly_one ~msg:"Type cast" irep |> of_irep in
      TypeCast value
  | Nondet -> Nondet
  (* A bunch of binary operators now*)
  | And -> lift_binop And
  | Ashr -> lift_binop Ashr
  | Bitand -> lift_binop Bitand
  | Bitor -> lift_binop Bitor
  | Bitnand -> lift_binop Bitnand
  | Bitxor -> lift_binop Bitxor
  | Div -> lift_binop Div
  | Equal -> lift_binop Equal
  | Ge -> lift_binop Ge
  | Gt -> lift_binop Gt
  | IeeeFloatEqual -> lift_binop IeeeFloatEqual
  | IeeeFloatNotequal -> lift_binop IeeeFloatNotequal
  | Implies -> lift_binop Implies
  | Le -> lift_binop Le
  | Lshr -> lift_binop Lshr
  | Lt -> lift_binop Lt
  | Minus -> lift_binop Minus
  | Mod -> lift_binop Mod
  | Mult -> lift_binop Mult
  | Notequal -> lift_binop Notequal
  | Or -> lift_binop Or
  | OverflowMinus -> lift_binop OverflowMinus
  | OverflowMult -> lift_binop OverflowMult
  | OverflowPlus -> lift_binop OverflowPlus
  | Plus -> lift_binop Plus
  | ROk -> lift_binop ROk
  | Rol -> lift_binop Rol
  | Ror -> lift_binop Ror
  | Shl -> lift_binop Shl
  | Xor -> lift_binop Xor
  (* And a bunch of unary operators *)
  | Bitnot -> lift_unop Bitnot
  | BitReverse -> lift_unop BitReverse
  | Bswap -> lift_unop Bswap
  | IsDynamicObject -> lift_unop IsDynamicObject
  | IsFinite -> lift_unop IsFinite
  | Not -> lift_unop Not
  | ObjectSize -> lift_unop ObjectSize
  | PointerObject -> lift_unop PointerObject
  | PointerOffset -> lift_unop PointerOffset
  | Popcount -> lift_unop Popcount
  | UnaryMinus -> lift_unop UnaryMinus
  (* Catch-all *)
  | id -> unhandled ("unhandled expr value: " ^ Id.to_string id)

and of_irep ~machine irep =
  let location = Location.sloc_in_irep irep in
  let type_ = Type.type_in_irep ~machine irep in
  let value = value_of_irep ~machine ~type_ irep in
  { value; type_; location }
