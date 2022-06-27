type value =
  | IntConstant of Z.t [@printer Z.pp_print]
  | CBoolConstant of bool
  | BoolConstant of bool
  | Symbol of string
  | FunctionCall of { func : t; args : t list }
  | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
  | ByteExtract of { e : t; offset : int }
  | UnOp of { op : Ops.Unary.t; e : t }
  | Struct of t list
  | AddressOf of t
  | Index of { array : t; index : t }
  | StringConstant of string
  | TypeCast of t

and t = { value : value; type_ : Type.t; location : Location.t }
[@@deriving show { with_path = false }]

let as_symbol e =
  match e.value with
  | Symbol s -> s
  | _ -> Gerror.fail "Expected a symbol, got something else!"

(** Lifting from Irep *)
open Irep.Infix

let rec byte_extract ~(machine : Machine_model.t) irep =
  let e, offset =
    Lift_utils.exactly_two ~failwith:(Gerror.fail ~irep) irep.sub
  in
  let offset =
    offset $ Value
    |> Irep.as_just_bitpattern ~width:machine.pointer_width ~signed:true
    |> Z.to_int
  in
  ByteExtract { e = of_irep ~machine e; offset }

and side_effecting_of_irep ~(machine : Machine_model.t) (irep : Irep.t) =
  let failwith = Gerror.fail ~irep in
  let of_irep = of_irep ~machine in
  match (irep $ Statement).id with
  | FunctionCall ->
      let func, args =
        match irep.sub with
        | [ fsym; args ] -> (of_irep fsym, List.map of_irep args.sub)
        | _ -> failwith "function call with not exactly 2 subs"
      in
      FunctionCall { func; args }
  | _ -> failwith "unknown side-effecting irep"

and lift_binop ~(machine : Machine_model.t) (irep : Irep.t) (op : Ops.Binary.t)
    =
  let of_irep = of_irep ~machine in
  match irep.sub with
  | [ a; b ] -> BinOp { op; lhs = of_irep a; rhs = of_irep b }
  | _ -> Gerror.fail ~irep "Binary operator doesn't have exactly two operands"

and lift_unop ~(machine : Machine_model.t) (irep : Irep.t) (op : Ops.Unary.t) =
  let of_irep = of_irep ~machine in
  match irep.sub with
  | [ a ] -> UnOp { op; e = of_irep a }
  | _ -> Gerror.fail ~irep "Unary operator doesn't have exactly one operand"

and value_of_irep ~(machine : Machine_model.t) ~(type_ : Type.t) (irep : Irep.t)
    =
  let failwith = Gerror.fail ~irep in
  let of_irep = of_irep ~machine in
  let lift_binop = lift_binop ~machine irep in
  let lift_unop = lift_unop ~machine irep in
  let exactly_one = Lift_utils.exactly_one ~failwith in
  let exactly_two = Lift_utils.exactly_two ~failwith in
  match irep.id with
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
          | _ -> failwith "Invalid bool constant")
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
          | _ -> failwith "invalid boolean value")
      | _ -> failwith "cannot handle constant type for now")
  | StringConstant -> StringConstant (irep $ Value |> Irep.as_just_string)
  | ByteExtractBigEndian when machine.is_big_endian ->
      byte_extract ~machine irep
  | ByteExtractLittleEndian when not machine.is_big_endian ->
      byte_extract ~machine irep
  | Symbol ->
      let name = irep $ Identifier |> Irep.as_just_string in
      Symbol name
  | SideEffect -> side_effecting_of_irep ~machine irep
  | AddressOf ->
      let pointee = exactly_one ~msg:"AddressOf" irep.sub in
      AddressOf (of_irep pointee)
  | Struct ->
      let fields = List.map of_irep irep.sub in
      Struct fields
  | Index ->
      let array, index = exactly_two ~msg:"Array Indexing" irep.sub in
      Index { array = of_irep array; index = of_irep index }
  | Typecast ->
      let value = exactly_one ~msg:"Type cast" irep.sub |> of_irep in
      TypeCast value
  (* A bunch of binary operators now*)
  | Le -> lift_binop Le
  | Ge -> lift_binop Ge
  | Lt -> lift_binop Lt
  | Notequal -> lift_binop Notequal
  (* And a bunch of unary operators *)
  | Not -> lift_unop Not
  (* Catch-all *)
  | id -> failwith ("unhandled expr value: " ^ Id.to_string id)

and of_irep ~machine irep =
  let location = Location.sloc_in_irep irep in
  let type_ = Type.type_in_irep ~machine irep in
  let value = value_of_irep ~machine ~type_ irep in
  { value; type_; location }
