type value =
  | IntConstant of Z.t [@printer Z.pp_print]
  | BoolConstant of bool
  | Symbol of string
  | FunctionCall of { func : t; args : t list }
  | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
  | AddressOf of t
  | Index of { array : t; index : t }
  | StringConstant of string
(* | Assign of { left : t; right : t } *)

and t = { value : value; type_ : Type.t; location : Location.t }
[@@deriving show { with_path = false }]

let as_symbol e =
  match e.value with
  | Symbol s -> s
  | _ -> Gerror.fail "Expected a symbol, got something else!"

(** Lifting from Irep *)
open Irep.Infix

let rec side_effecting_of_irep ~(machine : Machine_model.t) (irep : Irep.t) =
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

and value_of_irep ~(machine : Machine_model.t) ~(type_ : Type.t) (irep : Irep.t)
    =
  let failwith = Gerror.fail ~irep in
  let of_irep = of_irep ~machine in
  let lift_binop = lift_binop ~machine irep in
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
          | 1 -> BoolConstant true
          | 0 -> BoolConstant false
          | _ -> failwith "Invalid bool constant")
      | CInteger int_ty ->
          (* Importantly, int_ty cannot be bool *)
          let enc = IntType.Bv_encoding.encode ~machine int_ty in
          let v =
            irep $ Value
            |> Irep.as_just_bitpattern ~width:enc.width ~signed:enc.signed
          in
          IntConstant v
      | _ -> failwith "cannot handle constant of non-integer types for now")
  | StringConstant -> StringConstant (irep $ Value |> Irep.as_just_string)
  | Symbol ->
      let name = irep $ Identifier |> Irep.as_just_string in
      Symbol name
  | SideEffect -> side_effecting_of_irep ~machine irep
  | AddressOf ->
      let pointee = exactly_one ~msg:"AddressOf" irep.sub in
      AddressOf (of_irep pointee)
  | Index ->
      let array, index = exactly_two ~msg:"Array Indexing" irep.sub in
      Index { array = of_irep array; index = of_irep index }
  (* A bunch of binary operators now*)
  | Le -> lift_binop Le
  | Ge -> lift_binop Ge
  (* Catch-all *)
  | id -> failwith ("unhandled expr value: " ^ Id.to_string id)

and of_irep ~machine irep =
  let location = Location.sloc_in_irep irep in
  let type_ = Type.type_in_irep ~machine irep in
  let value = value_of_irep ~machine ~type_ irep in
  { value; type_; location }
