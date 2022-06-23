type value = IntConstant of Z.t | BoolConstant of bool | Symbol of string
(* | AddressOf of t *)
(* | Assign of { left : t; right : t } *)
(* | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t } *)

and t = { value : value; type_ : Type.t; location : Location.t }

(** Lifting from Irep *)
open Irep.Infix

let rec value_of_irep
    ~(machine : Machine_model.t)
    ~(type_ : Type.t)
    (irep : Irep.t) =
  match (irep.id, type_) with
  | Constant, CInteger I_bool -> (
      let v =
        irep $ Value
        |> Irep.as_just_bitpattern ~width:machine.bool_width ~signed:false
      in
      match Z.to_int v with
      | 1 -> BoolConstant true
      | 0 -> BoolConstant false
      | _ -> failwith "Invalid bool constant")
  | Constant, CInteger int_ty ->
      (* Importantly, int_ty cannot be bool *)
      let enc = IntType.Bv_encoding.encode ~machine int_ty in
      let v =
        irep $ Value
        |> Irep.as_just_bitpattern ~width:enc.width ~signed:enc.signed
      in
      IntConstant v
  | Symbol, _ ->
      let name = irep $ Identifier |> Irep.as_just_string in
      Symbol name
  | _ -> failwith "unhandled expr value"

and of_irep ~machine irep =
  let location = Location.sloc_in_irep irep in
  let type_ = Type.type_in_irep ~machine irep in
  let value = value_of_irep ~machine ~type_ irep in
  { value; type_; location }
