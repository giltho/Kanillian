type body =
  | Decl of { lhs : Expr.t; value : Expr.t option }
  (* | Assign of { lhs : Expr.t; rhs : Expr.t } *)
  | Block of t list
  | Skip
  | Return of Expr.t option
(* | Expression of Expr.t *)

and t = { location : Location.t; body : body }

(** Lifting from Irep *)
open Irep.Infix

let rec body_of_irep ~(machine : Machine_model.t) (irep : Irep.t) : body =
  let of_irep = of_irep ~machine in
  let expr_of_irep = Expr.of_irep ~machine in
  match (irep $ Statement).id with
  | Skip -> Skip
  | Block ->
      let content = List.map of_irep irep.sub in
      Block content
  | Decl ->
      let lhs, value =
        match irep.sub with
        | [ a; b ] -> (expr_of_irep a, Lift_utils.lift_option expr_of_irep b)
        | _ -> failwith "Invalid declaration statement!"
      in
      Decl { lhs; value }
  | Return ->
      let ret_irep = List.hd irep.sub in
      let ret_val = Lift_utils.lift_option expr_of_irep ret_irep in
      Return ret_val
  | Assign | _ -> failwith "unhandled statement"

and of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let location = Location.sloc_in_irep irep in
  let body = body_of_irep ~machine irep in
  { body; location }
