type body =
  | Decl of { lhs : Expr.t; value : Expr.t option }
  (* | Assign of { lhs : Expr.t; rhs : Expr.t } *)
  | Block of t list
  | Skip
  | Expression of Expr.t
  | Return of Expr.t option

and t = { location : Location.t; body : body }

(** Lifting from Irep *)
open Irep.Infix

let rec body_of_irep ~(machine : Machine_model.t) (irep : Irep.t) : body =
  let of_irep = of_irep ~machine in
  let expr_of_irep = Expr.of_irep ~machine in
  let failwith = Gerror.fail ~irep in
  match (irep $ Statement).id with
  | Skip -> Skip
  | Block ->
      let content = List.map of_irep irep.sub in
      Block content
  | Decl ->
      let lhs, value =
        match irep.sub with
        | [ a; b ] -> (expr_of_irep a, Lift_utils.lift_option expr_of_irep b)
        | [ a ] -> (expr_of_irep a, None)
        | _ -> failwith "Invalid declaration statement!"
      in
      Decl { lhs; value }
  | Return ->
      let ret_value_irep =
        match irep.sub with
        | [] -> Irep.nil
        | [ r ] -> r
        | _ -> failwith "more than one return value"
      in
      let ret_val = Lift_utils.lift_option expr_of_irep ret_value_irep in
      Return ret_val
  | Expression ->
      let irep_expr =
        match irep.sub with
        | [ e ] -> e
        | _ ->
            failwith
              "Don't know what it means to not have just one irep in statement \
               expression"
      in
      Expression (expr_of_irep irep_expr)
  | _ -> failwith "unhandled statement"

and of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let location = Location.sloc_in_irep irep in
  let body = body_of_irep ~machine irep in
  { body; location }
