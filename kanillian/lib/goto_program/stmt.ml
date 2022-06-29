type body =
  | Decl of { lhs : Expr.t; value : Expr.t option }
  | Assign of { lhs : Expr.t; rhs : Expr.t }
  | Assume of { cond : Expr.t }
  | Assert of { cond : Expr.t }
  | Block of t list
  | Label of string * t list
  | Goto of string
  | Switch of { control : Expr.t; cases : switch_case list; default : t option }
  | Skip
  | Expression of Expr.t
  | Return of Expr.t option

and switch_case = { case : Expr.t; sw_body : t }
and t = { location : Location.t; body : body }

let rec pp ft (t : t) =
  let open Fmt in
  match t.body with
  | Decl { lhs; value } ->
      pf ft "@[<h>%a %a%a;@]" Type.pp lhs.type_ Expr.pp lhs
        (fun ft -> function
          | None -> ()
          | Some e -> pf ft " = %a" Expr.pp e)
        value
  | Assign { lhs; rhs } -> pf ft "@[<h>%a = %a;@]" Expr.pp lhs Expr.pp rhs
  | Assume { cond } -> pf ft "@[<h>assume(%a);@]" Expr.pp cond
  | Assert { cond } -> pf ft "@[<h>assert(%a);@]" Expr.pp cond
  | Block body -> pf ft "@[<v 3>{ %a };@]" (Fmt.list ~sep:cut pp) body
  | Label (label, body) ->
      pf ft "@[<v 3>%s: {@.%a};@]" label (Fmt.list ~sep:cut pp) body
  | Skip -> pf ft "skip;"
  | Expression e -> pf ft "@[<v 3>{ %a };@]" Expr.pp e
  | Return e -> pf ft "@[<v 3>return %a;@]" (option Expr.pp) e
  | Goto label -> pf ft "@[<v 3>goto %s;@]" label
  | Switch _ -> pf ft "switch"

(** Lifting from Irep *)
open Irep.Infix

let rec body_of_irep ~(machine : Machine_model.t) (irep : Irep.t) : body =
  let of_irep = of_irep ~machine in
  let expr_of_irep = Expr.of_irep ~machine in
  let exactly_two = Lift_utils.exactly_two ~failwith in
  let failwith = Gerror.fail ~irep in
  match (irep $ Statement).id with
  | Skip -> Skip
  | Goto ->
      let label = irep $ Destination |> Irep.as_just_string in
      Goto label
  | Block ->
      let content = List.map of_irep irep.sub in
      Block content
  | Label ->
      let lab = irep $ Label |> Irep.as_just_string in
      let block = List.map of_irep irep.sub in
      Label (lab, block)
  | Decl ->
      let lhs, value =
        match irep.sub with
        | [ a; b ] -> (expr_of_irep a, Lift_utils.lift_option expr_of_irep b)
        | [ a ] -> (expr_of_irep a, None)
        | _ -> failwith "Invalid declaration statement!"
      in
      Decl { lhs; value }
  | Assign ->
      let lhs, rhs =
        match irep.sub with
        | [ a; b ] -> (expr_of_irep a, expr_of_irep b)
        | _ -> failwith "Assign stmt doesn't have two operands"
      in
      Assign { lhs; rhs }
  | Assume ->
      let to_assume =
        match irep.sub with
        | [ a ] -> expr_of_irep a
        | _ -> failwith "Assume that doesn't have one operand"
      in
      Assume { cond = to_assume }
  | Assert ->
      (* I might need to extract the property_class/msg here too *)
      let to_assert =
        match irep.sub with
        | [ a ] -> expr_of_irep a
        | _ -> failwith "Assert that doesn't have one operand"
      in
      Assume { cond = to_assert }
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
  | Switch ->
      let control, content = exactly_two ~msg:"Switch" irep.sub in
      let control = expr_of_irep control in
      let () =
        match (content $ Statement).id with
        | Block -> ()
        | _ -> failwith "Switch body is not a block"
      in
      let cases, default = switch_cases_of_irep ~machine content.sub in
      Switch { control; cases; default }
  | _ -> failwith "unhandled statement"

and switch_cases_of_irep ~machine l =
  let is_default irep =
    match irep $? Default with
    | Some { id = Id1; _ } -> true
    | _ -> false
  in
  match l with
  | [] -> ([], None)
  | a :: r when is_default a -> (
      let exactly_two = Lift_utils.exactly_two ~failwith in
      let failwith = Gerror.fail ~irep:a in
      let () =
        match (a $ Statement).id with
        | SwitchCase -> ()
        | _ ->
            failwith "Switch body contains something that is not a switch case"
      in
      let _, stmt = exactly_two ~msg:"default switch_case" a.sub in
      match switch_cases_of_irep ~machine r with
      | rest, None -> (rest, Some (of_irep ~machine stmt))
      | _, Some _ -> failwith "two default switch_cases!")
  | a :: r ->
      let exactly_two = Lift_utils.exactly_two ~failwith in
      let failwith = Gerror.fail ~irep:a in
      let () =
        match (a $ Statement).id with
        | SwitchCase -> ()
        | _ ->
            failwith "Switch body contains something that is not a switch case"
      in
      let cases, default = switch_cases_of_irep ~machine r in
      let case, body = exactly_two a.sub in
      let case = Expr.of_irep ~machine case in
      let sw_body = of_irep ~machine body in
      ({ case; sw_body } :: cases, default)

and of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let location = Location.sloc_in_irep irep in
  let body = body_of_irep ~machine irep in
  { body; location }