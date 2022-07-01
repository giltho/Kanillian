type body =
  | Decl of { lhs : Expr.t; value : Expr.t option }
  | Assign of { lhs : Expr.t; rhs : Expr.t }
  | Assume of { cond : Expr.t }
  | Assert of { cond : Expr.t; property_class : string option }
  | Block of t list
  | Label of string * t list
  | Goto of string
  | FunctionCall of { lhs : Expr.t option; func : Expr.t; args : Expr.t list }
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
  | FunctionCall { lhs; func; args } ->
      let pp_lhs ft lhs =
        match lhs with
        | None -> nop ft ()
        | Some lhs -> pf ft "%a = " Expr.pp lhs
      in
      pf ft "@[<h>%a%a(%a);@]" pp_lhs lhs Expr.pp func (list ~sep:comma Expr.pp)
        args
  | Assume { cond } -> pf ft "@[<h>assume(%a);@]" Expr.pp cond
  | Assert { cond; property_class } ->
      let pp_pc ft = function
        | None -> pf ft ""
        | Some s -> pf ft " #%s" s
      in
      pf ft "@[<h>assert(%a);%a@]" Expr.pp cond pp_pc property_class
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

open Lift_utils

let rec body_of_irep ~(machine : Machine_model.t) (irep : Irep.t) : body =
  let of_irep = of_irep ~machine in
  let expr_of_irep = Expr.of_irep ~machine in
  let unexpected = Gerror.unexpected ~irep in
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
        | _ -> unexpected "Invalid declaration statement!"
      in
      Decl { lhs; value }
  | Assign ->
      let lhs, rhs = exactly_two ~msg:"Assign stmt" irep in
      Assign { lhs = expr_of_irep lhs; rhs = expr_of_irep rhs }
  | Assume ->
      let to_assume = exactly_one ~msg:"Assume stmt" irep in
      Assume { cond = expr_of_irep to_assume }
  | Assert ->
      (* I might need to extract the property_class/msg here too *)
      let to_assert = exactly_one ~msg:"Assert stmt" irep in
      let property_class =
        let open Kutils.Syntaxes.Option in
        let* sloc = irep $? CSourceLocation in
        let+ pc = sloc $? PropertyClass in
        Irep.as_just_string pc
      in
      Assert { cond = expr_of_irep to_assert; property_class }
  | Return ->
      let ret_value_irep =
        match irep.sub with
        | [] -> Irep.nil
        | [ r ] -> r
        | _ -> unexpected "more than one return value"
      in
      let ret_val = Lift_utils.lift_option expr_of_irep ret_value_irep in
      Return ret_val
  | Expression ->
      let irep_expr = exactly_one ~msg:"Expression stmt" irep in
      Expression (expr_of_irep irep_expr)
  | Switch ->
      let control, content = exactly_two ~msg:"Switch" irep in
      let control = expr_of_irep control in
      let () =
        match (content $ Statement).id with
        | Block -> ()
        | _ -> unexpected "Switch body is not a block"
      in
      let cases, default = switch_cases_of_irep ~machine content.sub in
      Switch { control; cases; default }
  | FunctionCall ->
      let lhs, func, args = exactly_three ~msg:"FunctionCall stmt" irep in
      let lhs = Lift_utils.lift_option expr_of_irep lhs in
      let func = expr_of_irep func in
      let args = List.map expr_of_irep args.sub in
      FunctionCall { lhs; func; args }
  | id -> Gerror.unhandled ~irep ("statement " ^ Id.to_string id)

and switch_cases_of_irep ~machine l =
  let is_default irep =
    match irep $? Default with
    | Some { id = Id1; _ } -> true
    | _ -> false
  in
  match l with
  | [] -> ([], None)
  | irep :: r when is_default irep -> (
      let unexpected = Gerror.unexpected ~irep in
      let () =
        match (irep $ Statement).id with
        | SwitchCase -> ()
        | _ ->
            unexpected
              "Switch body contains something that is not a switch case"
      in
      let _, stmt = exactly_two ~msg:"default switch_case" irep in
      match switch_cases_of_irep ~machine r with
      | rest, None -> (rest, Some (of_irep ~machine stmt))
      | _, Some _ -> unexpected "two default switch_cases!")
  | irep :: r ->
      let () =
        match (irep $ Statement).id with
        | SwitchCase -> ()
        | _ ->
            Gerror.unexpected ~irep
              "Switch body contains something that is not a switch case"
      in
      let cases, default = switch_cases_of_irep ~machine r in
      let case, body = exactly_two irep in
      let case = Expr.of_irep ~machine case in
      let sw_body = of_irep ~machine body in
      ({ case; sw_body } :: cases, default)

and of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let location = Location.sloc_in_irep irep in
  let body = body_of_irep ~machine irep in
  { body; location }
