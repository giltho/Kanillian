type t = Expr of Expr.t | Stmt of Stmt.t | SVNone

let of_irep ~(machine : Machine_model.t) ~(type_ : Type.t) (irep : Irep.t) =
  if Irep.is_nil irep then SVNone
  else
    match type_ with
    | Code _ ->
        let stmt = Stmt.of_irep ~machine irep in
        Stmt stmt
    | _ ->
        let expr = Expr.of_irep ~machine irep in
        Expr expr
