module GExpr = Goto_lib.Expr
open Gil_syntax

(** Represents how a value is accessed.
    Given the CompCert memory model, I might actually need
    a 3rd variant, representing in-memory access for values that
    have to be copied, not just read *)
type access = InMemoryScalar of Expr.t | Direct of Expr.t

let rec as_access ~ctx (e : GExpr.t) =
  let as_access = as_access ~ctx in
  match e.value with
  | Symbol x ->
      let pvar = Expr.PVar x in
      if Ctx.in_memory ctx x then InMemoryScalar pvar else Direct pvar
  | Dereference e -> (
      match as_access e with
      | Direct e -> failwith "aa"
      | _ -> failwith "bb")
  | _ -> Error.unexpected "Lvalue.as_access for something that isn't an lvalue"
