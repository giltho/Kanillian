open Helpers
open Gil_syntax
module GType = Goto_lib.Type

type t =
  | ByCopy of { ptr : Expr.t; type_ : GType.t }
  | ByValue of Expr.t
  | Procedure of Expr.t
[@@deriving show { with_path = false }]

let as_value ?(error = Error.unexpected) ~msg = function
  | ByValue e -> e
  | _ -> error ("Expected ByValue expressions for " ^ msg)

let as_value_or_unhandled ~feature e =
  match e with
  | ByValue e -> Cs.return e
  | _ ->
      let cmd = assert_unhandled ~feature [] in
      Cs.return ~app:[ cmd ] (Expr.Lit Nono)

let as_procedure e =
  match e with
  | Procedure s -> s
  | _ -> Error.unexpected (Fmt.str "Expected procedure, got %a" pp e)

let dummy ~ctx type_ =
  if Ctx.representable_in_store ctx type_ then ByValue (Expr.Lit Nono)
  else ByCopy { ptr = Expr.Lit Nono; type_ }
