open Helpers
open Gil_syntax
module GType = Goto_lib.Type

type t = ByCopy of { ptr : Expr.t; type_ : GType.t } | ByValue of Expr.t
[@@deriving show { with_path = false }]

let as_value ?(error = Error.unexpected) ~msg = function
  | ByValue e -> e
  | ByCopy _ -> error ("Expected ByValue expressions for " ^ msg)

let as_value_or_unhandled ~feature e =
  match e with
  | ByValue e -> Cs.return e
  | ByCopy _ ->
      let cmd = assert_unhandled ~feature [] in
      Cs.return ~app:[ cmd ] (Expr.Lit Nono)

let dummy ~ctx type_ =
  if Ctx.representable_in_store ctx type_ then ByValue (Expr.Lit Nono)
  else ByCopy { ptr = Expr.Lit Nono; type_ }
