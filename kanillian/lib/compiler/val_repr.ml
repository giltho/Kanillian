open Helpers
open Gil_syntax
module GType = Goto_lib.Type

type t =
  | ByCopy of { ptr : Expr.t; type_ : GType.t }
  | ByValue of Expr.t
  | Procedure of Expr.t
[@@deriving show { with_path = false }, eq]

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

let same_kind a b =
  match (a, b) with
  | ByValue _, ByValue _ | Procedure _, Procedure _ -> true
  | ByCopy { type_ = typea; _ }, ByCopy { type_ = typeb; _ } ->
      GType.equal typea typeb
  | _ -> false

let copy_into (v : t) (x : string) : t Cs.with_cmds =
  match v with
  | Procedure e ->
      let cmd = Cmd.Assignment (x, e) in
      Cs.return ~app:[ cmd ] (Procedure (PVar x))
  | ByValue e ->
      let cmd = Cmd.Assignment (x, e) in
      Cs.return ~app:[ cmd ] (ByValue (PVar x))
  | ByCopy { ptr; type_ } ->
      let cmd = Cmd.Assignment (x, ptr) in
      Cs.return ~app:[ cmd ] (ByCopy { ptr = PVar x; type_ })
