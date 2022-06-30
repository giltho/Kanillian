module GExpr = Goto_lib.Expr
open Gil_syntax

(** Represents how a value is accessed.
    Given the CompCert memory model, I might actually need
    a 3rd variant, representing in-memory access for values that
    have to be copied, not just read *)
type access =
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | Direct of Expr.t

let rec as_access ~ctx (e : GExpr.t) : access Cs.with_cmds =
  let open Cs.Syntax in
  let as_access = as_access ~ctx in
  match e.value with
  | Symbol x ->
      let pvar = Expr.PVar x in
      if Ctx.in_memory ctx x then
        Cs.return (InMemoryScalar { ptr = pvar; loaded = None })
      else (Direct pvar, [])
  | Dereference e -> (
      let* ge = as_access e in
      match ge with
      | Direct ge ->
          (* We do read the memory, but it might be a "fake read".
             We don't necessarily need the value if we're going to dereference it
              (i.e &*p). *)
          let+ v = Memory.load_scalar ~ctx ge e.type_ in
          InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
      | InMemoryScalar { ptr; loaded } ->
          let* loaded =
            match loaded with
            | None ->
                let+ v = Memory.load_scalar ~ctx ptr e.type_ in
                Expr.PVar v
            | Some loaded -> Cs.return loaded
          in
          let+ v = Memory.load_scalar ~ctx loaded e.type_ in
          InMemoryScalar { ptr = loaded; loaded = Some (PVar v) })
  | _ -> Error.unexpected "Lvalue.as_access for something that isn't an lvalue"
