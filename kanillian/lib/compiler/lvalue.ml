module GExpr = Goto_lib.Expr
open Gil_syntax

(** Represents how a value is accessed.
    Given the CompCert memory model, I might actually need
    a 3rd variant, representing in-memory access for values that
    have to be copied, not just read.
    Moreover, InMemoryScalar should probably directly be aware of
    the chunk *)
type access =
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | Direct of string

let rec as_access ~ctx ~read (e : GExpr.t) : access Cs.with_body =
  let open Cs.Syntax in
  let b =
    Body_item.make
      ~loc:(Body_item.compile_location e.location)
      ~id:e.location.origin_id
  in
  let lift x = Cs.map_l b x in
  let as_access = as_access ~ctx ~read in
  match e.value with
  | Symbol x ->
      if Ctx.is_local ctx x then
        if Ctx.in_memory ctx x then
          Cs.return (InMemoryScalar { ptr = PVar x; loaded = None })
        else (Direct x, [])
      else
        let genvlookup = Cgil_lib.LActions.(str_ac (AGEnv GetSymbol)) in
        let sym_and_loc = Ctx.fresh_v ctx in
        let act = Cmd.LAction (sym_and_loc, genvlookup, [ Lit (String x) ]) in
        let ptr = Ctx.fresh_v ctx in
        let assign =
          Cmd.Assignment
            (ptr, EList [ Expr.list_nth (PVar sym_and_loc) 1; Expr.zero_i ])
        in
        Cs.return
          ~app:[ b act; b assign ]
          (InMemoryScalar { ptr = PVar ptr; loaded = None })
  | Dereference e -> (
      let* ge = as_access e in
      match ge with
      | Direct x ->
          (* We do read the memory, but it might be a "fake read".
             We don't necessarily need the value if we're going to dereference it
              (i.e &*p). Therefore, we keep both the value and the pointer around. *)
          let ge = Expr.PVar x in
          if read then
            let+ v = Memory.load_scalar ~ctx ge e.type_ |> Cs.map_l b in
            InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
          else Cs.return (InMemoryScalar { ptr = ge; loaded = None })
      | InMemoryScalar { ptr; loaded } ->
          let* loaded =
            lift
            @@
            match loaded with
            | None ->
                let+ v = Memory.load_scalar ~ctx ptr e.type_ in
                Expr.PVar v
            | Some loaded -> Cs.return loaded
          in
          if read then
            let+ v = Memory.load_scalar ~ctx loaded e.type_ |> lift in
            InMemoryScalar { ptr = loaded; loaded = Some (PVar v) }
          else Cs.return (InMemoryScalar { ptr = loaded; loaded = None }))
  (* Not sure about those 2 actually.
     The parameter in there, is it a pointer? *)
  | Index _ -> Error.unhandled "array index lvalue"
  | Member _ -> Error.unhandled "member lvalue"
  | _ -> Error.unexpected "Lvalue.as_access for something that isn't an lvalue"
