open Gil_syntax
module GType = Goto_lib.Type
module LActions = Cgil_lib.LActions

type chunk =
  | Int8signed
  | Int8unsigned
  | Int16signed
  | Int16unsigned
  | Int32
  | Int64
  | Float32
  | Float64

let chunk_to_string = function
  | Int8signed -> "int8signed"
  | Int8unsigned -> "int8unsigned"
  | Int16signed -> "int16signed"
  | Int16unsigned -> "int16unsigned"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Float32 -> "float32"
  | Float64 -> "float64"

(** Loads a value into the given variable.
    If no variable is given, one is created.
    In any case, the variable containing the value, as well as
    the load command is returned.
    If a variable is given, the returned variable
    is always equal to it *)
let load_scalar ~ctx ?var (e : Expr.t) (t : GType.t) =
  let chunk = chunk_for_type t in
  let var =
    match var with
    | Some var -> var
    | None -> Ctx.fresh_v ctx
  in
  let load_action = LActions.str_ac (AMem Load) in
  ()
