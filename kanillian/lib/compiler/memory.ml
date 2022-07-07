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

let int_chuck_for ~signed ~size =
  match size with
  | 8 -> if signed then Int8signed else Int8unsigned
  | 16 -> if signed then Int16signed else Int16unsigned
  | 32 -> Int32
  | 64 -> Int64
  | _ -> Error.unhandled "int_chuck_for: unsupported size"

let chunk_for_type ~(ctx : Ctx.t) (t : GType.t) =
  match t with
  | CInteger I_bool -> int_chuck_for ~signed:false ~size:ctx.machine.bool_width
  | CInteger I_char ->
      int_chuck_for
        ~signed:(not ctx.machine.char_is_unsigned)
        ~size:ctx.machine.char_width
  | CInteger I_int -> int_chuck_for ~signed:true ~size:ctx.machine.int_width
  | CInteger I_size_t ->
      int_chuck_for ~signed:false ~size:ctx.machine.pointer_width
  | CInteger I_ssize_t ->
      int_chuck_for ~signed:true ~size:ctx.machine.pointer_width
  | Signedbv { width } -> int_chuck_for ~signed:true ~size:width
  | Unsignedbv { width } -> int_chuck_for ~signed:false ~size:width
  | Float -> Float32
  | Double -> Float64
  | Pointer _ -> int_chuck_for ~signed:false ~size:ctx.machine.pointer_width
  | _ ->
      Error.code_error
        ("chunk_for_type: received a type that is not a scalar " ^ GType.show t)

let ptr_add p i =
  let loc = Expr.list_nth p 0 in
  let offset = Expr.list_nth p 1 in
  let open Expr.Infix in
  Expr.EList [ loc; offset + Expr.int i ]

(* Allocates the memory with the right size, and
   returns a location expression, addressing the block *)
let alloc ~loc_var ~size : Expr.t * string Cmd.t =
  let alloc = Cgil_lib.LActions.(str_ac (AMem Alloc)) in
  let cmd = Cmd.LAction (loc_var, alloc, [ Expr.zero_i; Expr.int size ]) in
  let loc = Expr.list_nth (PVar loc_var) 0 in
  (loc, cmd)

(* Allocates the memory with the right size, and
   returns a pointer expression pointing to the
   beginning of the allocated block. *)
let alloc_ptr ~ctx ty : Expr.t * string Cmd.t =
  let size = Ctx.size_of ctx ty in
  let loc, cmd = alloc ~loc_var:(Ctx.fresh_v ctx) ~size in
  let ptr = Expr.EList [ loc; Expr.zero_i ] in
  (ptr, cmd)

let alloc_temp ~ctx ~location ty : Expr.t Cs.with_cmds =
  let ptr, alloc_cmd = alloc_ptr ~ctx ty in
  let temp = Ctx.fresh_v ctx in
  let assign = Cmd.Assignment (temp, ptr) in
  let () = Ctx.register_allocated_temp ctx ~name:temp ~type_:ty ~location in
  Cs.return ~app:[ alloc_cmd; assign ] (Expr.PVar temp)

(** Should only be called for a local that is in memory*)
let dealloc_local ~ctx (l : Ctx.Local.t) : Body_item.t =
  if not (Ctx.in_memory ctx l.symbol) then
    Error.code_error "dealloc_local: local is not in memory";
  let free = Cgil_lib.LActions.(str_ac (AMem Free)) in
  let size = Ctx.size_of ctx l.type_ |> Expr.int in
  let var = Ctx.fresh_v ctx in
  let cmd =
    Cmd.LAction
      (var, free, [ Expr.list_nth (Expr.PVar l.symbol) 0; Expr.zero_i; size ])
  in
  let loc = Body_item.compile_location l.location in
  Body_item.make ~loc ~id:l.location.origin_id cmd

(** Loads a value into the given variable.
    If no variable is given, one is created.
    In any case, the variable containing the value, as well as
    the load command is returned.
    If a variable is given, the returned variable
    is always equal to it *)
let load_scalar ~ctx ?var (e : Expr.t) (t : GType.t) : string Cs.with_cmds =
  let chunk = chunk_for_type ~ctx t in
  let chunk = Expr.Lit (String (chunk_to_string chunk)) in
  let var =
    match var with
    | Some var -> var
    | None -> Ctx.fresh_v ctx
  in
  let loadv = Cgil_lib.CConstants.Internal_Functions.loadv in
  let load_cmd = Cmd.Call (var, Lit (String loadv), [ chunk; e ], None, None) in
  (var, [ load_cmd ])

let store_scalar ~ctx ?var (p : Expr.t) (v : Expr.t) (t : GType.t) :
    string Cmd.t =
  let chunk = chunk_for_type ~ctx t in
  let chunk = Expr.Lit (String (chunk_to_string chunk)) in
  let var =
    match var with
    | Some var -> var
    | None -> Ctx.fresh_v ctx
  in
  let storev = Cgil_lib.CConstants.Internal_Functions.storev in
  let store_cmd =
    Cmd.Call (var, Lit (String storev), [ chunk; p; v ], None, None)
  in
  store_cmd
