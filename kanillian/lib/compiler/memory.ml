open Gil_syntax
module GType = Goto_lib.Type
module Interface = Memory_model.Interface

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
  | 8 -> Some (if signed then Int8signed else Int8unsigned)
  | 16 -> Some (if signed then Int16signed else Int16unsigned)
  | 32 -> Some Int32
  | 64 -> Some Int64
  | _ -> None

let chunk_for_type ~(ctx : Ctx.t) (t : GType.t) : chunk option =
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
  | Float -> Some Float32
  | Double -> Some Float64
  | Pointer _ -> int_chuck_for ~signed:false ~size:ctx.machine.pointer_width
  | _ ->
      Error.code_error
        ("chunk_for_type: received a type that is not a scalar " ^ GType.show t)

let ptr_add_e p e =
  let loc = Expr.list_nth p 0 in
  let offset = Expr.list_nth p 1 in
  let open Expr.Infix in
  Expr.EList [ loc; offset + e ]

let ptr_add p i = ptr_add_e p (Expr.int i)

let ptr_add_v p v =
  let v = Expr.list_nth v 1 in
  ptr_add_e p v

(* Allocates the memory with the right size, and
   returns a location expression, addressing the block *)
let alloc ~loc_var ~size : Expr.t * string Cmd.t =
  let alloc = Interface.(str_ac (AMem Alloc)) in
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
  let free = Interface.(str_ac (AMem Free)) in
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
  match chunk_for_type ~ctx t with
  | None ->
      let cmd = Helpers.assert_unhandled ~feature:(LoadScalar t) [ e ] in
      Cs.return ~app:[ cmd ] "UNREACHABLE"
  | Some chunk ->
      let chunk = Expr.Lit (String (chunk_to_string chunk)) in
      let var =
        match var with
        | Some var -> var
        | None -> Ctx.fresh_v ctx
      in
      let loadv = Constants.Internal_functions.loadv in
      let load_cmd =
        Cmd.Call (var, Lit (String loadv), [ chunk; e ], None, None)
      in
      (var, [ load_cmd ])

let store_scalar ~ctx ?var (p : Expr.t) (v : Expr.t) (t : GType.t) :
    string Cmd.t =
  match chunk_for_type ~ctx t with
  | None -> Helpers.assert_unhandled ~feature:(StoreScalar t) []
  | Some chunk ->
      let chunk = Expr.Lit (String (chunk_to_string chunk)) in
      let var =
        match var with
        | Some var -> var
        | None -> Ctx.fresh_v ctx
      in
      let storev = Constants.Internal_functions.storev in
      let store_cmd =
        Cmd.Call (var, Lit (String storev), [ chunk; p; v ], None, None)
      in
      store_cmd

let memcpy ~ctx ~(type_ : GType.t) ~(dst : Expr.t) ~(src : Expr.t) =
  let temp = Ctx.fresh_v ctx in
  let size = Ctx.size_of ctx type_ in
  let memcpy = Constants.Internal_functions.ef_memcpy in
  (* TODO: emit a signal that alignment check is not performed correctly *)
  Cmd.Call
    ( temp,
      Lit (String memcpy),
      [ Expr.int size; Expr.zero_i; dst; src ],
      None,
      None )

(* let write_composit
     ~ctx
     ~(type_ : GType.t)
     ~(dst : Expr.t)
     (writes : (int * Val_repr.t Cs.with_cmds) Seq.t) : string Cmd.t list =
   [] *)

let write_composit
    ~ctx
    ~annot
    ~dst
    (writes : (int * Val_repr.composit_write) Seq.t) =
  let loc = Expr.list_nth dst 0 in
  let base_ofs = Expr.list_nth dst 1 in
  let at_ofs i = Expr.EList [ loc; Expr.Infix.( + ) base_ofs (Expr.int i) ] in
  let rec aux start_ofs writes =
    match writes () with
    | Seq.Nil -> []
    | Cons ((_, Val_repr.Poison _), rest) ->
        (* TODO: Once poison is implemented in Gillian-C,
           a poison write should actively poison memory.
           Maybe Poison should directly be a variant of Val_repr, but maybe not.
           To be understood *)
        aux start_ofs rest
    | Cons ((i, V { type_ = ty; value = v, body }), rest) ->
        let curr_ofs = start_ofs + i in
        let dst = at_ofs curr_ofs in
        let cmds =
          match v with
          | Val_repr.ByCopy { ptr = src; type_ } ->
              [ annot (memcpy ~ctx ~dst ~src ~type_) ]
          | Val_repr.ByValue e -> [ annot (store_scalar ~ctx dst e ty) ]
          | Val_repr.ByCompositValue { writes; _ } -> aux curr_ofs writes
          | Val_repr.Procedure _ ->
              Error.code_error "Writing a procedure in a composit"
        in
        body @ cmds @ aux start_ofs rest
  in
  aux 0 writes

let write ~ctx ~(type_ : GType.t) ~annot ~(dst : Expr.t) ~(src : Val_repr.t) :
    Body_item.t list =
  if Ctx.is_zst_access ctx type_ then []
  else
    match src with
    | Val_repr.ByCopy { ptr = src; type_ } ->
        [ annot (memcpy ~ctx ~dst ~src ~type_) ]
    | Val_repr.ByValue e -> [ annot (store_scalar ~ctx dst e type_) ]
    | Val_repr.ByCompositValue { writes; _ } ->
        write_composit ~ctx ~annot ~dst writes
    | Val_repr.Procedure _ -> Error.code_error "Writing a procedure"
