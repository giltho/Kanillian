open Gillian.Utils.Prelude

let representable_in_store ~prog ~machine (type_ : Type.t) =
  match type_ with
  | Bool
  | CInteger _
  | Float
  | Double
  | Signedbv _
  | Unsignedbv _
  | Pointer _
  | Empty -> true
  | _ -> Program.is_zst ~prog ~machine type_

module Generators = struct
  let make prefix =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = prefix ^ string_of_int c in
      incr id;
      ret

  let temp_var () = make "temp__"
  let label () = make "cc"
end

module Local = struct
  open Goto_lib

  type t = { symbol : string; type_ : Type.t; location : Location.t }

  (** Returns the locals Hashtbl and the in_memory hashset *)
  let gather ~prog ~machine stmt =
    let locals = Hashtbl.create 1 in
    let in_memory = Hashset.empty () in
    let visitor =
      object
        inherit [bool] Goto_lib.Visitors.iter as super

        method! visit_expr_value ~ctx ~type_ (e : Expr.value) =
          match e with
          | Member { lhs = e; _ }
          | Index { array = e; _ }
          | AddressOf e
          | ByteExtract { e; _ } -> super#visit_expr ~ctx:true e
          | Symbol x
            when ctx || not (representable_in_store ~prog ~machine type_) ->
              Hashset.add in_memory x
          | _ -> super#visit_expr_value ~ctx ~type_ e

        method! visit_stmt_body ~ctx (s : Stmt.body) =
          match s with
          | Decl { lhs = { value = Symbol x; type_; location } as lhs; value }
            ->
              Hashtbl.replace locals x { symbol = x; type_; location };
              super#visit_expr ~ctx lhs;
              Option.iter (super#visit_expr ~ctx) value
          | _ -> super#visit_stmt_body ~ctx s
      end
    in
    visitor#visit_stmt ~ctx:false stmt;
    (locals, in_memory)
end

type t = {
  machine : Machine_model.t;
  prog : Program.t;
  fresh_v : unit -> string;
  in_memory : string Hashset.t;
  locals : (string, Local.t) Hashtbl.t;
  allocated_temps : Local.t Hashset.t;
  fresh_lab : unit -> string;
  harness : string option;
  break_lab : string option;
}

let make ~machine ~prog ~harness () =
  {
    allocated_temps = Hashset.empty ~size:32 ();
    locals = Hashtbl.create 0;
    in_memory = Hashset.empty ~size:0 ();
    machine;
    prog;
    fresh_v = (fun () -> failwith "uninitialized var generator");
    fresh_lab = Generators.label ();
    harness;
    break_lab = None;
  }

let with_new_generators t = { t with fresh_v = Generators.temp_var () }
let fresh_v t = t.fresh_v ()
let fresh_lab t = t.fresh_lab ()
let in_memory t x = Hashset.mem t.in_memory x
let is_local t x = Hashtbl.mem t.locals x
let tag_lookup ctx x = Hashtbl.find ctx.prog.types x

let resolve_type ctx t =
  match t with
  | Type.StructTag tag -> tag_lookup ctx tag
  | Type.UnionTag tag -> tag_lookup ctx tag
  | _ -> t

let register_allocated_temp ctx ~name:symbol ~type_ ~location =
  let local = Local.{ symbol; type_; location } in
  Hashset.add ctx.allocated_temps local

let rec resolve_struct_components ctx (ty : Type.t) =
  let tag_lookup x = tag_lookup ctx x in
  match ty with
  | Struct { components; _ } -> components
  | StructTag x -> resolve_struct_components ctx (tag_lookup x)
  | _ -> Error.code_error "resolve_struct_components for non-struct"

let size_of ctx ty =
  Error.rethrow_gerror (fun () ->
      let tag_lookup x = tag_lookup ctx x in
      let machine = ctx.machine in
      Type.size_of ~tag_lookup ~machine ty)

let offset_struct_field ctx ty field =
  Error.rethrow_gerror (fun () ->
      let tag_lookup x = tag_lookup ctx x in
      let machine = ctx.machine in
      Type.offset_struct_field ~tag_lookup ~machine ty field)

let is_zst_access ctx (ty : Type.t) =
  match ty with
  | Bool | Code _ -> false
  | _ -> size_of ctx ty == 0

let representable_in_store ctx ty =
  representable_in_store ~machine:ctx.machine ~prog:ctx.prog ty

let with_entering_body ctx ~body ~params ~location =
  (* FIXME: params in memory should be assigned when entering function body! *)
  let locals, in_memory =
    Local.gather ~machine:ctx.machine ~prog:ctx.prog body
  in
  List.iter
    (fun (p : Param.t) ->
      Option.iter
        (fun symbol ->
          Hashtbl.add locals symbol Local.{ symbol; type_ = p.type_; location })
        p.identifier)
    params;
  let allocated_temps = Hashset.empty ~size:32 () in
  let fresh_v = Generators.temp_var () in
  { ctx with in_memory; locals; fresh_v; allocated_temps }

let archi ctx : Archi.t =
  match ctx.machine.Machine_model.pointer_width with
  | 32 -> Arch32
  | 64 -> Arch64
  | _ -> Error.unexpected "Unknown pointer width"

let with_break ctx lab f =
  let ctx = { ctx with break_lab = Some lab } in
  f ctx

let is_function_symbol ctx s =
  match s with
  | "__CPROVER_assert"
  | "__CPROVER_assume"
  | "__CPROVER_initialize"
  | "__CPROVER__start" -> true
  | _ -> Hashtbl.mem ctx.prog.funs s

let type_equal ctx ta tb =
  Type.equal (resolve_type ctx ta) (resolve_type ctx tb)

let ptr_chunk ctx =
  match archi ctx with
  | Arch32 -> Chunk.U32
  | Arch64 -> Chunk.U64

let ptr_64 ctx =
  match archi ctx with
  | Arch32 -> false
  | Arch64 -> true
