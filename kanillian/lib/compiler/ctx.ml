open Gillian.Utils.Prelude

let representable_in_store ~prog (type_ : Type.t) =
  match type_ with
  | Bool
  | CInteger _
  | Float
  | Double
  | Signedbv _
  | Unsignedbv _
  | Pointer _
  | Empty -> true
  | _ -> Program.is_zst ~prog type_

module Generators = struct
  let make prefix =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = prefix ^ string_of_int c in
      incr id;
      ret

  let temp_var () = make "temp__"
  let temp_lvar () = make "#lvar_"
  let label () = make "cc"
end

module Local = struct
  open Goto_lib

  type t = { symbol : string; type_ : Type.t; location : Location.t }

  (** Returns the locals Hashtbl and the in_memory hashset *)
  let gather ~prog stmt =
    let locals = Hashtbl.create 1 in
    let in_memory = Hashset.empty () in
    let visitor =
      object
        inherit [bool] Goto_lib.Visitors.iter as super

        method! visit_expr_value ~ctx ~type_ (e : Expr.value) =
          match e with
          | Member { lhs = e; _ } | Index { array = e; _ } | AddressOf e ->
              super#visit_expr ~ctx:true e
          | Symbol x when ctx || not (representable_in_store ~prog type_) ->
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
  fresh_lv : unit -> string;
  in_memory : string Hashset.t;
  locals : (string, Local.t) Hashtbl.t;
  allocated_temps : Local.t Hashset.t;
  fresh_lab : unit -> string;
}

let make ~machine ~prog () =
  {
    allocated_temps = Hashset.empty ~size:32 ();
    locals = Hashtbl.create 0;
    in_memory = Hashset.empty ~size:0 ();
    machine;
    prog;
    fresh_v = (fun () -> failwith "uninitialized var generator");
    fresh_lv = Generators.temp_lvar ();
    fresh_lab = Generators.label ();
  }

let with_new_generators t = { t with fresh_v = Generators.temp_var () }
let fresh_v t = t.fresh_v ()
let fresh_lv t = t.fresh_lv ()
let fresh_lab t = t.fresh_lab ()
let in_memory t x = Hashset.mem t.in_memory x
let is_local t x = Hashtbl.mem t.locals x

let register_allocated_temp ctx ~name:symbol ~type_ ~location =
  let local = Local.{ symbol; type_; location } in
  Hashset.add ctx.allocated_temps local

let size_of ctx ty =
  Error.rethrow_gerror (fun () ->
      let tag_lookup x = Hashtbl.find ctx.prog.types x in
      let machine = ctx.machine in
      Type.size_of ~tag_lookup ~machine ty)

let offset_struct_field ctx ty field =
  Error.rethrow_gerror (fun () ->
      let tag_lookup x = Hashtbl.find ctx.prog.types x in
      let machine = ctx.machine in
      Type.offset_struct_field ~tag_lookup ~machine ty field)

let is_zst_access ctx (ty : Type.t) =
  match ty with
  | Bool | Code _ -> false
  | _ -> size_of ctx ty == 0

let representable_in_store ctx ty = representable_in_store ~prog:ctx.prog ty

let with_entering_body ctx stmt =
  let locals, in_memory = Local.gather ~prog:ctx.prog stmt in
  let allocated_temps = Hashset.empty ~size:32 () in
  let fresh_v = Generators.temp_var () in
  { ctx with in_memory; locals; fresh_v; allocated_temps }
