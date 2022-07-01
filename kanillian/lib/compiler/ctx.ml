open Gillian.Utils.Prelude

module Generators = struct
  let temp_var () =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = "temp__" ^ string_of_int c in
      incr id;
      ret

  let temp_lvar () =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = "#lvar_" ^ string_of_int c in
      incr id;
      ret
end

module Local = struct
  open Goto_lib

  type t = { symbol : string; type_ : Type.t; location : Location.t }

  (** Returns the locals Hashtbl and the in_memory hashset *)
  let gather ~prog stmt =
    let locals = Hashtbl.create 1 in
    let in_memory = Hashset.empty () in
    let representable_in_store (type_ : Type.t) =
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
    in
    let visitor =
      object
        inherit [bool] Goto_lib.Visitors.iter as super

        method! visit_expr_value ~ctx ~type_ (e : Expr.value) =
          match e with
          | Member { lhs = e; _ } | Index { array = e; _ } | AddressOf e ->
              super#visit_expr ~ctx:true e
          | Symbol x when ctx || not (representable_in_store type_) ->
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
}

let make ~machine ~prog () =
  {
    locals = Hashtbl.create 0;
    in_memory = Hashset.empty ~size:0 ();
    machine;
    prog;
    fresh_v = (fun () -> failwith "uninitialized var generator");
    fresh_lv = Generators.temp_lvar ();
  }

let with_new_generators t = { t with fresh_v = Generators.temp_var () }
let fresh_v t = t.fresh_v ()
let fresh_lv t = t.fresh_lv ()
let in_memory t x = Hashset.mem t.in_memory x
let is_local t x = Hashtbl.mem t.locals x

let size_of ctx ty =
  try
    let tag_lookup x = Hashtbl.find ctx.prog.types x in
    let machine = ctx.machine in
    Type.size_of ~tag_lookup ~machine ty
  with
  | Gerror.Code_error (_, msg) -> Error.code_error msg
  | Gerror.Unexpected_irep (_, msg) -> Error.unexpected msg
  | Gerror.Unhandled_irep (_, msg) -> Error.unhandled msg

let is_zst_access ctx (ty : Type.t) =
  match ty with
  | Bool -> false
  | _ -> size_of ctx ty == 0

let with_entering_body ctx stmt =
  let locals, in_memory = Local.gather ~prog:ctx.prog stmt in
  let fresh_v = Generators.temp_var () in
  { ctx with in_memory; locals; fresh_v }
