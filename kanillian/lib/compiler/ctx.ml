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

module Symbols_in_memory = struct
  open Goto_lib

  (** A hashet containing all the  *)
  let gather ~prog stmt =
    let set = Hashset.empty () in
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
    let addressed_visitor =
      object
        inherit [bool] Goto_lib.Visitors.iter as super

        method! visit_expr_value ~ctx ~type_ (e : Expr.value) =
          match e with
          | Member { lhs = e; _ } | Index { array = e; _ } | AddressOf e ->
              super#visit_expr ~ctx:true e
          | Symbol x when ctx || not (representable_in_store type_) ->
              Hashset.add set x
          | _ -> super#visit_expr_value ~ctx ~type_ e
      end
    in
    addressed_visitor#visit_stmt ~ctx:false stmt;
    set
end

type t = {
  machine : Machine_model.t;
  prog : Program.t;
  fresh_v : unit -> string;
  fresh_lv : unit -> string;
  in_memory : string Hashset.t;
}

let make ~machine ~prog () =
  {
    in_memory = Hashset.empty ();
    machine;
    prog;
    fresh_v = (fun () -> failwith "uninitialized var generator");
    fresh_lv = (fun () -> failwith "uninitialized lvar generator");
  }

let with_new_generators t =
  {
    t with
    fresh_v = Generators.temp_var ();
    fresh_lv = Generators.temp_lvar ();
  }

let fresh_v t = t.fresh_v ()
let fresh_lv t = t.fresh_lv ()
let in_memory t x = Hashset.mem t.in_memory x

let with_in_memory t stmt =
  let in_memory = Symbols_in_memory.gather ~prog:t.prog stmt in
  { t with in_memory }

let size_of ctx ty =
  try
    let tag_lookup x = Hashtbl.find ctx.prog.types x in
    let machine = ctx.machine in
    Type.size_of ~tag_lookup ~machine ty
  with
  | Gerror.Code_error (_, msg) -> Error.code_error msg
  | Gerror.Unexpected_irep (_, msg) -> Error.unexpected msg
  | Gerror.Unhandled_irep (_, msg) -> Error.unhandled msg
