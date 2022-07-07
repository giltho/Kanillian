let should_be_filtered = function
  (* The next 4 names contain arrays of size infinity.
     We don't handle that for now, we don't know if that's necessary *)
  | "__CPROVER_thread_key_dtors"
  | "__CPROVER_thread_keys"
  | "__CPROVER_memory"
  | "__CPROVER_threads_exited" -> true
  (* The following names usually contain unhandled irep.
     In any case we don't use them yet. *)
  | "__CPROVER_initialize" | "__CPROVER__start" -> true
  | _ -> false

module Global_var = struct
  type t = {
    type_ : Type.t;
    symbol : string;
    value : Expr.t option;
    location : Location.t;
  }
end

module Func = struct
  type t = {
    params : Param.t list;
    body : Stmt.t option;
        (** If the body is empty,
            it means that the function is not defined,
            and it should return nondet *)
    return_type : Type.t;
    location : Location.t;
    symbol : string;
  }
end

type t = {
  vars : (string, Global_var.t) Hashtbl.t;
  funs : (string, Func.t) Hashtbl.t;
  types : (string, Type.t) Hashtbl.t;
}

let of_symtab ~machine (symtab : Symtab.t) : t =
  let env =
    {
      vars = Hashtbl.create 1;
      funs = Hashtbl.create 1;
      types = Hashtbl.create 1;
    }
  in
  symtab
  |> Hashtbl.iter (fun name (sym : Irep_lib.Symbol.t) ->
         if sym.is_file_local || should_be_filtered name then ()
         else
           let () =
             if sym.is_weak || sym.is_volatile then
               Gerror.unhandled "weak or volatile value"
           in
           let location = Location.of_irep sym.location in
           let type_ = Type.of_irep ~machine sym.type_ in
           let value = SymbolValue.of_irep ~machine ~type_ sym.value in
           if sym.is_type then Hashtbl.add env.types name type_
           else
             match type_ with
             | Bool ->
                 (* We can't write pure bools in memory, so let's ignore these for now
                    A solution would be to keep track of those, and convert them
                    to and from u8 every time *)
                 ()
             | Code { params; return_type } ->
                 let body =
                   match value with
                   | SVNone -> None
                   | Stmt s -> Some s
                   | Expr _ ->
                       Gerror.unexpected "function body is not a statment"
                 in
                 let func =
                   Func.{ symbol = name; params; return_type; location; body }
                 in
                 Hashtbl.add env.funs name func
             | _ ->
                 let value =
                   match value with
                   | SVNone -> None
                   | Expr e -> Some e
                   | _ ->
                       Gerror.unexpected "variable value is not an expression"
                 in
                 let var =
                   Global_var.{ symbol = name; type_; value; location }
                 in
                 Hashtbl.add env.vars name var);
  env

let fold_functions f prog acc = Hashtbl.fold f prog.funs acc
let fold_variables f prog acc = Hashtbl.fold f prog.vars acc

let rec is_zst ~prog (ty : Type.t) : bool =
  match ty with
  | Array (_, 0) | Struct { components = []; _ } -> true
  | StructTag s -> Hashtbl.find prog.types s |> is_zst ~prog
  | _ -> false
