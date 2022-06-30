module Global_var = struct
  type t = {
    type_ : Type.t;
    symbol : string;
    value : Expr.t option;
    location : Location.t;
  }

  let init _t = ()
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
         if Sym_clean.is_cbmc_specific name || sym.is_file_local then ()
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
