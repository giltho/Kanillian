type t = (string, Type.t) Hashtbl.t

let get_and_remove_from_symtab (symtab : Gsymtab.t) : t =
  let tyenv = Hashtbl.create 1 in
  symtab
  |> Hashtbl.filter_map_inplace (fun name (sym : Gsymbol.t) ->
         if not sym.is_type then Some sym
         else
           let () = Hashtbl.add tyenv name sym.type_ in
           None);
  tyenv
