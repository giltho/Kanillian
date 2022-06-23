type t = (string, Gsymbol.t) Hashtbl.t

let of_symtab ~machine (symtab : Symtab.t) : t =
  let ret = Hashtbl.create (Hashtbl.length symtab) in
  Hashtbl.iter
    (fun name sym -> Hashtbl.add ret name (Gsymbol.of_symbol ~machine sym))
    symtab;
  ret
