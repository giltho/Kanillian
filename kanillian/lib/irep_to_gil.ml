open Gil_syntax
open Irep.Infix

let location irep =
  let open Location in
  let loc_start =
    {
      pos_line = (irep $ Line).id |> Id.to_string |> int_of_string;
      pos_column = 0;
    }
  in
  let loc_end = { loc_start with pos_column = 2 } in
  Location.{ loc_source = (irep $ File).id |> Id.to_string; loc_start; loc_end }

let find_main symtab = Hashtbl.find symtab "main"

let compile_function ~name (sym : Symbol.t) : (Annot.t, string) Proc.t =
  let location = location sym.location in
  let proc_params = [] in
  let proc_spec = None in
  let proc_body = [||] in
  Proc.
    {
      proc_name = name;
      proc_source_path = Some location.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
    }

let compile (symtab : Symtab.t) : (Annot.t, string) Prog.t =
  let prog = Prog.create () in
  let main = find_main symtab in
  let prog = Prog.add_proc prog (compile_function ~name:"main" main) in
  Fmt.pr "%a" Prog.pp_labeled prog;
  failwith "need to implement more" |> ignore;
  prog
