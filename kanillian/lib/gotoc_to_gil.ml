open Gil_syntax

let compile_location (loc : Goto_lib.Location.t) =
  match loc with
  | None -> Location.none
  | Some loc ->
      let pos_line = Option.value ~default:0 loc.line in
      let pos_column = Option.value ~default:0 loc.col in
      let loc_start = Location.{ pos_line; pos_column } in
      let loc_end = Location.{ pos_line; pos_column = pos_column + 2 } in

      Location.{ loc_source = loc.source; loc_start; loc_end }

let find_main symtab = Hashtbl.find symtab "main"

let compile_function ~name (sym : Gsymbol.t) : (Annot.t, string) Proc.t =
  if sym.is_volatile || sym.is_weak then
    failwith "Cannot handled volatile or weak data yet";
  let f_loc = compile_location sym.location in
  let proc_params = [] in
  let proc_spec = None in
  let proc_body = [||] in
  Proc.
    {
      proc_name = name;
      proc_source_path = Some f_loc.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
    }

let compile (symtab : Gsymtab.t) : (Annot.t, string) Prog.t =
  let prog = Prog.create () in
  (* let functions = Hashtbl.fold (fun (s, m) -> if) *)
  let main = find_main symtab in
  let prog = Prog.add_proc prog (compile_function ~name:"main" main) in
  Fmt.pr "%a" Prog.pp_labeled prog;
  failwith "alright, at least I got here!" |> ignore;
  prog
