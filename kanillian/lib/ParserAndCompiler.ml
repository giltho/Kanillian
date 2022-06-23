open Gillian
module CP = Cgil_lib.ParserAndCompiler

let initialize _ = CP.init_compcert ()
let env_var_import_path = CP.env_var_import_path
let other_imports = []

type tl_ast = Gsymtab.t

module TargetLangOptions = struct
  type t = unit

  let term = Cmdliner.Term.(const ())
  let apply () = ()
end

type err = string

let pp_err = Fmt.string

let parse_symtab_into_goto file =
  let json = Yojson.Safe.from_file file in
  let tbl = Irep_lib.Symtab.of_yojson json in
  Result.map
    (fun tbl ->
      Logging.normal ~severity:Warning (fun m ->
          m "Extracting main!! Need to remove that in the future");
      let main = Hashtbl.find tbl "main" in
      let tbl = Hashtbl.create 1 in
      let () = Hashtbl.add tbl "main" main in
      Goto_lib.Gsymtab.of_symtab ~machine:!Kconfig.machine_model tbl)
    tbl

let create_compilation_result path goto_prog gil_prog =
  let open CommandLine.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  { gil_progs = [ (gil_path, gil_prog) ]; source_files; tl_ast = goto_prog }

let parse_and_compile_files files =
  let open Utils.Syntaxes.Result in
  let f files =
    let path =
      match files with
      | [ p ] -> p
      | _ -> failwith "Kanillian only handles one symtab file at the moment"
    in
    let+ goto_prog = parse_symtab_into_goto path in
    let gil_prog = Gotoc_to_gil.compile goto_prog in
    create_compilation_result path goto_prog gil_prog
  in
  f files