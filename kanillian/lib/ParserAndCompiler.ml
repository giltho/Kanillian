open Gillian
module CP = Cgil_lib.ParserAndCompiler

let initialize _ = CP.init_compcert ()

let env_var_import_path =
  Some Kanillian_compiler.Kconstants.Imports.env_path_var

let other_imports = []

type tl_ast = Program.t

module TargetLangOptions = struct
  type t = { main_only : bool }

  let term =
    let open Cmdliner in
    let docs = Manpage.s_common_options in
    let doc = "Hack - compile only the main function" in
    let main_only =
      Arg.(value & flag & info [ "only-main"; "main-only" ] ~docs ~doc)
    in
    let opt main_only = { main_only } in
    Term.(const opt $ main_only)

  let apply { main_only } = Kconfig.main_only := main_only
end

type err = string

let pp_err = Fmt.string

let parse_symtab_into_goto file =
  let json = Yojson.Safe.from_file file in
  let tbl = Irep_lib.Symtab.of_yojson json in
  Result.map
    (fun tbl ->
      let machine = Machine_model.consume_from_symtab tbl in
      if not Machine_model.(equal machine archi64) then
        failwith "For now, kanillian can only run on archi64";
      Kconfig.machine_model := machine;
      Logging.normal ~severity:Warning (fun m ->
          m
            "Filtering every cprover_specific symbol!! Need to remove that in \
             the future");
      let tbl =
        if !Kconfig.main_only then
          let ntbl = Hashtbl.create 1 in
          let () = Hashtbl.add ntbl "main" (Hashtbl.find tbl "main") in
          ntbl
        else tbl
      in
      Goto_lib.Program.of_symtab ~machine tbl)
    tbl

let create_compilation_result path goto_prog gil_prog =
  let open CommandLine.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  { gil_progs = [ (gil_path, gil_prog) ]; source_files; tl_ast = goto_prog }

let parse_and_compile_files files =
  let open Kanillian_compiler in
  let open Utils.Syntaxes.Result in
  let f files =
    let path =
      match files with
      | [ p ] -> p
      | _ -> failwith "Kanillian only handles one symtab file at the moment"
    in
    let+ goto_prog = parse_symtab_into_goto path in
    let () = Sanitize.sanitize_program_in_place goto_prog in
    let context = Ctx.make ~machine:!Kconfig.machine_model ~prog:goto_prog () in
    let gil_prog = Compile.compile context in
    create_compilation_result path goto_prog gil_prog
  in
  f files
