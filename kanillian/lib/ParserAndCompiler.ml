open Gillian
module CP = Cgil_lib.ParserAndCompiler

let initialize _ =
  let open Kanillian_compiler in
  CP.init_compcert ();
  Utils.Config.entry_point := Kconstants.CBMC_names.start;
  at_exit (fun () -> Option.iter Helpers.Stats.report !Kconfig.kstats_file)

let env_var_import_path =
  Some Kanillian_compiler.Kconstants.Imports.env_path_var

let other_imports = []

type tl_ast = Program.t

module TargetLangOptions = struct
  type t = { main_only : bool; kstats_file : string option }

  let term =
    let open Cmdliner in
    let docs = Manpage.s_common_options in
    let doc =
      "Compile only the main function and its dependencies. It's a hack, use \
       only for testing things."
    in
    let main_only =
      Arg.(value & flag & info [ "only-main"; "main-only" ] ~docs ~doc)
    in
    let doc =
      "If set, write out a file containing the statistics about the \
       compilation process."
    in
    let kstats_file =
      Arg.(
        value
        & opt (some string) None
        & info [ "kstats-file"; "kstats" ] ~docs ~doc)
    in
    let opt main_only kstats_file = { main_only; kstats_file } in
    Term.(const opt $ main_only $ kstats_file)

  let apply { main_only; kstats_file } =
    Kconfig.main_only := main_only;
    Kconfig.kstats_file := kstats_file
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
  let path =
    match files with
    | [ p ] -> p
    | _ -> failwith "Kanillian only handles one symtab file at the moment"
  in
  let+ goto_prog = parse_symtab_into_goto path in
  if !Kconfig.main_only then Main_only.filter_funs goto_prog;
  let goto_prog = Sanitize.sanitize_program goto_prog in
  let context = Ctx.make ~machine:!Kconfig.machine_model ~prog:goto_prog () in
  let gil_prog = Compile.compile context in
  create_compilation_result path goto_prog gil_prog
