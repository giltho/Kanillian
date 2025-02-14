type 'tl_ast compiled_progs = {
  gil_progs : (string * (Annot.t, string) Prog.t) list;
  source_files : SourceFiles.t;
  tl_ast : 'tl_ast;
}

module type S = sig
  module TargetLangOptions : sig
    (** Command line options specific to the target language. *)
    type t

    (** A term that will be added to every command. *)
    val term : t Cmdliner.Term.t

    (** A side-effect function that will determine the behaviour of the target-language specific options *)
    val apply : t -> unit
  end

  (** Type of error that can occur during parsing or compilation *)
  type err

  (** Type of the target language AST *)
  type tl_ast

  (** Pretty printer for type {!err} *)
  val pp_err : Format.formatter -> err -> unit

  (** Takes a set of source file paths, parses them with the user's language, and
      then compiles them to a single or a set of GIL programs. The returned GIL
      program(s) should be ready to be analysed. *)
  val parse_and_compile_files :
    string list -> (tl_ast compiled_progs, err) result

  (** [other_imports] is an association list that maps extensions to a parser
      and compiler. For example, it is possible to import a JSIL file in a GIL
      program using [import "file.jsil";]. In order to do so, the [other_imports]
      list should contain the tuple [("jsil", parse_and_compile_jsil_file)] where
      [parse_and_compile_jsil_file] is a function that takes a file path, parses
      the file as a JSIL program, and compiles this to a GIL program. *)
  val other_imports :
    (string * (string -> ((Annot.t, string) Prog.t, err) result)) list

  (** Contains the name of the environment variable which contains the path to where the runtime is stored. *)
  val env_var_import_path : string option

  (** Function that will be executed at initialisation. It will be passed the current execution mode as parameter *)
  val initialize : ExecMode.t -> unit
end

module Dummy : S = struct
  module TargetLangOptions = struct
    type t = unit

    let term = Cmdliner.Term.(const ())
    let apply () = ()
  end

  type tl_ast = unit
  type err = unit

  let pp_err _ _ =
    failwith
      "Please implement the compiling interface to use with the '-compile' \
       flag or test suites"

  let parse_and_compile_files _ =
    failwith
      "Please implement the compiling interface to use with the '-compile' \
       flag or test suites"

  let other_imports = []
  let env_var_import_path = None
  let initialize _ = ()
end
