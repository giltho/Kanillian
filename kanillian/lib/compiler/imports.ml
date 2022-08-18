open Gillian.Utils.ExecMode

let env_path_var = "KANILLIAN_RUNTIME_PATH"

module GEnvConfig = struct
  type t = Allocated_functions | Unallocated_functions
  (* We don't provide a way of making functions allocated for now.
     It's implemented from Gillian-C, but we ignore it.
     Long term, they should always be allocated, it should be done through concrete locations.
     The global environments should be static and not symbolic at all! It should be configured at compile
     time or something, maybe serialized in a file with the gil program if needed.
     Calls to genv actions and predicates should simply replaced at runtime by the concrete values.
     Things would get much more efficient, and also probably more correct.*)

  let any_genv_config = [ Allocated_functions; Unallocated_functions ]
  let allocated = [ Allocated_functions ]
  let unallocated = [ Unallocated_functions ]
end

open GEnvConfig
open Archi

type t = {
  file : string;
  arch : Archi.t list;
  exec : Gillian.Utils.ExecMode.t list;
  genv_config : GEnvConfig.t list;
}

(** All imports, should not be used as such, imports should be selected using the [import] function *)
let all_imports =
  [
    {
      file = "archi32_constants.gil";
      arch = a32;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "archi64_constants.gil";
      arch = a64;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "internals.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "global_environment_common.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "genv_allocated_functions.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = allocated;
    };
    {
      file = "internal_casts.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "internal_binops.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "internal_unops.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "internal_stdlib.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_verif.gil";
      arch = any_arch;
      exec = exec_with_preds;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_non_verif.gil";
      arch = any_arch;
      exec = concrete_exec;
      genv_config = any_genv_config;
    };
    {
      file = "rust_allocation_internals.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "string.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
  ]

let imports arch exec_mode genv_config =
  let select x =
    List.mem arch x.arch && List.mem exec_mode x.exec
    && List.mem genv_config x.genv_config
  in
  List.map (fun imp -> (imp.file, false)) (List.filter select all_imports)
