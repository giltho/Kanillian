open Gillian.Utils.ExecMode

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

let env_path_var = "GILLIAN_C_RUNTIME_PATH"

type t = {
  file : string;
  arch : Archi.t list;
  exec : Gillian.Utils.ExecMode.t list;
  genv_config : GEnvConfig.t list;
}

(** All imports, should not be used as such, imports should be selected using the [import] function *)
let all_imports =
  [
    (* Common *)
    {
      file = "unops_common.gil";
      arch = any_arch;
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
      file = "global_environment_common_concrete.gil";
      arch = any_arch;
      exec = concrete_exec;
      genv_config = any_genv_config;
    };
    {
      file = "global_environment_common_symbolic.gil";
      arch = any_arch;
      exec = non_concrete_exec;
      genv_config = any_genv_config;
    };
    {
      file = "binops_common.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "logic_common.gil";
      arch = any_arch;
      exec = exec_with_preds;
      genv_config = any_genv_config;
    };
    {
      file = "string.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    (* Arch64 specific *)
    {
      file = "stdlib_archi64.gil";
      arch = a64;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_archi64_verif.gil";
      arch = a64;
      exec = ver_exec;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_archi64_non_verif.gil";
      arch = a64;
      exec = non_ver_exec;
      genv_config = any_genv_config;
    };
    {
      file = "global_environment_archi64.gil";
      arch = a64;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "logic_archi64.gil";
      arch = a64;
      exec = exec_with_preds;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi64_all_exec.gil";
      arch = a64;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi64_non_bi.gil";
      arch = a64;
      exec = non_bi_exec;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi64_bi_exec.gil";
      arch = a64;
      exec = bi_exec;
      genv_config = any_genv_config;
    };
    (* Arch32 specific *)
    {
      file = "stdlib_archi32.gil";
      arch = a32;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_archi32_verif.gil";
      arch = a32;
      exec = ver_exec;
      genv_config = any_genv_config;
    };
    {
      file = "stdlib_archi32_non_verif.gil";
      arch = a32;
      exec = non_ver_exec;
      genv_config = any_genv_config;
    };
    {
      file = "global_environment_archi32.gil";
      arch = a32;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "logic_archi32.gil";
      arch = a32;
      exec = exec_with_preds;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi32_all_exec.gil";
      arch = a32;
      exec = all_exec;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi32_non_bi.gil";
      arch = a32;
      exec = non_bi_exec;
      genv_config = any_genv_config;
    };
    {
      file = "binops_archi32_bi_exec.gil";
      arch = a32;
      exec = bi_exec;
      genv_config = any_genv_config;
    };
    (* Global functions *)
    {
      file = "genv_allocated_functions.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = allocated;
    };
    {
      file = "genv_unallocated_functions.gil";
      arch = any_arch;
      exec = all_exec;
      genv_config = unallocated;
    };
  ]

let imports arch exec_mode genv_config =
  let select x =
    List.mem arch x.arch && List.mem exec_mode x.exec
    && List.mem genv_config x.genv_config
  in
  List.map (fun imp -> (imp.file, false)) (List.filter select all_imports)
