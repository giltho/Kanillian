module CBMC_names = struct
  let initialize = "__CPROVER_initialize"
  let start = "__CPROVER__start"
end

module Comp_functions = struct
  let cmpl_le = "i__cbmc_binop_cmpl_le"
  let cmpl_ge = "i__cbmc_binop_cmpl_ge"
  let cmpl_lt = "i__cbmc_binop_cmpl_lt"
  let cmpl_eq = "i__cbmc_binop_cmpl_eq"
  let cmpl_ne = "i__cbmc_binop_cmpl_ne"
  let cmpl_gt = "i__cmbc_binop_cmpl_gt"
  let cmpu_le = "i__cbmc_binop_cmpu_le"
  let cmpu_gt = "i__cbmc_binop_cmpu_gt"
  let cmpu_ne = "i__cbmc_binop_cmpu_ne"
  let cmpu_eq = "i__cbmc_binop_cmpu_eq"
  let cmpu_ge = "i__cbmc_binop_cmpu_ge"
  let cmplu_le = "i__cbmc_binop_cmplu_le"
  let cmplu_eq = "i__cbmc_binop_cmplu_eq"
  let cmplu_ne = "i__cbmc_binop_cmplu_ne"
  let cmplu_ge = "i__cbmc_binop_cmplu_ge"
  let cmplu_lt = "i__cbmc_binop_cmplu_lt"
  let cmplu_gt = "i__cbmc_binop_cmplu_gt"
  let cmpfs_ge = "i__cbmc_binop_cmpfs_ge"
  let cmpfs_le = "i__cbmc_binop_cmpfs_le"
  let cmp_gt = "i__cbmc_binop_cmp_gt"
  let cmp_ge = "i__cbmc_binop_cmp_ge"
  let cmp_lt = "i__cbmc_binop_cmp_lt"
  let cmpu_lt = "i__cbmc_binop_cmpu_lt"
  let cmp_le = "i__cbmc_binop_cmp_le"
  let cmp_eq = "i__cbmc_binop_cmp_eq"
  let cmp_ne = "i__cbmc_binop_cmp_ne"
end

module Binop_functions = struct
  (* We override it, because we know it cannot be a pointer *)
  open Cgil_lib.CConstants.BinOp_Functions

  let addl = "i__cbmc_binop_addl"
  let mull = "i__cbmc_binop_mull"
  let add = add
  let mul = mul
end

module Cast_functions = struct
  let unsign_int = "i__cbmc_unsign_int"
  let unsign_long = "i__cbmc_unsign_long"
end

module Imports = struct
  let env_path_var = "KANILLIAN_RUNTIME_PATH"
  let imports = [ ("cbmc_runtime.gil", false) ]
end
