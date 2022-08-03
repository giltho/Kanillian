module Internal_functions = struct
  let malloc = "i__malloc"
  let calloc = "i__calloc"
  let memmove = "i__memmove"
  let store_zeros = "i__store_zeros"

  let hook = function
    | "malloc" -> Some malloc
    | "calloc" -> Some calloc
    | "memmove" -> Some memmove
    | _ -> None
end

module VTypes = struct
  let int_type = "int"
  let long_type = "long"
  let single_type = "single"
  let float_type = "float"
end

module CBMC_names = struct
  let initialize = "__CPROVER_initialize"
  let start = "__CPROVER__start"
end

module Kanillian_names = struct
  let return_by_copy_name = "i___ret"
  let ret_label = "ret"
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
  let cmpfs_eq = "i__cbmc_binop_cmpfs_eq"
  let cmpfs_ne = "i__cbmc_binop_cmpfs_ne"
  let cmpf_eq = "i__cbmc_binop_cmpf_eq"
  let cmpf_ne = "i__cbmc_binop_cmpf_ne"
  let cmp_gt = "i__cbmc_binop_cmp_gt"
  let cmp_ge = "i__cbmc_binop_cmp_ge"
  let cmp_lt = "i__cbmc_binop_cmp_lt"
  let cmpu_lt = "i__cbmc_binop_cmpu_lt"
  let cmp_le = "i__cbmc_binop_cmp_le"
  let cmp_eq = "i__cbmc_binop_cmp_eq"
  let cmp_ne = "i__cbmc_binop_cmp_ne"
end

module Binop_functions = struct
  let addl = "i__cbmc_binop_addl"
  let mull = "i__cbmc_binop_mull"
  let add = "i__binop_add"
  let mul = "i__binop_mul"
  let mod_ = "i__binop_mod"
  let modlu = "i__binop_modlu"
  let modl = "i__binop_modl"
  let modu = "i__binop_modu"
  let overflow_plus_u64 = "i__cbmc_binop_overflow_plus_u64"
end

module Cast_functions = struct
  let unsign_int = "i__cbmc_unsign_int"
  let unsign_long = "i__cbmc_unsign_long"
  let sign_int = "i__cbmc_sign_int"
  let sign_long = "i__cbmc_sign_long"
end

module Imports = struct
  let env_path_var = "KANILLIAN_RUNTIME_PATH"
  let imports = [ ("cbmc_runtime.gil", false) ]
end

module Internal_Predicates = struct
  let _prefix = "i__"
  let is_int = _prefix ^ "is_int"
  let is_ptr_to_0 = _prefix ^ "is_ptr_to_0"
  let is_ptr = _prefix ^ "is_ptr"
  let is_ptr_to_0_opt = _prefix ^ "is_ptr_to_0_opt"
  let is_ptr_opt = _prefix ^ "is_ptr_opt"
  let is_ptr_to_int_opt = _prefix ^ "is_ptr_to_int_opt"
  let is_ptr_to_float_opt = _prefix ^ "is_ptr_to_float_opt"
  let is_ptr_to_long_opt = _prefix ^ "is_ptr_to_long_opt"
  let is_ptr_to_single_opt = _prefix ^ "is_ptr_to_single_opt"
  let is_long = _prefix ^ "is_long"
  let is_single = _prefix ^ "is_single"
  let is_float = _prefix ^ "is_float"

  (** Internal value getters *)
  let ptr_to_0_get = _prefix ^ "ptr_to_0"

  let ptr_get = _prefix ^ "ptr"
  let int_get = _prefix ^ "int"
  let single_get = _prefix ^ "single"
  let long_get = _prefix ^ "long"
  let float_get = _prefix ^ "float"

  (** global_env *)
  let global_env = _prefix ^ "global_env"

  let glob_fun = _prefix ^ "glob_fun"
  let glob_var_unallocated = _prefix ^ "glob_var_unallocated"
  let glob_var_unallocated_loc = _prefix ^ "glob_var_unallocated_loc"
  let fun_ptr = _prefix ^ "function_ptr"

  (* Arrays *)

  let malloced = _prefix ^ "malloced"
  let zeros_ptr_size = _prefix ^ "zeros_ptr_size"
  let undefs_ptr_size = _prefix ^ "undefs_ptr_size"
  let array_ptr = _prefix ^ "array_ptr"

  (* Pointer arithmetic *)

  let ptr_add = _prefix ^ "ptr_add"
end
