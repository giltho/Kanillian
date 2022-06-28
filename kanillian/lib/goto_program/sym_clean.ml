(* At some point, this should be just put actually
   set up in the environment. For now we filter them. *)
let is_cbmc_specific = function
  | "__CPROVER_assert"
  | "__CPROVER_assume"
  | "__CPROVER_architecture_NULL_is_zero"
  | "__CPROVER_architecture_os"
  | "__CPROVER_architecture_arch"
  | "__CPROVER_architecture_endianness"
  | "__CPROVER_architecture_alignment"
  | "__CPROVER_architecture_wchar_t_width"
  | "__CPROVER_architecture_long_double_width"
  | "__CPROVER_architecture_double_width"
  | "__CPROVER_deallocated"
  | "__CPROVER_next_thread_key"
  | "__CPROVER_thread_key_dtors"
  | "__CPROVER_thread_keys"
  | "__CPROVER_architecture_wchar_t_is_unsigned"
  | "__CPROVER_initialize"
  | "__CPROVER_next_thread_id"
  | "__CPROVER_architecture_char_is_unsigned"
  | "__CPROVER_pipe_count"
  | "__CPROVER_thread_id"
  | "__CPROVER_constant_infinity_uint"
  | "__CPROVER_architecture_pointer_width"
  | "__CPROVER_dead_object"
  | "__CPROVER_size_t"
  | "__CPROVER_architecture_short_int_width"
  | "__CPROVER_memory"
  | "__CPROVER_rounding_mode"
  | "__CPROVER_threads_exited"
  | "__CPROVER_architecture_memory_operand_size"
  | "__CPROVER_architecture_word_size"
  | "__CPROVER_new_object"
  | "__CPROVER_malloc_is_new_array"
  | "__CPROVER__start"
  | "__CPROVER_memory_leak"
  | "__CPROVER_alloca_object"
  | "__CPROVER_max_malloc_size"
  | "__CPROVER_architecture_int_width"
  | "__CPROVER_architecture_long_int_width"
  | "__CPROVER_architecture_bool_width"
  | "__CPROVER_architecture_char_width"
  | "__CPROVER_architecture_long_long_int_width"
  | "__CPROVER_architecture_single_width"
  | "__nondet"
  | "return'" -> true
  | _ -> false

let clean_table tbl =
  Hashtbl.filter_map_inplace
    (fun x i -> if is_cbmc_specific x then None else Some i)
    tbl
  [@@ocaml.deprecated]
