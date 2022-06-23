type t = {
  name : string;
  location : Location.t;
  type_ : Type.t;
  value : SymbolValue.t;
  base_name : string option;
  pretty_name : string option;
  module_ : string option;
  is_type : bool;
  is_macro : bool;
  is_exported : bool;
  is_input : bool;
  is_output : bool;
  is_state_var : bool;
  is_property : bool;
  is_static_lifetime : bool;
  is_thread_local : bool;
  is_lvalue : bool;
  is_file_local : bool;
  is_extern : bool;
  is_volatile : bool;
  is_parameter : bool;
  is_auxiliary : bool;
  is_weak : bool;
}

let of_symbol ~(machine : Machine_model.t) (isym : Symbol.t) : t =
  let str_opt = function
    | "" -> None
    | s -> Some s
  in
  let type_ = Type.of_irep ~machine isym.type_ in
  {
    value = SymbolValue.of_irep ~machine ~type_ isym.value;
    type_;
    location = Location.of_irep isym.location;
    name = isym.name;
    module_ = str_opt isym.module_;
    base_name = str_opt isym.base_name;
    pretty_name = str_opt isym.pretty_name;
    (* mode is ignored because it is useless *)
    (* mode = isym.mode; *)
    is_type = isym.is_type;
    is_macro = isym.is_macro;
    is_exported = isym.is_exported;
    is_input = isym.is_input;
    is_output = isym.is_output;
    is_state_var = isym.is_state_var;
    is_property = isym.is_property;
    is_static_lifetime = isym.is_static_lifetime;
    is_thread_local = isym.is_thread_local;
    is_lvalue = isym.is_lvalue;
    is_file_local = isym.is_file_local;
    is_extern = isym.is_extern;
    is_volatile = isym.is_volatile;
    is_parameter = isym.is_parameter;
    is_auxiliary = isym.is_auxiliary;
    is_weak = isym.is_weak;
  }