module Machine_model : sig
  type t = {
    alignment : int;
    bool_width : int;
    char_is_unsigned : bool;
    char_width : int;
    double_width : int;
    float_width : int;
    int_width : int;
    is_big_endian : bool;
    long_double_width : int;
    long_int_width : int;
    long_long_int_width : int;
    memory_operand_size : int;
    null_is_zero : bool;
    pointer_width : int;
    short_int_width : int;
    single_width : int;
    wchar_t_is_unsigned : bool;
    wchar_t_width : int;
    word_size : int;
  }
  [@@deriving eq]

  val archi64 : t
end

module Ops : sig
  module Binary : sig
    type t =
      | And
      | Ashr
      | Bitand
      | Bitor
      | Bitnand
      | Bitxor
      | Div
      | Equal
      | Ge
      | Gt
      | IeeeFloatEqual
      | IeeeFloatNotequal
      | Implies
      | Le
      | Lshr
      | Lt
      | Minus
      | Mod
      | Mult
      | Notequal
      | Or
      | OverflowMinus
      | OverflowMult
      | OverflowPlus
      | Plus
      | ROk
      | Rol
      | Ror
      | Shl
      | Xor
  end

  module Self : sig
    type t = Postdecrement | Postincrement | Predecrement | Preincrement
  end

  module Unary : sig
    type t =
      | Bitnot
      | BitReverse
      | Bswap
      | IsDynamicObject
      | IsFinite
      | Not
      | ObjectSize
      | PointerObject
      | PointerOffset
      | Popcount
      | CountTrailingZeros of { allow_zero : bool }
      | CountLeadingZeros of { allow_zero : bool }
      | UnaryMinus
  end
end

module Location : sig
  type tt = { source : string; line : int option; col : int option }
  type t = tt option

  val of_irep : Irep.t -> t
end

module IntType : sig
  type t = I_bool | I_char | I_int | I_size_t | I_ssize_t

  val pp : Format.formatter -> t -> unit

  module Bv_encoding : sig
    type int_type = t
    type t = { signed : bool; width : int }

    val equal : t -> t -> bool
    val encode : machine:Machine_model.t -> int_type -> t
  end

  val which_int_type_opt :
    machine:Machine_model.t -> signed:bool -> width:int -> t option

  (* Fails if not found *)
  val which_int_type : machine:Machine_model.t -> signed:bool -> width:int -> t
end

module rec Param : sig
  type t = {
    type_ : Type.t;
    identifier : string option;
    base_name : string option;
  }

  val of_irep : machine:Machine_model.t -> Irep.t -> t
  val pp : Format.formatter -> t -> unit
end

and Type : sig
  type t =
    | Array of t * int
    | Bool
    | CInteger of IntType.t
    | Code of { params : Param.t list; return_type : t }
    | Pointer of t
    | Empty
  (* | Signedbv of { width : int }
     | Unsignedbv of { width : int } *)

  val is_function : t -> bool
  val of_irep : machine:Machine_model.t -> Irep.t -> t
  val pp : Format.formatter -> t -> unit
end

module Expr : sig
  type value = IntConstant of Z.t | BoolConstant of bool | Symbol of string
  and t = { value : value; type_ : Type.t; location : Location.t }

  val value_of_irep : machine:Machine_model.t -> type_:Type.t -> Irep.t -> value
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

module Stmt : sig
  type body =
    | Decl of { lhs : Expr.t; value : Expr.t option }
    | Block of t list
    | Skip
    | Return of Expr.t option

  and t = { location : Location.t; body : body }

  val body_of_irep : machine:Machine_model.t -> Irep.t -> body
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

module SymbolValue : sig
  type t = Expr of Expr.t | Stmt of Stmt.t | SVNone

  val of_irep : machine:Machine_model.t -> type_:Type.t -> Irep.t -> t
end

module Gsymbol : sig
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

  val of_symbol : machine:Machine_model.t -> Symbol.t -> t
end

module Gsymtab : sig
  type t = (string, Gsymbol.t) Hashtbl.t

  val of_symtab : machine:Machine_model.t -> Symtab.t -> t
end