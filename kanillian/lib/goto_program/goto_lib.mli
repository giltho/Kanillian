module Machine_model : sig
  type t = {
    alignment : int;
    bool_width : int;
    char_is_unsigned : bool;
    char_width : int;
    double_width : int;
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
  [@@deriving eq, show]

  val archi64 : t

  (** Consumes the architecture data from the symtab, and returns the built machine_model. *)
  val consume_from_symtab : Symtab.t -> t
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
    [@@deriving show]
  end

  module Self : sig
    type t = Postdecrement | Postincrement | Predecrement | Preincrement
    [@@deriving show]
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
    [@@deriving show]
  end
end

module Location : sig
  type t = {
    origin_id : int;
    source : string option;
    line : int option;
    col : int option;
  }

  val of_irep : Irep.t -> t
end

module IntType : sig
  type t = I_bool | I_char | I_int | I_size_t | I_ssize_t [@@deriving show]

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

and Datatype_component : sig
  type t =
    | Field of { name : string; type_ : Type.t }
    | Padding of { name : string; bits : int }
end

and Type : sig
  type t =
    | Array of t * int
    | Bool
    | CInteger of IntType.t
    | Float
    | Double
    | Code of { params : Param.t list; return_type : t }
    | Pointer of t
    | Struct of { components : Datatype_component.t list; tag : string }
    | StructTag of string
    | Union of { components : Datatype_component.t list; tag : string }
    | UnionTag of string
    | Constructor
    | Empty
  (* | Signedbv of { width : int }
     | Unsignedbv of { width : int } *)
  [@@deriving show]

  val is_function : t -> bool
  val as_int_type : t -> IntType.t
  val of_irep : machine:Machine_model.t -> Irep.t -> t
  val pp : Format.formatter -> t -> unit
end

module Expr : sig
  type value =
    | IntConstant of Z.t
    | CBoolConstant of bool
    | BoolConstant of bool
    | Symbol of string
    | FunctionCall of { func : t; args : t list }
    | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
    | ByteExtract of { e : t; offset : int }
    | UnOp of { op : Ops.Unary.t; e : t }
    | Struct of t list
    | AddressOf of t
    | Index of { array : t; index : t }
    | StringConstant of string
    | TypeCast of t

  and t = { value : value; type_ : Type.t; location : Location.t }
  [@@deriving show]

  val as_symbol : t -> string
  val value_of_irep : machine:Machine_model.t -> type_:Type.t -> Irep.t -> value
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

module Stmt : sig
  type body =
    | Decl of { lhs : Expr.t; value : Expr.t option }
    | Assign of { lhs : Expr.t; rhs : Expr.t }
    | Assume of { cond : Expr.t }
    | Assert of { cond : Expr.t }
    | Block of t list
    | Label of string * t list
    | Goto of string
    | Skip
    | Expression of Expr.t
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