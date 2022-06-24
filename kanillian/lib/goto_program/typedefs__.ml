type type_ =
  | Array of type_ * int
  | Bool
  | CInteger of IntType.t
  | Code of { params : param list; return_type : type_ }
  | Pointer of type_
  | StructTag of string
  | Empty

(* | Signedbv of { width : int }
   | Unsignedbv of { width : int } *)
and param = {
  type_ : type_;
  identifier : string option;
  base_name : string option;
}
[@@deriving show { with_path = false }]