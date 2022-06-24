type t = Typedefs__.type_ =
  | Array of t * int
  | Bool  (** CBMC specific. `__CPROVER_bool x`. A single bit boolean *)
  | CInteger of IntType.t
  | Code of { params : Typedefs__.param list; return_type : t }
  | Pointer of t
  | Empty
(*| Signedbv of { width: int }
  | Unsignedbv of { width: int } *)
[@@deriving show { with_path = false }]

let pp fmt t = Typedefs__.pp_type_ fmt t

let is_function = function
  | Code _ -> true (* Will also be true for variadic code if ever *)
  | _ -> false

open Irep.Infix

(** This feels a bit hacky, maybe the constant-deserialization
    should be factored out somewhere else,
    and used both here and in [Expr]
    For example, here, there are no check that it's of the right type *)
let size_of_irep irep = irep $ Value |> Irep.as_just_int

let rec of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let of_irep = of_irep ~machine in
  let failwith = Gerror.fail ~irep in
  match irep.id with
  | Array ->
      let elem_ty = List.hd irep.sub |> of_irep in
      let sz = size_of_irep (irep $ Size) in
      Array (elem_ty, sz)
  | Bool -> Bool
  | CBool -> CInteger I_bool
  | Unsignedbv ->
      let int_ty =
        IntType.which_int_type ~machine ~signed:false
          ~width:(irep $ Width |> Irep.as_just_int)
      in
      CInteger int_ty
  | Signedbv ->
      let int_ty =
        IntType.which_int_type ~machine ~signed:true
          ~width:(irep $ Width |> Irep.as_just_int)
      in
      CInteger int_ty
  | Code ->
      let param_ireps = irep $ Parameters in
      let params = List.map (param_of_irep ~machine) param_ireps.sub in
      let return_type = of_irep (irep $ ReturnType) in
      Code { params; return_type }
  | Pointer ->
      let points_to =
        match irep.sub with
        | [ ty ] -> of_irep ty
        | _ -> failwith "Pointer type has more than one operands"
      in
      Pointer points_to
  | Empty -> Empty
  | other -> failwith ("unhandled type: " ^ Id.to_string other)

and param_of_irep ~machine irep =
  let identifier = Option.map Irep.as_just_string (irep $? CIdentifier) in
  let base_name = Option.map Irep.as_just_string (irep $? CBaseName) in
  let type_ = of_irep ~machine (irep $ Type) in
  Typedefs__.{ identifier; base_name; type_ }

let type_in_irep ~machine irep = of_irep ~machine (irep $ Type)

let as_int_type = function
  | CInteger ty -> ty
  | _ -> failwith "Not an integer type"
