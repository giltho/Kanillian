type t = Typedefs__.type_ =
  | Array of t * int
  | Bool
  | CInteger of IntType.t
  | Float
  | Double
  | Code of { params : Typedefs__.param list; return_type : t }
  | Pointer of t
  | Struct of { components : Typedefs__.datatype_component list; tag : string }
  | StructTag of string
  | Union of { components : Typedefs__.datatype_component list; tag : string }
  | UnionTag of string
  | Constructor
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
let size_of_irep irep =
  irep $ Value |> Irep.as_just_bitpattern ~signed:false ~width:32 |> Z.to_int

let rec of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let of_irep = of_irep ~machine in
  let datatype_component_of_irep = datatype_component_of_irep ~machine in
  let failwith = Gerror.fail ~irep in
  match irep.id with
  | Array ->
      let elem_ty = List.hd irep.sub |> of_irep in
      let sz = size_of_irep (irep $ Size) in
      Array (elem_ty, sz)
  | Bool -> Bool
  | Floatbv -> (
      (* TODO: there should probably be a check about the width or something.
         Maybe it should be obtained from the width and not that weird f field *)
      match irep $ F |> Irep.as_just_int with
      | 23 -> Float
      | 52 -> Double
      | _ -> failwith "unsupported floatbv kind")
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
  | Struct ->
      let tag = irep $ Tag |> Irep.as_just_string in
      let components =
        (irep $ Components).sub |> List.map datatype_component_of_irep
      in
      Struct { components; tag }
  | StructTag ->
      let identifier = irep $ Identifier |> Irep.as_just_string in
      StructTag identifier
  | Union ->
      let tag = irep $ Tag |> Irep.as_just_string in
      let components =
        (irep $ Components).sub |> List.map datatype_component_of_irep
      in
      Union { tag; components }
  | UnionTag ->
      let identifier = irep $ Identifier |> Irep.as_just_string in
      UnionTag identifier
  | Constructor -> Constructor
  | Empty -> Empty
  | other -> failwith ("unhandled type: " ^ Id.to_string other)

and param_of_irep ~machine irep =
  let identifier = Option.map Irep.as_just_string (irep $? CIdentifier) in
  let base_name = Option.map Irep.as_just_string (irep $? CBaseName) in
  let type_ = of_irep ~machine (irep $ Type) in
  Typedefs__.{ identifier; base_name; type_ }

and datatype_component_of_irep ~machine irep : Datatype_component.t =
  let is_padding =
    match irep $? CIsPadding |> Option.map (fun x -> x.Irep.id) with
    | Some Id1 -> true
    | _ -> false
  in
  if is_padding then
    let name = irep $ Name |> Irep.as_just_string in
    let bits = irep $ Type $ Width |> Irep.as_just_int in
    Padding { name; bits }
  else
    let name = irep $ Name |> Irep.as_just_string in
    let type_ = irep $ Type |> of_irep ~machine in
    Field { name; type_ }

let type_in_irep ~machine irep = of_irep ~machine (irep $ Type)

let as_int_type = function
  | CInteger ty -> ty
  | _ -> failwith "Not an integer type"
