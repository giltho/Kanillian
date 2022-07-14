module J = struct
  include Yojson.Safe.Util

  let iter_obj f = function
    | `Assoc l -> List.iter f l
    | _ -> Error.user_error "Not a json object"

  module Infix = struct
    let ( $? ) j m =
      match member m j with
      | `Null -> None
      | other -> Some other
  end
end

module Unhandled = struct
  type feature =
    | CompositNondet
    | ReturnByCopy
    | CallArgumentByCopy
    | DeclCompositValue
    | ByteExtract
    | StructConstant
    | ArrayConstant
    | StringConstant
    | OutputStmt
    | ArrayIndex
    | BinOp of Ops.Binary.t * (Type.t * Type.t) option
    | UnOp of Ops.Unary.t
    | LoadScalar of Type.t
    | StoreScalar of Type.t
    | Cast of Type.t * Type.t

  let feature_to_string = function
    | CompositNondet -> "CompositNondet"
    | ReturnByCopy -> "ReturnByCopy"
    | CallArgumentByCopy -> "CallArgumentByCopy"
    | DeclCompositValue -> "DeclCompositValue"
    | ByteExtract -> "ByteExtract"
    | StructConstant -> "StructConstant"
    | ArrayConstant -> "ArrayConstant"
    | StringConstant -> "StringConstant"
    | OutputStmt -> "OutputStmt"
    | ArrayIndex -> "ArrayIndex"
    | UnOp unop -> "Unop::" ^ Ops.Unary.show unop
    | BinOp (b, types) -> (
        "Binop::" ^ Ops.Binary.show b
        ^
        match types with
        | None -> ""
        | Some (ta, tb) -> "::" ^ Type.show ta ^ "::" ^ Type.show tb)
    | LoadScalar t -> "LoadScalar::" ^ Type.show t
    | StoreScalar t -> "StoreScalar::" ^ Type.show t
    | Cast (from, to_) -> "Cast::" ^ Type.show from ^ "::" ^ Type.show to_

  let stats : (feature, int) Hashtbl.t = Hashtbl.create 1

  let json_stats () =
    let assoc =
      Hashtbl.to_seq stats
      |> Seq.map (fun (feature, count) ->
             (feature_to_string feature, `Int count))
      |> List.of_seq
    in
    `Assoc assoc

  let signal (feature : feature) =
    let current_count =
      Option.value ~default:0 (Hashtbl.find_opt stats feature)
    in
    Hashtbl.replace stats feature (current_count + 1)
end

let report file =
  let unhandled = Unhandled.json_stats () in
  let json = `Assoc [ ("unhandled", unhandled) ] in
  Yojson.Safe.to_file file json
