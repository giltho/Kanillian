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
    | OutputStmt

  let feature_to_string = function
    | CompositNondet -> "CompositNondet"
    | ReturnByCopy -> "ReturnByCopy"
    | CallArgumentByCopy -> "CallArgumentByCopy"
    | DeclCompositValue -> "DeclCompositValue"
    | ByteExtract -> "ByteExtract"
    | StructConstant -> "StructConstant"
    | OutputStmt -> "OutputStmt"

  let feature_of_string = function
    | "CompositNondet" -> Some CompositNondet
    | "ReturnByCopy" -> Some ReturnByCopy
    | "CallArgumentByCopy" -> Some CallArgumentByCopy
    | "DeclCompositValue" -> Some DeclCompositValue
    | "ByteExtract" -> Some ByteExtract
    | "StructConstant" -> Some StructConstant
    | "OutputStmt" -> Some OutputStmt
    | _ -> None

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

let init_from file =
  if not (Sys.file_exists file) then ()
  else
    let open J.Infix in
    let json = Yojson.Safe.from_file file in
    json $? "unhandled"
    |> Option.iter
         (J.iter_obj (fun (feature, count) ->
              let open Unhandled in
              match feature_of_string feature with
              | None -> Error.user_error "Unknown feature in the kstats file"
              | Some feature -> Hashtbl.replace stats feature (J.to_int count)))

let report file =
  let unhandled = Unhandled.json_stats () in
  let json = `Assoc [ ("unhandled", unhandled) ] in
  Yojson.Safe.to_file file json
