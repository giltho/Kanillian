module Unhandled = struct
  type feature =
    | CompositNondet
    | ReturnByCopy
    | CallArgumentByCopy
    | DeclCompositValue
    | ByteExtract
    | StructConstant
  [@@deriving show { with_path = false }]

  let stats : (feature, int) Hashtbl.t = Hashtbl.create 1

  let signal (feature : feature) =
    let current_count =
      Option.value ~default:0 (Hashtbl.find_opt stats feature)
    in
    Hashtbl.replace stats feature (current_count + 1)
end