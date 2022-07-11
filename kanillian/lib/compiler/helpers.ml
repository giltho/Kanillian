module Stats = struct
  module Unhandled = struct
    type feature =
      | CompositNondet
      | ReturnByCopy
      | CallArgumentByCopy
      | DeclCompositValue
      | ByteExtract
      | StructConstant
      | OutputStmt
    [@@deriving show { with_path = false }]

    let stats : (feature, int) Hashtbl.t = Hashtbl.create 1

    let json_stats () =
      let assoc =
        Hashtbl.to_seq stats
        |> Seq.map (fun (feature, count) -> (show_feature feature, `Int count))
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
end

module Gcu = struct
  include Compcert
  module Vt = Cgil_lib.ValueTranslation
end

let assert_unhandled ~feature args =
  let open Gil_syntax in
  let open Stats.Unhandled in
  let () = signal feature in
  let feature_string = Expr.string (show_feature feature) in
  Cmd.Fail ("unhandled", feature_string :: args)
