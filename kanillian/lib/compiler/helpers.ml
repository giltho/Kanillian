module Gcu = struct
  include Compcert
  module Vt = Cgil_lib.ValueTranslation
end

let assert_unhandled ~feature args =
  let open Gil_syntax in
  let open Stats.Unhandled in
  let () = signal feature in
  let feature_string = Expr.string (feature_to_string feature) in
  Cmd.Fail ("unhandled", feature_string :: args)
