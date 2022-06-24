let loc_in_irep v_of_irep irep =
  let open Irep.Infix in
  irep $? CSourceLocation |> Option.value ~default:Irep.nil |> Location.of_irep
