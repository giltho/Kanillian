let loc_in_irep v_of_irep irep =
  let open Irep.Infix in
  irep $? CSourceLocation
  |> Option.value ~default:(Irep.make Nil)
  |> Location.of_irep
