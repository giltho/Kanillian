let lift_option f (irep : Irep.t) =
  match irep.id with
  | Nil -> None
  | _ -> Some (f irep)