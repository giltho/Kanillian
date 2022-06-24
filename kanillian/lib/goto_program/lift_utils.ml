let lift_option f (irep : Irep.t) =
  match irep.id with
  | Nil -> None
  | _ -> Some (f irep)

let exactly_one ~failwith ?(msg = "") l =
  match l with
  | [ a ] -> a
  | _ -> failwith (msg ^ " does not have exactly one sub")

let exactly_two ~failwith ?(msg = "") l =
  match l with
  | [ a; b ] -> (a, b)
  | _ -> failwith (msg ^ " does not have exactly two subs")