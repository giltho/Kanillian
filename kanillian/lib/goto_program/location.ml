(** This is a much simplified version of CBMC's locations.
    Gillian only handles this, so there's not much more we can do.
    Maybe there is a way to extract a better gillian location from
    CBMC locs, but it's not worth it right now. *)
type tt = { source : string; line : int option; col : int option }
[@@deriving show { with_path = false }]

type t = tt option [@@deriving show { with_path = false }]

let of_irep (irep : Irep.t) : t =
  let open Irep.Infix in
  match irep with
  | { id = Nil; _ } -> None
  | { id = EmptyString; _ } ->
      irep $? File
      |> Option.map (fun file_rep ->
             let source = Irep.as_just_string file_rep in
             let line = Option.map Irep.as_just_int (irep $? Line) in
             let col = Option.map Irep.as_just_int (irep $? Column) in
             { source; line; col })
  | _ -> Gerror.fail ~irep "wrong Irep location"

let sloc_in_irep irep =
  let open Irep.Infix in
  irep $? CSourceLocation |> Option.value ~default:Irep.nil |> of_irep
