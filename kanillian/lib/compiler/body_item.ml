open Gil_syntax

type t = Annot.t * string option * string Cmd.t

let compile_location (loc : Goto_lib.Location.t) =
  match loc.source with
  | None -> Location.none
  | Some source ->
      let pos_line = Option.value ~default:0 loc.line in
      let pos_column = Option.value ~default:0 loc.col in
      let loc_start = Location.{ pos_line; pos_column } in
      let loc_end = Location.{ pos_line; pos_column = pos_column + 2 } in
      Location.{ loc_source = source; loc_start; loc_end }

let make ?loop ?label ?loc ?id cmd : t =
  let annot = Annot.make ?origin_loc:loc ?origin_id:id ?loop_info:loop () in
  (annot, label, cmd)
