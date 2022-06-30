open Gil_syntax

type t = Annot.t * string option * string Cmd.t

let make ?loop ?label ?loc ?id cmd : t =
  let annot = Annot.make ?origin_loc:loc ?origin_id:id ?loop_info:loop () in
  (annot, label, cmd)
