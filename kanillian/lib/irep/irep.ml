type t = { id : Id.t; sub : t list; named_sub : (Id.t * t) list }

let make ?(sub = []) ?(named_sub = []) id = { id; sub; named_sub }
let lookup name irep = List.assoc name irep.named_sub
let lookup_opt name irep = List.assoc_opt name irep.named_sub

module Infix = struct
  let ( $ ) irep name = lookup name irep
  let ( $$ ) irep name = lookup (Id.of_string name) irep
  let ( $? ) irep name = lookup_opt name irep
  let ( $$? ) irep name = lookup_opt (Id.of_string name) irep
end

let as_just_string irep = Id.to_string irep.id
let as_just_int irep = Id.to_int irep.id

let as_just_bitpattern ~width ~signed irep =
  Id.to_bitpattern ~width ~signed irep.id

let is_nil irep =
  match irep.id with
  | Id.Nil -> true
  | _ -> false

let rec of_yojson json =
  let open Kutils.J in
  let id = Id.of_yojson (json $ "id") in
  let sub =
    match json $ "sub" with
    | `Null -> []
    | `List l -> List.map of_yojson l
    | _ -> Kutils.J.parse_error json "sub is not a list"
  in
  let named_sub =
    match json $ "namedSub" with
    | `Null -> []
    | `Assoc l -> List.map (fun (n, j) -> (Id.of_string n, of_yojson j)) l
    | _ -> Kutils.J.parse_error json "namedSub is not an object!"
  in
  { id; sub; named_sub }
