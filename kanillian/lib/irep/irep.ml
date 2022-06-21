type t = { id : Id.t; sub : t list; named_sub : (Id.t * t) list }

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
