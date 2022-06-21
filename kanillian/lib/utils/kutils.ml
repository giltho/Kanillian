include Utils

exception Not_ok of string

module J = struct
  open Yojson.Safe.Util

  exception Parse_error of Yojson.Safe.t * string

  let ( $ ) x y = member y x
  let parse_error j msg = raise (Parse_error (j, msg))

  let catch_type_error (f : 'a -> 'b) : 'a -> 'b =
   fun x -> try f x with Type_error (s, j) -> parse_error j s

  let to_string = catch_type_error to_string
  let to_bool = catch_type_error to_bool
  let to_list = catch_type_error to_list
end
