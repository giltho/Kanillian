include Utils

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
  let to_assoc = catch_type_error to_assoc
end

let z_of_hex s =
  let ret = ref Z.zero in
  String.iter
    (fun c ->
      let to_add =
        match c with
        | '0' .. '9' -> Char.code c - Char.code '0'
        | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
        | _ -> failwith "invalid hexadecimal value"
      in
      ret := Z.add (Z.shift_left !ret 4) (Z.of_int to_add))
    s;
  !ret
