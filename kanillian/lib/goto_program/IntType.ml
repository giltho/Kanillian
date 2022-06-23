type t = I_bool | I_char | I_int | I_size_t | I_ssize_t
[@@deriving show { with_path = false }]

module Bv_encoding = struct
  type int_type = t
  type t = { signed : bool; width : int } [@@deriving eq]

  let encode ~(machine : Machine_model.t) = function
    | I_int -> { signed = true; width = machine.int_width }
    | I_size_t -> { signed = false; width = machine.pointer_width }
    | I_char ->
        { signed = machine.char_is_unsigned; width = machine.char_width }
    | I_ssize_t -> { signed = true; width = machine.pointer_width }
    | I_bool -> failwith "kani doesn't encode bools as bitvectors"
end

let which_int_type_opt ~machine ~signed ~width =
  List.find_opt
    (fun it ->
      Bv_encoding.equal { signed; width } (Bv_encoding.encode ~machine it))
    [ I_int; I_size_t; I_char; I_size_t; I_ssize_t ]

let which_int_type ~machine ~signed ~width =
  match which_int_type_opt ~machine ~signed ~width with
  | None -> failwith "no known int type for this kind of bitvectors"
  | Some x -> x