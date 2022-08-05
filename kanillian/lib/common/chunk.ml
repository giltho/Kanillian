type t = U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128 | F32 | F64
[@@deriving eq, yojson]

let to_string = function
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | U128 -> "u128"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | I128 -> "i128"
  | F32 -> "f32"
  | F64 -> "f64"

let of_string = function
  | "u8" -> U8
  | "u16" -> U16
  | "u32" -> U32
  | "u64" -> U64
  | "u128" -> U128
  | "i8" -> I8
  | "i16" -> I16
  | "i32" -> I32
  | "i64" -> I64
  | "i128" -> I128
  | "f32" -> F32
  | "f64" -> F64
  | _ -> failwith "invalid chunk"

let bounds =
  let open Z in
  function
  | U8 -> Some (zero, (one lsl 8) - one)
  | U16 -> Some (zero, (one lsl 16) - one)
  | U32 -> Some (zero, (one lsl 32) - one)
  | U64 -> Some (zero, (one lsl 64) - one)
  | U128 -> Some (zero, (one lsl 128) - one)
  | I8 -> Some (neg (one lsl 7), (one lsl 7) - one)
  | I16 -> Some (neg (one lsl 15), (one lsl 15) - one)
  | I32 -> Some (neg (one lsl 31), (one lsl 31) - one)
  | I64 -> Some (neg (one lsl 63), (one lsl 63) - one)
  | I128 -> Some (neg (one lsl 127), (one lsl 127) - one)
  | F32 | F64 -> None

let size = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 | F32 -> 4
  | U64 | I64 | F64 -> 8
  | U128 | I128 -> 16

let align = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 | F32 -> 4
  | U64 | I64 | F64 -> 8
  | U128 | I128 -> 16

let of_int_type ~signed ~size =
  match (size, signed) with
  | 8, true -> Some I8
  | 8, false -> Some U8
  | 16, true -> Some I16
  | 16, false -> Some U16
  | 32, true -> Some I32
  | 32, false -> Some U32
  | 64, true -> Some I64
  | 64, false -> Some U64
  | 128, true -> Some I128
  | 128, false -> Some U128
  | _ -> None
