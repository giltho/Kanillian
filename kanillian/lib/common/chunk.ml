type t =
  | Int8signed
  | Int8unsigned
  | Int16signed
  | Int16unsigned
  | Int32
  | Int64
  | Float32
  | Float64
[@@deriving yojson, eq]

let of_string = function
  | "int8signed" -> Int8signed
  | "int8unsigned" -> Int8unsigned
  | "int16signed" -> Int16signed
  | "int16unsigned" -> Int16unsigned
  | "int32" -> Int32
  | "int64" -> Int64
  | "float32" -> Float32
  | "float64" -> Float64
  | str -> failwith ("unknown chunk : " ^ str)

let to_string = function
  | Int8signed -> "int8signed"
  | Int8unsigned -> "int8unsigned"
  | Int16signed -> "int16signed"
  | Int16unsigned -> "int16unsigned"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Float32 -> "float32"
  | Float64 -> "float64"

let size = function
  | Int8signed | Int8unsigned -> 1
  | Int16signed | Int16unsigned -> 2
  | Int32 -> 4
  | Int64 -> 8
  | Float32 -> 4
  | Float64 -> 8

let align = function
  | Int8signed | Int8unsigned -> 1
  | Int16signed | Int16unsigned -> 2
  | Int32 -> 4
  | Int64 -> 8
  | Float32 -> 4
  | Float64 -> 8

let ptr = Int64
