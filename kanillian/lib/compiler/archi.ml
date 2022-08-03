type t = Arch64 | Arch32

let a64 = [ Arch64 ]
let a32 = [ Arch32 ]
let any_arch = [ Arch32; Arch64 ]

let of_pointer_width = function
  | 32 -> Arch32
  | 64 -> Arch64
  | _ -> Error.unexpected "Archi that is neither 64 nor 32"
