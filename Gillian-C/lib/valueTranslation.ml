open Compcert
module Literal = Gillian.Gil_syntax.Literal
open CConstants

let true_name id =
  let str = Camlcoq.extern_atom id in
  if str.[0] = '$' then Prefix.uvar ^ String.sub str 1 (String.length str - 1)
  else str

let float_of_z z = float_of_int (Camlcoq.Z.to_int z)

let z_of_float z = Camlcoq.Z.of_sint (int_of_float z)

let string_of_chunk chunk =
  let open AST in
  match chunk with
  | Mint8signed    -> "int8signed"
  | Mint8unsigned  -> "int8unsigned"
  | Mint16signed   -> "int16signed"
  | Mint16unsigned -> "int16unsigned"
  | Mint32         -> "int32"
  | Mint64         -> "int64"
  | Mfloat32       -> "float32"
  | Mfloat64       -> "float64"
  | Many32         -> "any32"
  | Many64         -> "any64"

let chunk_of_string str =
  let open AST in
  match str with
  | "int8signed"    -> Mint8signed
  | "int8unsigned"  -> Mint8unsigned
  | "int16signed"   -> Mint16signed
  | "int16unsigned" -> Mint16unsigned
  | "int32"         -> Mint32
  | "int64"         -> Mint64
  | "float32"       -> Mfloat32
  | "float64"       -> Mfloat64
  | "any32"         -> Many32
  | "any64"         -> Many64
  | _               -> failwith ("unknown chunk : " ^ str)

let loc_name_of_block block =
  let int_block = Camlcoq.P.to_int block in
  let string_block = string_of_int int_block in
  Prefix.loc ^ string_block

let block_of_loc_name loc_name =
  let size_int = String.length loc_name - 2 in
  let string_block = String.sub loc_name 2 size_int in
  let int_block = int_of_string string_block in
  Camlcoq.P.of_int int_block

let compcert_size_of_gil = z_of_float

let gil_size_of_compcert = float_of_z

let compcert_of_gil gil_value =
  let open Literal in
  let open Values in
  match gil_value with
  | Undefined -> Vundef
  | LList [ String typ; Num ocaml_float ] when String.equal typ VTypes.int_type
    ->
      let coq_int = z_of_float ocaml_float in
      Vint coq_int
  | LList [ String typ; Num ocaml_float ]
    when String.equal typ VTypes.float_type ->
      let coq_float = Camlcoq.coqfloat_of_camlfloat ocaml_float in
      Vfloat coq_float
  | LList [ String typ; Num ocaml_float ]
    when String.equal typ VTypes.single_type ->
      let coq_float = Camlcoq.coqfloat_of_camlfloat ocaml_float in
      Vsingle coq_float
  | LList [ String typ; Num ocaml_float ] when String.equal typ VTypes.long_type
    ->
      let coq_int = z_of_float ocaml_float in
      Vlong coq_int
  | LList [ Loc loc; Num ocaml_ofs ] ->
      let block = block_of_loc_name loc in
      let ptrofs = z_of_float ocaml_ofs in
      Vptr (block, ptrofs)
  | _ ->
      failwith
        (Format.asprintf "Invalid serialization of value : %a" pp gil_value)

let gil_of_compcert compcert_value =
  let open Literal in
  let open Values in
  match compcert_value with
  | Vundef               -> Undefined
  | Vint i               ->
      let ocaml_float = float_of_z i in
      LList [ String VTypes.int_type; Num ocaml_float ]
  | Vfloat f             ->
      let ocaml_float = Camlcoq.camlfloat_of_coqfloat f in
      LList [ String VTypes.float_type; Num ocaml_float ]
  | Vsingle f32          ->
      let ocaml_float = Camlcoq.camlfloat_of_coqfloat32 f32 in
      LList [ String VTypes.single_type; Num ocaml_float ]
  | Vlong i64            ->
      let ocaml_float = float_of_z i64 in
      LList [ String VTypes.long_type; Num ocaml_float ]
  | Vptr (block, ptrofs) ->
      let loc = loc_name_of_block block in
      let ocaml_ofs = float_of_z ptrofs in
      LList [ Loc loc; Num ocaml_ofs ]

let string_of_permission perm =
  let open Compcert.Memtype in
  match perm with
  | Freeable -> "Freeable"
  | Writable -> "Writable"
  | Readable -> "Readable"
  | Nonempty -> "Nonempty"

let permission_of_string str =
  let open Compcert.Memtype in
  match str with
  | "Freeable" -> Freeable
  | "Writable" -> Writable
  | "Readable" -> Readable
  | "Nonempty" -> Nonempty
  | _          -> failwith ("Unkown permission : " ^ str)

let permission_opt_of_string str =
  try Some (permission_of_string str)
  with Failure _ ->
    if String.equal "None" str then None
    else failwith ("Unkown optional permission : " ^ str)

let string_of_permission_opt p_opt =
  match p_opt with
  | None   -> "None"
  | Some p -> string_of_permission p

let compcert_block_of_gil gil_block =
  let open Gillian.Gil_syntax.Literal in
  match gil_block with
  | LList [ Loc l; Num low; Num high ] ->
      ((block_of_loc_name l, z_of_float low), z_of_float high)
  | _ -> failwith (Format.asprintf "Invalid block to free : %a" pp gil_block)

let gil_init_data init_data =
  let open Literal in
  let num_i n = Num (float_of_z n) in
  let open Compcert.AST in
  match init_data with
  | Init_int8 n            -> LList [ String "int8"; num_i n ]
  | Init_int16 n           -> LList [ String "int16"; num_i n ]
  | Init_int32 n           -> LList [ String "int32"; num_i n ]
  | Init_int64 n           -> LList [ String "int64"; num_i n ]
  | Init_float32 n         ->
      LList [ String "float32"; Num (Camlcoq.camlfloat_of_coqfloat32 n) ]
  | Init_float64 n         ->
      LList [ String "float64"; Num (Camlcoq.camlfloat_of_coqfloat n) ]
  | Init_space n           -> LList [ String "space"; num_i n ]
  | Init_addrof (sym, ofs) ->
      LList [ String "addrof"; String (true_name sym); num_i ofs ]
