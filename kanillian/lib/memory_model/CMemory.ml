open Gillian.Concrete

type vt = Values.t
type st = Subst.t
type err_t = unit
type t = unit
type action_ret = ASucc of (t * vt list) | AFail of err_t list

let init () = ()
let execute_action _ _ _ = failwith "c_memory not implemented in Kanillian"
let copy () = ()
let pp _ () = ()
let pp_err _ () = ()
