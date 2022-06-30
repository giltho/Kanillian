(** Throw this exception if some unexpected goto
    was received for compilation *)
exception UnexpectedGoto of string

(** Throw this exception if some valid goto was received,
    but it is not handled yet. *)
exception UnhandledGoto of string

(** Throw this exception if there is a bug in the compiler code *)
exception Code_error of string

(** Throw this exception if the user made a mistake when invoking Kanillian *)
exception User_error of string

let () =
  Printexc.register_printer (function
    | UnexpectedGoto msg ->
        Some ("Did not expect to receive this goto for compilation:\n\n" ^ msg)
    | UnhandledGoto msg -> Some ("Goto fragment not handled yet:\n\n" ^ msg)
    | Code_error msg ->
        Some ("There is a bug in the Kanillian compiler:\n\n" ^ msg)
    | User_error msg -> Some ("User error - " ^ msg)
    | _ -> None)

let unexpected s = raise (UnexpectedGoto s)
let unhandled s = raise (UnhandledGoto s)
let code_error s = raise (Code_error s)
let user_error s = raise (User_error s)
