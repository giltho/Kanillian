type unhandled_feature =
  | PointerConstantNotNull
  | SideEffect of Id.t
  | ConstantWithType of string
  | Expr of Id.t
  | Statement of Id.t
  | WeakOrVolatile

let unhandled_feature_to_string = function
  | PointerConstantNotNull -> "PointerConstantNotNull"
  | SideEffect id -> "SideEffect::" ^ Id.to_string id
  | ConstantWithType s -> "ConstantWithType::" ^ s
  | Expr id -> "Expr::" ^ Id.to_string id
  | Statement id -> "Statement::" ^ Id.to_string id
  | WeakOrVolatile -> "WeakOrVolatile"

exception Unexpected_irep of Irep.t option * string
exception Unhandled_irep of Irep.t option * unhandled_feature
exception Code_error of Irep.t option * string

let () =
  Printexc.register_printer (function
    | Unexpected_irep (irep, msg) ->
        let json =
          match irep with
          | None -> ""
          | Some irep -> Irep.to_yojson irep |> Yojson.Safe.pretty_to_string
        in
        Some (Fmt.str "Unexpected Irep:\n%s\n\n%s" msg json)
    | Unhandled_irep (irep, feature) ->
        let json =
          match irep with
          | None -> ""
          | Some irep -> Irep.to_yojson irep |> Yojson.Safe.pretty_to_string
        in
        let msg =
          Fmt.str "UNHANDLED IREP: %s\n" (unhandled_feature_to_string feature)
        in
        Fmt.pr "%s@?" msg;
        Some (Fmt.str "%s\n%s" msg json)
    | Code_error (irep, msg) ->
        let json =
          match irep with
          | None -> ""
          | Some irep ->
              Fmt.str "\n\nHappened while handling this irep:\n%a"
                (Yojson.Safe.pretty_print ~std:false)
                (Irep.to_yojson irep)
        in
        Some (Fmt.str "There seem to be a bug in Kanillain: %s.%s" msg json)
    | _ -> None)

let unexpected ?irep msg = raise (Unexpected_irep (irep, msg))
let unhandled ?irep feature = raise (Unhandled_irep (irep, feature))
let code_error ?irep msg = raise (Code_error (irep, msg))
