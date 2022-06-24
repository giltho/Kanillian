exception Lift_error of Irep.t option * string

let () =
  Printexc.register_printer (function
    | Lift_error (irep, msg) ->
        let json =
          match irep with
          | None -> ""
          | Some irep -> Irep.to_yojson irep |> Yojson.Safe.pretty_to_string
        in
        Some (Fmt.str "Error in irep translation:\n%s\n\n%s" msg json)
    | _ -> None)

let fail ?irep msg = raise (Lift_error (irep, msg))