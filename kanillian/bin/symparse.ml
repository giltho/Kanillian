let machine = Goto_lib.Machine_model.archi64

let () =
  let filename = Sys.argv.(1) in
  let json = Yojson.Safe.from_file filename in
  let tbl = Irep_lib.Symtab.of_yojson json in
  match tbl with
  | Ok tbl ->
      let main = Hashtbl.find tbl "main" in
      let tbl = Hashtbl.create 1 in
      let () = Hashtbl.add tbl "main" main in
      let lifted = Goto_lib.Gsymtab.of_symtab ~machine tbl in
      Kanillian.Gotoc_to_gil.compile lifted |> ignore
  | Error msg -> Fmt.failwith "Failed with: %s" msg
