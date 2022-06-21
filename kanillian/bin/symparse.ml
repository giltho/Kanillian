let () =
  let filename = Sys.argv.(1) in
  let json = Yojson.Safe.from_file filename in
  let tbl = Irep_lib.Symtab.of_yojson json in
  match tbl with
  | Ok tbl -> Kanillian.Irep_to_gil.compile tbl |> ignore
  | Error msg -> Fmt.failwith "Failed with: %s" msg
