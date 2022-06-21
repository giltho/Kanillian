let () =
  let filename = Sys.argv.(1) in
  let json = Yojson.Safe.from_file filename in
  let tbl = Irep_lib.Symtab.of_yojson json in
  match tbl with
  | Ok tbl -> Printf.printf "Success! There are %d symbols" (Hashtbl.length tbl)
  | Error msg -> Fmt.failwith "Failed with: %s" msg
