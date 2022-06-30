let sanitize_symbol s = Str.global_replace (Str.regexp "[:]") "_" s

let sanitizer =
  object
    inherit [unit] Visitors.map as super

    method! visit_expr_value ~ctx ~type_ value =
      match value with
      | Symbol s -> Symbol (sanitize_symbol s)
      | _ -> super#visit_expr_value ~ctx ~type_ value
  end

let sanitize_expr = sanitizer#visit_expr ~ctx:()
let sanitize_stmt = sanitizer#visit_stmt ~ctx:()

(** Sanitizes every variable symbol symbol. *)
let sanitize_program_in_place (prog : Program.t) =
  (* Create a second table with the new vars *)
  let new_vars = Hashtbl.create (Hashtbl.length prog.vars) in
  Hashtbl.iter
    (fun name gvar ->
      let new_name = sanitize_symbol name in
      let new_gvar =
        Program.Global_var.
          {
            type_ = gvar.type_;
            symbol = new_name;
            value = Option.map sanitize_expr gvar.value;
            location = gvar.location;
          }
      in
      Hashtbl.add new_vars new_name new_gvar)
    new_vars;
  (* Override the old table *)
  Hashtbl.clear prog.vars;
  Hashtbl.iter (fun name gvar -> Hashtbl.add prog.vars name gvar) new_vars;

  Hashtbl.filter_map_inplace
    (fun _ func ->
      Some Program.Func.{ func with body = Option.map sanitize_stmt func.body })
    prog.funs
