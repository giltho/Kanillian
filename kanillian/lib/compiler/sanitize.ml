let sanitize_symbol s =
  let replaced = Str.global_replace (Str.regexp {|[:\.\$]|}) "_" s in
  match String.get replaced 0 with
  | 'A' .. 'Z' | 'a' .. 'z' -> replaced
  | _ -> "m__" ^ replaced

let sanitize_symbol s =
  let new_s = sanitize_symbol s in
  new_s

let sanitizer =
  object
    inherit [unit] Visitors.map as super

    method! visit_expr_value ~ctx ~type_ value =
      match value with
      (* Rename any other symbol *)
      | Symbol s -> Symbol (sanitize_symbol s)
      | _ -> super#visit_expr_value ~ctx ~type_ value
  end

let sanitize_expr = sanitizer#visit_expr ~ctx:()
let sanitize_stmt = sanitizer#visit_stmt ~ctx:()

let sanitize_param (p : Param.t) =
  { p with identifier = Option.map sanitize_symbol p.identifier }

(** Sanitizes every variable symbol symbol. *)
let sanitize_program (prog : Program.t) =
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
    prog.vars;

  let new_funs = Hashtbl.create (Hashtbl.length prog.funs) in
  Hashtbl.iter
    (fun name func ->
      let new_name = sanitize_symbol name in
      let new_fun =
        Program.Func.
          {
            symbol = new_name;
            params = List.map sanitize_param func.params;
            body = Option.map sanitize_stmt func.body;
            location = func.location;
            return_type = func.return_type;
          }
      in
      Hashtbl.add new_funs new_name new_fun)
    prog.funs;

  { prog with funs = new_funs; vars = new_vars }
