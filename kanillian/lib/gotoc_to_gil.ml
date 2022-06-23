open Gil_syntax
module GExpr = Goto_lib.Expr

(** Gillian-C utils for compilation*)
module Gcu = struct
  include Compcert
  module Vt = Cgil_lib.ValueTranslation
end

let sanitize_symbol s = Str.global_replace (Str.regexp "[:]") "_" s
let as_gil_variable e = GExpr.as_symbol e |> sanitize_symbol

let compile_location (loc : Goto_lib.Location.t) =
  match loc with
  | None -> Location.none
  | Some loc ->
      let pos_line = Option.value ~default:0 loc.line in
      let pos_column = Option.value ~default:0 loc.col in
      let loc_start = Location.{ pos_line; pos_column } in
      let loc_end = Location.{ pos_line; pos_column = pos_column + 2 } in
      Location.{ loc_source = loc.source; loc_start; loc_end }

let find_main symtab = Hashtbl.find symtab "main"

module Body_item = struct
  type t = Annot.t * string option * string Cmd.t

  let make ?loop ?label ?loc cmd : t =
    let annot = Annot.make ?origin_loc:loc ?loop_info:loop () in
    (annot, label, cmd)
end

let compile_expr (expr : GExpr.t) : Body_item.t list * Expr.t =
  (* let loc = compile_location expr.location in
     let b = Body_item.make ~loc in *)
  match expr.value with
  | Symbol s -> ([], PVar s)
  | BoolConstant b ->
      let i = if b then Gcu.Camlcoq.Z.one else Gcu.Camlcoq.Z.zero in
      let lit = Gcu.Vt.gil_of_compcert (Gcu.Values.Vint i) in
      ([], Lit lit)
  | IntConstant z ->
      let int_ty = Goto_lib.Type.as_int_type expr.type_ in
      let cz = Gcu.Vt.z_of_int z in
      let ccert_value =
        let open Gcu.Values in
        match int_ty with
        | I_int | I_char -> Vint cz
        | I_size_t -> (
            match !Kconfig.machine_model.pointer_width with
            | 32 -> Vint cz
            | 64 -> Vlong cz
            | _ -> failwith "Gillian only handles archi 32 and archi 64 for now"
            )
        | I_ssize_t ->
            failwith "Lookup in compcert what kind of value ssize_t is"
        | I_bool -> failwith "IntConstant with type I_bool"
      in
      let lit = Gcu.Vt.gil_of_compcert ccert_value in

      ([], Lit lit)
(* | _ -> failwith "Cannot compile expr yet" *)

let rec compile_statement (stmt : Stmt.t) : Body_item.t list =
  let loc = compile_location stmt.location in
  let b = Body_item.make ~loc in
  let add_annot x = List.map b x in
  match stmt.body with
  | Skip -> [ b Skip ]
  | Block ss -> List.concat_map compile_statement ss
  | Return e ->
      let s, e =
        match e with
        | Some e -> compile_expr e
        | None -> ([], Lit Null)
      in
      let variable = Utils.Names.return_variable in
      s @ add_annot [ Assignment (variable, e); ReturnNormal ]
  | Decl { lhs; value } ->
      let lhs = as_gil_variable lhs in
      let s, v =
        match value with
        | Some e -> compile_expr e
        | None -> ([], Lit Undefined)
      in
      s @ [ b (Assignment (lhs, v)) ]
(* | _ -> failwith "Cannot compile statement yet" *)

let compile_function ~(params : Param.t list) (sym : Gsymbol.t) :
    (Annot.t, string) Proc.t =
  if sym.is_volatile || sym.is_weak then
    failwith "Cannot handled volatile or weak data yet";
  let f_loc = compile_location sym.location in
  let proc_params = List.map (fun x -> Option.get x.Param.identifier) params in
  let proc_spec = None in
  let stmt =
    match sym.value with
    | Stmt s -> s
    | _ -> failwith "symbol value of function is not a statement"
  in
  let proc_body = Array.of_list (compile_statement stmt) in
  Proc.
    {
      proc_name = sym.name;
      proc_source_path = Some f_loc.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
    }

let compile (symtab : Gsymtab.t) : (Annot.t, string) Prog.t =
  let prog = Prog.create () in
  (* let functions = Hashtbl.fold (fun (s, m) -> if) *)
  let main = find_main symtab in
  let params =
    match main.type_ with
    | Code { params; _ } -> params
    | _ -> failwith "main is not code??"
  in
  let prog = Prog.add_proc prog (compile_function ~params main) in
  prog
