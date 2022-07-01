(* This file should end up disappearing if everything goes well.
   It contains a way to filter out everything that is not called from main. *)

module Hashset = Gillian.Utils.Prelude.Hashset

let funs_in stmt =
  let funs = Hashset.empty () in
  let collector =
    object
      inherit [unit] Goto_lib.Visitors.iter as super

      method! visit_expr_value ~ctx ~type_ =
        function
        | FunctionCall { func = { value = Symbol x; _ }; args } ->
            Hashset.add funs x;
            List.iter (super#visit_expr ~ctx) args
        | e -> super#visit_expr_value ~ctx ~type_ e
    end
  in
  let () = collector#visit_stmt ~ctx:() stmt in
  funs

let funs_in_func ~(prog : Program.t) fname =
  let func = Hashtbl.find prog.funs fname in
  let stmt = Option.get func.body in
  funs_in stmt

let gather_funs ~(prog : Program.t) ~seen to_see =
  while not (Queue.is_empty to_see) do
    let fname = Queue.pop to_see in
    Hashset.add seen fname;
    let new_funcs = funs_in_func ~prog fname in
    Hashset.iter
      (fun x -> if not (Hashset.mem seen x) then Queue.push x to_see)
      new_funcs
  done

let from_main prog =
  let to_see = Queue.create () in
  Queue.push "main" to_see;
  let seen = Hashset.empty () in
  gather_funs ~prog ~seen to_see;
  seen

let filter_funs (prog : Program.t) =
  let to_keep = from_main prog in
  Hashtbl.filter_map_inplace
    (fun x s -> if Hashset.mem to_keep x then Some s else None)
    prog.funs
