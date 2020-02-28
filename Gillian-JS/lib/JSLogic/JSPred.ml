open JSLogicCommon
module Type = Gillian.Gil_syntax.Type
module Pred = Jsil_syntax.Pred

type t = {
  name : string;
  num_params : int;
  params : (string * Type.t option) list;
  ins : int list;
  definitions : ((string * string list) option * JSAsrt.t) list;
  pure : bool;
}

let js2jsil
    (pred_def : t)
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fun_tbl : pre_fun_tbl_type) : Pred.t =
  let jsil_definitions =
    List.map
      (fun (os, a) -> (os, JSAsrt.js2jsil None cc_tbl vis_tbl fun_tbl None a))
      pred_def.definitions
  in
  {
    Pred.name = pred_def.name;
    num_params = pred_def.num_params;
    params = pred_def.params;
    ins = pred_def.ins;
    definitions = jsil_definitions;
    pure = pred_def.pure;
    normalised = false;
  }
