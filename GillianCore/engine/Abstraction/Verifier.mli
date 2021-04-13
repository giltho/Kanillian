module type S = sig
  module SAInterpreter : GInterpreter.S

  type t

  type result_t

  val start_time : float ref

  val reset : unit -> unit

  val verify_prog :
    (Annot.t, int) Prog.t -> bool -> SourceFiles.t option -> unit

  val verify_up_to_procs :
    (Annot.t, int) Prog.t -> result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit
end

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) : S
