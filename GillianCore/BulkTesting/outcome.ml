(** This is stupid, outcome is actually the interpreter. Doesn't really make sense, this should be changed. *)
module type S = sig
  module Val : Val.S
  module ESubst : ESubst.S with type vt = Val.t and type t = Val.et
  module Store : Store.S with type vt = Val.t

  module State :
    State.S
      with type vt = Val.t
       and type st = ESubst.t
       and type store_t = Store.t

  module ParserAndCompiler : ParserAndCompiler.S
  module External : External.S

  type t =
    | ParseAndCompileError of ParserAndCompiler.err
    | FailedExec of string
    | FinishedExec of
        (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t list

  val pp_what_test_did : Format.formatter -> t -> unit

  val pp_what_branch_did :
    Format.formatter ->
    (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t ->
    unit
end

module Make
    (ValP : Val.S)
    (ESubstP : ESubst.S with type vt = ValP.t and type t = ValP.et)
    (StoreP : Store.S with type vt = ValP.t)
    (StateP : State.S
                with type vt = ValP.t
                 and type st = ESubstP.t
                 and type store_t = StoreP.t)
    (PC : ParserAndCompiler.S)
    (ExternalP : External.S) :
  S
    with module Val = ValP
     and module ESubst = ESubstP
     and module Store = StoreP
     and module State = StateP
     and module ParserAndCompiler = PC
     and module External = ExternalP = struct
  module Val = ValP
  module ParserAndCompiler = PC
  module State = StateP
  module ESubst = ESubstP
  module Store = StoreP
  module External = ExternalP

  type t =
    | ParseAndCompileError of ParserAndCompiler.err
    | FailedExec of string
    | FinishedExec of
        (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t list

  let pp_what_test_did fmt = function
    | ParseAndCompileError e ->
        Fmt.pf fmt "failed at parsing time with error: %a"
          ParserAndCompiler.pp_err e
    | FailedExec msg ->
        Fmt.pf fmt "failed at execution with message: \"%s\"" msg
    | FinishedExec [ RSucc _ ] ->
        Fmt.pf fmt "finished its execution successfully"
    | FinishedExec [ RFail { proc; proc_idx; errors; _ } ] ->
        Fmt.pf fmt
          "finished its execution with failure in proc %s at command %i with \
           errors: %a"
          proc proc_idx
          (Fmt.Dump.list (ExecErr.pp Val.pp State.pp_err))
          errors
    | FinishedExec _ ->
        Fmt.pf fmt "finished its execution with several branches"

  let pp_what_branch_did fmt b =
    ExecRes.pp_what_exec_did Val.pp (ExecErr.pp Val.pp State.pp_err) fmt b
end

module Make_Concrete (CMemory : CMemory.S) =
  Make (CVal.M) (CVal.CESubst) (CStore) (CState.Make (CMemory))

module Make_Symbolic (SMemory : SMemory.S) =
  Make (SVal.M) (SVal.SESubst) (SStore) (SState.Make (SMemory))
