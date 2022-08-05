open Monadic
open Gil_syntax

type t [@@deriving yojson]

val alocs : t -> Utils.Containers.SS.t
val lvars : t -> Utils.Containers.SS.t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val substitution : le_subst:(Expr.t -> Expr.t) -> t -> t
val zero_of_chunk : Chunk.t -> t
val any_of_chunk : Chunk.t -> t Delayed.t
val of_chunk_and_expr : Chunk.t -> Expr.t -> t Delayed.t
val sure_is_zero : t -> bool
val to_gil_expr : t -> Expr.t

module SVArray : sig
  type sval = t
  type t = Arr of Expr.t | AllUndef | AllZeros [@@deriving yojson]

  val reduce : t -> t Delayed.t
  val pp : Format.formatter -> t -> unit
  val empty : t
  val is_empty : t -> Formula.t
  val sure_is_all_zeros : t -> bool
  val equal : t -> t -> bool
  val concat_knowing_size : t * Expr.t -> t * Expr.t -> t Delayed.t
  val concat : t -> t -> t option
  val to_single_value : chunk:Chunk.t -> t -> sval option Delayed.t
  val singleton : sval -> t
  val array_sub : t -> Expr.t -> Expr.t -> t
  val array_cons : sval -> t -> t option
  val array_append : t -> sval -> t option

  val to_gil_expr_undelayed :
    chunk:Chunk.t -> range:Expr.t * Expr.t -> t -> Expr.t * Formula.t list

  val to_gil_expr :
    chunk:Chunk.t -> range:Expr.t * Expr.t -> t -> Expr.t Delayed.t

  val of_gil_expr_exn : Expr.t -> t
  val learn_chunk : chunk:Chunk.t -> size:Expr.t -> t -> unit Delayed.t
  val subst : le_subst:(Expr.t -> Expr.t) -> t -> t
end

module Infix : sig
  val ( @: ) : SVArray.t -> SVArray.t -> SVArray.t option
  val ( ^: ) : t -> SVArray.t -> SVArray.t option
  val ( ^:? ) : t -> SVArray.t option -> SVArray.t option
end
