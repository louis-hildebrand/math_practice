(** Initializes the random number generator. *)
val init: unit -> unit

(** Initializes the random number generator with the given seed. *)
val seed: int -> unit

(**
 * Generates a random expression with the given characteristics.
 *
 * min_depth: Minimum depth of the expression tree (inclusive)
 * max_depth: Maximum depth of the expression tree (inclusive)
 * width: Maximum width of the expression tree (inclusive)
 * min_const: Minimum value for constants (inclusive)
 * max_const: Maximum value for constants (exclusive)
 *)
val next_fractional: int -> int -> int -> int -> int -> Base.expr
