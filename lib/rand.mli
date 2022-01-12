open Rational

(** Initializes the random number generator. *)
val init: unit -> unit

(** Initializes the random number generator with the given seed. *)
val seed: int -> unit

(** Generates a random expression containing fractions.
  @param min_depth Minimum depth of the expression tree (inclusive)
  @param max_depth Maximum depth of the expression tree (inclusive)
  @param width Maximum width of the expression tree (inclusive)
  @param min_const Minimum value for constants (inclusive)
  @param max_const Maximum value for constants (exclusive)
  @param max_denom Maximum value for the denominators of fractions (inclusive)
 *)
val next_fractional: int -> int -> int -> rational -> rational -> int -> Base.expr

(** Generates a random expression containing decimal numbers.
  @param min_depth Minimum depth of the expression tree (inclusive)
  @param max_depth Maximum depth of the expression tree (inclusive)
  @param width Maximum width of the expression tree (inclusive)
  @param min_const Minimum value for constants (inclusive)
  @param max_const Maximum value for constants (exclusive)
  @param decimal_places Maximum number of decimal places in constants (inclusive)
 *)
val next_decimal: int -> int -> int -> float -> float -> int -> Base.expr
