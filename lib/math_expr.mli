(** Math expression. *)
type expr =
  | Z of int           (** Integer *)
  | Add of expr list   (** e1 + e2 + e3 + ... *)
  | Sub of expr list   (** e1 - e2 - e3 - ... *)
  | Mul of expr list   (** e1 * e2 * e3 * ... *)
  | Div of expr * expr (** e1 / e2 *)

(** Expression is badly formed. *)
exception InvalidExpr of string

(** Initializes the random number generator. *)
val init: unit -> unit

(** Initializes the random number generator with the given seed. *)
val seed: int -> unit

(**
 * Generates a random expression with the given characteristics.
 *
 * min_depth: Minimum depth of the expression tree (inclusive)
 * max_depth: Maximum depth of the expression tree (exclusive)
 * width: Maximum width of the expression tree (inclusive)
 * min_const: Minimum value for constants (inclusive)
 * max_const: Maximum value for constants (exclusive)
 *)
val next_rand: int -> int -> int -> int -> int -> expr

(** Converts the given expression to a string. *)
val string_of_expr: expr -> string
