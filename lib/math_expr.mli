(** Math expression. *)
type expr =
  | Z of int           (** Integer *)
  | Var of string      (** Named variable *)
  | Neg of expr        (** -e1 *)
  | Add of expr list   (** e1 + e2 + e3 + ... *)
  | Mul of expr list   (** e1 * e2 * e3 * ... *)
  | Div of expr * expr (** e1 / e2 *)

(** Expression is badly formed. *)
exception InvalidExpr of string

(** Expression involves an invalid operation (e.g. division by zero). *)
exception Undefined of string

(** Attempt to evaluate an expression when some variables were not given a value. *)
exception UndefinedVariable of string

(** Attempt to evaluate an expression when some variables were given multiple values.
    This exception is raised even if the values are all the same. *)
exception MultipleDefinitions of string

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
val next_rand: int -> int -> int -> int -> int -> expr

(** Converts the given expression to a string. *)
val string_of_expr: expr -> string

(** Evaluates the given expression. *)
val eval: expr -> (string * float) list -> float

(** Simplifies the given expression. *)
val simplify: expr -> expr
