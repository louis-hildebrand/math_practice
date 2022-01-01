(** Math expression. *)
type expr =
  | Z of int           (** Integer *)
  | Var of string      (** Named variable *)
  | Neg of expr        (** -e1 *)
  | Add of expr list   (** e1 + e2 + e3 + ... *)
  | Mul of expr list   (** e1 * e2 * e3 * ... *)
  | Div of expr * expr (** e1 / e2 *)

(** Rational number (numerator and denominator). *)
type rational

(** Expression is badly formed. *)
exception InvalidExpr of string

(** Expression involves an invalid operation (e.g. division by zero). *)
exception Undefined of string

(** Attempt to evaluate an expression when some variables were not given a value. *)
exception UndefinedVariable of string

(** Attempt to evaluate an expression when some variables were given multiple values.
    This exception is raised even if the values are all the same. *)
exception MultipleDefinitions of string

(** Instantiates a rational number. *)
val new_rational: int -> int -> rational

(** Checks whether two rational numbers are equal in value. *)
val equal_rational: rational -> rational -> bool

(** Converts the given expression to a string. *)
val string_of_expr: expr -> string

(** Converts the given rational number to a string. *)
val string_of_rational: rational -> string

(** Evaluates the given expression. *)
val eval: expr -> (string * float) list -> float

(** Evaluates an expression involving only rational numbers. *)
val evalr: expr -> (string * rational) list -> rational

(** Simplifies the given expression. *)
val simplify: expr -> expr
