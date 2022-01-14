(** {!Base} defines the expression type and provides a few simple functions for handling them. *)

open Rational

(** Math expression. *)
type expr =
  | Z of int           (** Integer *)
  | R of float         (** Real number *)
  | Var of string      (** Named variable *)
  | Neg of expr        (** -e1 *)
  | Add of expr list   (** e1 + e2 + e3 + ... *)
  | Mul of expr list   (** e1 * e2 * e3 * ... *)
  | Div of expr * expr (** e1 / e2 *)
  | Pow of expr * expr (** e1^e2 *)

(** Expression is badly formed. *)
exception InvalidExpr of string

(** Expression involves an invalid operation (e.g. division by zero). *)
exception Undefined of string

(** Attempt to evaluate an expression when some variables were not given a value. *)
exception UndefinedVariable of string

(** Attempt to evaluate an expression when some variables were given multiple values.
    This exception is raised even if the values are all the same. *)
exception MultipleDefinitions of string

(** Attempt to evaluate an expression that includes constants other than integers and fractions. *)
exception NonRational of string

(** Converts the given expression to a string. *)
val string_of_expr: expr -> string

(** Evaluates the given expression. *)
val eval: expr -> (string * float) list -> float

(** Evaluates an expression involving only rational numbers. *)
val eval_rational: expr -> (string * rational) list -> rational
