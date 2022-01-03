open Base

(** Rational number (numerator and denominator). *)
type rational

(** Attempt to evaluate an expression that includes constants other than integers and fractions. *)
exception NonRational of string

(** Instantiates a rational number. *)
val new_rational: int -> int -> rational

(** Converts the given rational number to a string. *)
val string_of_rational: rational -> string

(** Computes the sum of rational numbers. *)
val (+:): rational -> rational -> rational

(** Computes the difference of rational numbers. *)
val (-:): rational -> rational -> rational

(* Computes the negation of a single rational number. *)
val (~-:): rational -> rational

(** Computes the product of rational numbers. *)
val ( *: ): rational -> rational -> rational

(** Computes the quotient of rational numbers. *)
val (/:): rational -> rational -> rational

(** Checks whether two rational numbers are equal in value. *)
val (=:): rational -> rational -> bool

(** Checks whether two rational numbers are different in value. *)
val (<>:): rational -> rational -> bool

(** Checks whether a rational number is strictly less than another. *)
val (<:): rational -> rational -> bool

(** Checks whether a rational number is less than or equal to another. *)
val (<=:): rational -> rational -> bool

(** Checks whether a rational number is strictly greater than another. *)
val (>:): rational -> rational -> bool

(** Checks whether a rational number is greater than or equal to another. *)
val (>=:): rational -> rational -> bool

(** Evaluates an expression involving only rational numbers. *)
val eval_rational: expr -> (string * rational) list -> rational
