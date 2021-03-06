(** {!Rational} defines rational numbers and related functions. *)

(** Rational number (numerator and denominator). *)
type rational

(** Attempt to take an even root of a negative number. *)
exception Root_negative of int

(** Attempt to evaluate an expression that involves non-rational numbers. *)
exception NonRational of string

(** Instantiates a rational number. *)
val new_rational: int -> int -> rational

(** Converts the given rational number to a string. *)
val string_of_rational: rational -> string

(** Converts the given rational number to the equivalent floating-point number. *)
val float_of_rational: rational -> float

(** Exposes the numerator and denominator of the simplified fraction. *)
val split: rational -> int * int

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

(** Computes the result of exponentiation, provided the result is rational. *)
val (^:): rational -> rational -> rational

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
