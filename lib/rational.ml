open Base
open Printf

type rational = int * int

(* Helper functions ------------------------------------------------------------------------------------------------- *)
(* Computes the greatest common divisor of n and m. *)
let gcd (n: int) (m: int): int =
  let rec gcd' big small =
    if small = 0 then big
    else gcd' small (big mod small)
  in
  let (n', m') = (abs n, abs m) in
  gcd' (max n' m') (min n' m')

(* Reduces the given fraction.
 * WARNING: reduce_fraction does NOT check for division by zero. It is NOT safe to call reduce_fraction 0 0.
 *)
let reduce_fraction ((numer: int), (denom: int)): (int * int) =
  let g =
    if denom > 0 then gcd numer denom 
    else ~- (gcd numer denom)
  in
  (numer / g, denom / g)

(* Public functions ------------------------------------------------------------------------------------------------- *)
let new_rational (numerator: int) (denominator: int): rational =
  if denominator = 0 then
    raise (Undefined 
      (sprintf "Attempt to initialize rational number with denominator of zero (%d/%d)." numerator denominator))
  else
    let (n', d') = reduce_fraction (numerator, denominator) in
    (n', d')

(* TODO: Remove *)
exception NotImplemented

(** Computes the sum of rational numbers. *)
let (+:) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2) *)
  (* If overflow ever becomes a problem, try finding the LCM of the denominators instead *)
  (n1*d2 + n2*d1, d1*d2)

(** Computes the difference of rational numbers. *)
let (-:) ((n1, d1): rational) ((n2, d2): rational): rational =
  raise NotImplemented

(* Computes the negation of a single rational number. *)
let (~-:) (x: rational): rational =
  (new_rational 0 1) -: x

(** Computes the product of rational numbers. *)
let ( *: ) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* n1/d1 * n2/d2 = (n1*n2)/(d1*d2) *)
  (n1 * n2, d1 * d2)

(** Computes the quotient of rational numbers. *)
let (/:) ((n1, d1): rational) ((n2, d2): rational): rational =
  raise NotImplemented

(* Checks whether two rational numbers are equal in value. *)
let (=:) (x: rational) (y: rational): bool =
  reduce_fraction x = reduce_fraction y

(** Checks whether a rational number is strictly less than another. *)
let (<:) (x: rational) (y: rational): bool =
  raise NotImplemented

(** Checks whether a rational number is less than or equal to another. *)
let (<=:) (x: rational) (y: rational): bool =
  raise NotImplemented

(** Checks whether a rational number is strictly greater than another. *)
let (>:) (x: rational) (y: rational): bool =
  raise NotImplemented

(** Checks whether a rational number is greater than or equal to another. *)
let (>=:) (x: rational) (y: rational): bool =
  raise NotImplemented

let string_of_rational ((n, d): rational): string =
  if d = 1 then string_of_int n
  else sprintf "%d/%d" n d

let eval_rational (e: expr) (vals: (string * rational) list): rational =
  raise NotImplemented
