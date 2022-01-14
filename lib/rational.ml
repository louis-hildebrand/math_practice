open Printf

type rational = int * int

exception NonRational of string

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
(* NOTE: Every function that returns a fraction must simplify it as much as possible. *)

let new_rational (numerator: int) (denominator: int): rational =
  if denominator = 0 then
    raise Division_by_zero
  else
    let (n', d') = reduce_fraction (numerator, denominator) in
    (n', d')

let float_of_rational ((n, d): rational): float =
  (float_of_int n) /. (float_of_int d)

let split ((n, d): rational): int * int =
  (n, d)

(* If overflow ever becomes a problem for the operators, try finding the LCM of the denominators instead. *)

let (+:) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2) *)
  (* Assume both denominators are nonzero. *)
  reduce_fraction (n1*d2 + n2*d1, d1*d2)

let (-:) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* n1/d1 - n2/d2 = (n1*d2 - n2*d1)/(d1*d2) *)
  (* Assume both denominators are nonzero. *)
  reduce_fraction (n1*d2 - n2*d1, d1*d2)

let (~-:) (x: rational): rational =
  (new_rational 0 1) -: x

let ( *: ) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* n1/d1 * n2/d2 = (n1*n2) / (d1*d2) *)
  (* Assume both denominators are nonzero. *)
  reduce_fraction (n1 * n2, d1 * d2)

let (/:) ((n1, d1): rational) ((n2, d2): rational): rational =
  (* (n1/d1) / (n2/d2) = (n1*d2) / (d1*n2) *)
  (* Assume both denominators are nonzero. *)
  let n' = n1 * d2 in
  let d' = d1 * n2 in
  if d' = 0 then raise Division_by_zero
  else new_rational n' d'

let (=:) (x: rational) (y: rational): bool =
  (* Assume the fractions are already reduced *)
  x = y

let (<>:) (x: rational) (y: rational): bool =
  not (x =: y)

let (<:) ((n1, d1): rational) ((n2, d2): rational): bool =
  n1*d2 < n2*d1

let (<=:) ((n1, d1): rational) ((n2, d2): rational): bool =
  n1*d2 <= n2*d1

let (>:) ((n1, d1): rational) ((n2, d2): rational): bool =
  n1*d2 > n2*d1

let (>=:) ((n1, d1): rational) ((n2, d2): rational): bool =
  n1*d2 >= n2*d1

let string_of_rational ((n, d): rational): string =
  if d = 1 then string_of_int n
  else sprintf "%d/%d" n d
