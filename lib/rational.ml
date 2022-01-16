open Printf

type rational = int * int

exception Root_negative of int
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

(* Generates a function which will calculate p^n. *)
let pow (n: int): int -> int =
  let rec powGen (n: int) (acc: int -> int) =
    if n = 0 then acc
    else powGen (n - 1) (fun p -> p * (acc p))
  in
  powGen n (fun _ -> 1)

let string_of_ordinal (n: int): string =
  if n mod 10 = 1 && n <> 11 then sprintf "%dst" n
  else if n mod 10 = 2 && n <> 12 then sprintf "%dnd" n
  else if n mod 10 = 3 && n <> 13 then sprintf "%drd" n
  else sprintf "%dth" n

(* Finds the nth root of p, provided the result exists and is an integer. *)
let nth_root (n: int) (p: int): int =
  let nth_pow = pow n in
  let ap = abs p in
  (* Tries to find the nth root of p using binary search *)
  let rec find_root low high =
    if low > high then raise (NonRational (sprintf "%s root of %d" (string_of_ordinal n) p))
    else
      let mid = (high + low) / 2 in
      let result = nth_pow mid in
      if result = ap then mid
      else if result > p then find_root low (mid - 1)
      else find_root (mid + 1) high
  in
  if p = 0 then 0
  else if p < 0 && n mod 2 = 0 then raise (Root_negative p)
  else if p < 0 then -(find_root 1 (-p))
  else find_root 1 p

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

let rec (^:) (((n1, d1) as base): rational) (((n2, d2) as power): rational): rational =
  if n2 < 0 then  (* All functions return positive denominator, so checking the numerator is enough. *)
    (* x^-n = (1/x)^n *)
    (new_rational 1 1 /: base) ^: ~-:power
  else
    (* (n1/d1)^(n2/d2) = n1^(n2/d2) / d1^(n2/d2) 
       and x^(a/b) = (x^(1/b))^a *)
    let n = pow n2 (nth_root d2 n1) in
    let d = pow n2 (nth_root d2 d1) in
    new_rational n d

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
