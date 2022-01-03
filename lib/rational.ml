open Base
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
    raise (Undefined 
      (sprintf "Attempt to initialize rational number with denominator of zero (%d/%d)." numerator denominator))
  else
    let (n', d') = reduce_fraction (numerator, denominator) in
    (n', d')

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

let rec eval_rational (e: expr) (vals: (string * rational) list): rational =
  match e with
  | Z n -> new_rational n 1
  | R x -> raise (NonRational (sprintf "Floating-point value %g is not an integer or a fraction." x))
  | Var name -> 
      let vs = List.map (fun (_, v) -> v) (List.filter (fun (n, _) -> n = name) vals) in
      (match vs with
      | [] -> raise (UndefinedVariable (sprintf "No definition provided for variable '%s'." name))
      | [v] -> v
      | v1::v2::_ ->
          raise (MultipleDefinitions (sprintf "Multiple definitions provided for variable '%s' (e.g. %s, %s)."
            name (string_of_rational v1) (string_of_rational v2))))
  | Neg e' -> ~-:(eval_rational e' vals)
  | Add es when List.length es >= 2 ->
      List.fold_left (fun acc e -> acc +: (eval_rational e vals)) (new_rational 0 1) es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Mul es when List.length es >= 2 ->
      List.fold_left (fun acc e -> acc *: (eval_rational e vals)) (new_rational 1 1) es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      let n = eval_rational e1 vals in
      let d = eval_rational e2 vals in
      if d =: (new_rational 0 1) then
        raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
      else
        n /: d
