open Base
open Rational
open Printf

type operation =
  | ONeg
  | OAdd
  | OMul
  | ODiv

type constant =
  | Frac
  | Decimal

type element = Op of operation | Const of constant

exception Internal of string

(* Helper functions *)
let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

let get_rand_from_list (options: 'a list): 'a =
  let i = Random.int (List.length options) in
  List.nth options i

let expr_of_rational (r: rational): expr =
  let (n, d) = split r in
  if d = 1 then Z n
  else Div (Z n, Z d)

(* Public functions *)
let init: unit -> unit =
  Random.self_init

let seed (n: int): unit =
  Random.init n

(* Decides what the next element in the expression should be.
 * Ensures that the minimum and maximum depths are respected and disallows division if all constants will be zero.
 *)
let choose_next_element (min_depth: int) (max_depth: int) (only_zero: bool) (allowed_ops: operation list) 
    (allowed_consts: constant list): element =
  (* Disallow division if the only allowed constant is 0 *)
  let allowed_ops = if only_zero then List.filter (fun o -> o != ODiv) allowed_ops else allowed_ops in
  (* Disallow operations if the maximum depth has been reached *)
  let allowed = if max_depth = 0 then
    List.map (fun c -> Const c) allowed_consts
  (* Disallow constants until minimum depth has been reached *)
  else if min_depth > 0 then
    List.map (fun o -> Op o) allowed_ops
  else
    (List.map (fun o -> Op o) allowed_ops) @ (List.map (fun c -> Const c) allowed_consts)
  in
  get_rand_from_list allowed

(* Returns the numerator of the first rational number greater than or equal to min_val and with denominator d. *)
let numerator_of_first_with_denom (min_val: rational) (d: int): int =
  let numerator_float = float_of_rational (min_val *: (new_rational d 1)) in
  truncate (ceil numerator_float)

(* Returns the first rational number greater than or equal to min_val and with denominator d. *)
let first_with_denom (min_val: rational) (d: int): rational =
      let numerator = numerator_of_first_with_denom min_val d in
      new_rational numerator d

(* Generates a random rational number. *)
let get_random_rational (min_val: rational) (max_val: rational) (max_denom: int): rational =
  let rec get_allowed n d acc =
    if d > max_denom then acc
    else if (new_rational n d) >=: max_val then get_allowed (numerator_of_first_with_denom min_val (d + 1)) (d + 1) acc
    else let acc' = (new_rational n d) :: acc in get_allowed (n + 1) d acc'
  in
  (* First int greater than or equal to min_val *)
  let minimum_int = numerator_of_first_with_denom min_val 1 in
  let allowed = get_allowed minimum_int 1 [] in
  get_rand_from_list allowed

(* Checks whether there exists a rational number that satisfies the given conditions.
 * - min_val: Minimum value (inclusive)
 * - max_val: Maximum value (exclusive)
 * - max_denom: Maximum value of the denominator (inclusive)
 * - ignore: Numbers to ignore (i.e. consider invalid)
 *)
let exists_rational (min_val: rational) (max_val: rational) (max_denom: int) (ignore: rational list): bool =
  if min_val >=: max_val then raise (Invalid_argument "Minimum constant must be less than maximum constant.")
  else if max_denom < 1 then raise (Invalid_argument "Maximum denominator must be at least 1.")
  else
    (* Checks if there exists any valid fraction with denominator >= d. *)
    let rec exists_rational' d =
      if d > max_denom then false
      else
        (* Checks if there exists any valid fraction greater than or equal to r and having denominator d. *)
        let rec check_val r =
          if r >=: max_val then false
          else if List.mem r ignore then check_val (r +: new_rational 1 d)
          else true
        in
        let first = first_with_denom min_val d in
        check_val first || exists_rational' (d + 1)
    in
    exists_rational' 1

let next_fractional (min_depth: int) (max_depth: int) (width: int) (min_const: rational) (max_const: rational)
    (max_denom: int): expr =
  if min_depth < 0 then
    raise (Invalid_argument "Minimum depth of expression cannot be negative.")
  else if max_depth < 0 then
    raise (Invalid_argument "Maximum depth of expression cannot be negative.")
  else if min_depth > max_depth then
    raise (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
  else if width < 2 then
    raise (Invalid_argument "Width of expression must be at least 2.")
  else if not (exists_rational min_const max_const max_denom []) then
    let msg = sprintf "No constants satisfy the given conditions (>= %s, < %s, denominator <= %d)."
      (string_of_rational min_const) (string_of_rational max_const) max_denom
    in
    raise (Invalid_argument msg)
  else
    let only_zero = not (exists_rational min_const max_const max_denom [new_rational 0 1]) in
    let rec next_fractional' min_depth max_depth =
      let next = choose_next_element min_depth max_depth only_zero [ONeg; OAdd; OMul; ODiv] [Frac] in
      match next with
      | Op ONeg -> Neg (next_fractional' min_depth max_depth)
      | Op OAdd -> Add (get_args min_depth max_depth)
      | Op OMul -> Mul (get_args min_depth max_depth)
      | Op ODiv -> get_division min_depth max_depth
      | Const Frac -> expr_of_rational (get_random_rational min_const max_const max_denom)
      (* Should never happen *)
      | Const Decimal -> raise (Internal "Invalid output from choose_next_element: (Const Decimal).") [@coverage off]
    and get_args min_depth max_depth =
      let (min_depth', max_depth') = (max 0 (min_depth - 1), max_depth - 1) in
      List.map (fun f -> f ()) (repeat (fun () -> next_fractional' min_depth' max_depth') width)
    and get_nonzero min_depth max_depth =
      (* Is there a more efficient way of doing this? *)
      let rec try_generate_nonzero num_attempts =
        if num_attempts >= 1000 then
          (* Should never happen *)
          raise (Internal "Failed to generate nonzero expression.") [@coverage off]
        else
          let e = next_fractional' min_depth max_depth in
          if eval_rational e [] <>: new_rational 0 1 then e
          else try_generate_nonzero (num_attempts + 1)
      in
      try_generate_nonzero 0
    and get_division min_depth max_depth =
      let (min_depth', max_depth') = (max 0 (min_depth - 1), max_depth - 1) in
      let numer = next_fractional' min_depth' max_depth' in
      let denom = get_nonzero min_depth' max_depth' in
      (* TODO: Check for division by 0 or for both being integers *)
      Div (numer, denom)
    in
    next_fractional' min_depth max_depth

let roundi (x: float): int =
  truncate (floor (x +. 0.5))

(* Checks whether there exists a decimal number that satisfies the given conditions.
 * - min_val: Minimum value (inclusive)
 * - max_val: Maximum value (exclusive)
 * - decimal_places: Maximum value of the denominator (inclusive)
 * - ignore: Numbers to ignore (i.e. consider invalid)
 *)
let exists_decimal (min_val: float) (max_val: float) (decimal_places: int) (ignore: float list): bool =
  if min_val >= max_val then raise (Invalid_argument "Minimum constant must be less than maximum constant.")
  else if decimal_places < 0 then raise (Invalid_argument "Maximum number of decimal places must be at least 0.")
  else
    let m = 10.0 ** (float_of_int decimal_places) in
    let min_int = roundi (min_val *. m) in (* Inclusive *)
    let max_int = roundi (max_val *. m) in (* Exclusive *)
    (* Checks whether n or any integer greater than n is valid. *)
    let rec exists_decimal' n =
      if n >= max_int then false
      else if List.mem ((float_of_int n) /. m) ignore then exists_decimal' (n + 1)
      else true
    in
    exists_decimal' min_int

let get_random_decimal (min_val: float) (max_val: float) (decimal_places: int): float =
  let m = 10.0 ** (float_of_int decimal_places) in
  let min_int = roundi (min_val *. m) in (* Inclusive *)
  let max_int = roundi (max_val *. m) in (* Exclusive *)
  let out_int = min_int + Random.int (max_int - min_int) in
  (float_of_int out_int) /. m

let next_decimal (min_depth: int) (max_depth: int) (width: int) (min_const: float) (max_const: float)
    (decimal_places: int): expr =
  if min_depth < 0 then
    raise (Invalid_argument "Minimum depth of expression cannot be negative.")
  else if max_depth < 0 then
    raise (Invalid_argument "Maximum depth of expression cannot be negative.")
  else if min_depth > max_depth then
    raise (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
  else if width < 2 then
    raise (Invalid_argument "Width of expression must be at least 2.")
  else if not (exists_decimal min_const max_const decimal_places []) then
    raise (Invalid_argument (sprintf
      "No constants satisfy the given conditions (>= %g, < %g, <= %d decimal places)."
      min_const max_const decimal_places))
  else
    let only_zero = not (exists_decimal min_const max_const decimal_places [0.0]) in
    let rec next_decimal' min_depth max_depth =
      let next = choose_next_element min_depth max_depth only_zero [ONeg; OAdd; OMul] [Decimal] in
      match next with
      | Op ONeg -> Neg (next_decimal' min_depth max_depth)
      | Op OAdd -> Add (get_args min_depth max_depth)
      | Op OMul -> Mul (get_args min_depth max_depth)
      | Const Decimal -> R (get_random_decimal min_const max_const decimal_places)
      (* Should never happen *)
      | Op ODiv -> raise (Internal "Invalid output from choose_next_element (Op ODiv).") [@coverage off]
      | Const Frac -> raise (Internal "Invalid output from choose_next_element (Const Frac).") [@coverage off]
    and get_args min_depth max_depth =
      let (min_depth', max_depth') = (max 0 (min_depth - 1), max_depth - 1) in
      List.map (fun f -> f ()) (repeat (fun () -> next_decimal' min_depth' max_depth') width)
    in
    next_decimal' min_depth max_depth
