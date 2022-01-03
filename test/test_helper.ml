open Dobson.Base
open Dobson.Rational
open OUnit2
open Printf

(* Calculates the depth of an expression tree. *)
(* TODO: Add option so that expressions of the form Div (Z _, Z _) have depth 1? *)
let rec depth (e: expr): int =
  match e with
  | Z _
  | Div (Z _, Z _)
  | Var _ -> 0
  | Neg e' -> depth e'
  | Add es
  | Mul es -> 1 + (List.fold_left (fun acc e -> max acc (depth e)) 0 es)
  | Div (e1, e2) -> 1 + max (depth e1) (depth e2)

let rec max_width (e: expr): int =
  match e with
  | Z _
  | Var _ -> 1
  | Neg e' -> max_width e'
  | Add es
  | Mul es -> List.fold_left (fun acc e -> max acc (max_width e)) (List.length es) es
  | Div (e1, e2) -> max 2 (max (max_width e1) (max_width e2))

let rec max_denom (e: expr): int =
  match e with
  | Z _ -> 1
  | Var _ -> 0
  | Div (Z _, Z d) -> d
  | Neg e' -> max_denom e'
  | Add es
  | Mul es -> List.fold_left (fun acc e -> max acc (max_denom e)) 0 es
  | Div (e1, e2) -> max (max_denom e1) (max_denom e2)

(* Checks whether the given expression involves division by an expression that is identical to 0. *)
let rec has_div_by_zero (e: expr): bool =
  match e with
  | Z _
  | Var _ -> false
  | Neg e -> has_div_by_zero e
  | Add es
  | Mul es -> List.exists (fun arg -> has_div_by_zero arg) es
  | Div (e1, e2) -> has_div_by_zero e1 || has_div_by_zero e2 || (try eval e2 [] = 0.0 with UndefinedVariable _ -> false)

(* Computes the greatest common divisor of n and m. *)
let gcd (n: int) (m: int): int =
  let rec gcd' big small =
    if small = 0 then big
    else gcd' small (big mod small)
  in
  let (n', m') = (abs n, abs m) in
  gcd' (max n' m') (min n' m')

(* Checks whether the given fraction is already reduced. *)
let is_reduced_fraction ((n: int), (d: int)): bool =
  1 = gcd n d

let is_simplified (e: expr): bool =
  match e with
  | Z _
  | Var _ -> true
  | Div (Z n, Z d) when is_reduced_fraction (n, d) -> true
  | Neg (Z n) when n >= 0 -> true
  | Neg _
  | Add _
  | Mul _
  | Div _ -> false

(* tabulate origin dest returns the list [origin; origin + 1; ...; dest - 1; dest] *)
let rec tabulate (origin: int) (dest: int): int list =
  if origin = dest then [dest]
  else origin :: (tabulate (origin + 1) dest)

(* Returns a list containing n elements, where each element is x. *)
let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

let assert_equal_string = assert_equal ~printer: (fun x -> x)

let assert_equal_int = assert_equal ~printer: string_of_int

let assert_equal_float = assert_equal ~printer: string_of_float ~cmp: (cmp_float ~epsilon: 1e-15)

let assert_equal_expr = assert_equal ~printer: string_of_expr

let assert_equal_rational = assert_equal ~printer: string_of_rational ~cmp: (=:)

let assert_at_least ?(geq: 'a -> 'a -> bool = (>=)) ?(msg: string option) ?(printer: ('a -> string) option) 
    (expected: 'a) (actual: 'a): unit =
  if geq actual expected then
    ()
  else 
    let msg' = match msg with
      | None ->
          (match printer with
          | None -> "Value less than required."
          | Some p -> sprintf "Expected value to be at least %s but received %s." (p expected) (p actual))
      | Some str -> str
    in
    assert_failure msg'

let assert_at_most ?(leq: 'a -> 'a -> bool = (<=)) ?(msg: string option) ?(printer: ('a -> string) option) 
    (expected: 'a) (actual: 'a): unit =
  if leq actual expected then
    () 
  else
    let msg' = match msg with
      | None ->
          (match printer with
          | None -> "Value greater than required."
          | Some p -> sprintf "Expected value to be at most %s but received %s." (p expected) (p actual))
      | Some str -> str
    in
    assert_failure msg'

let assert_expr_min_depth (expected_depth: int) (seed: int) (e: expr): unit =
  let actual_depth = depth e in
  let msg = sprintf
    "Expected minimum depth to be %d but found expression with depth %d:\n\
     - seed: %d\n\
     - expression: %s"
    expected_depth actual_depth seed (string_of_expr e)
  in
  assert_at_least ~msg: msg ~printer: string_of_int expected_depth actual_depth

let assert_expr_max_depth (expected_depth: int) (seed: int) (e: expr): unit =
  let actual_depth = depth e in
  let msg = sprintf
    "Expected maximum depth to be %d but found expression with depth %d:\n\
     - seed: %d\n\
     - expression: %s"
    expected_depth actual_depth seed (string_of_expr e)
  in
  assert_at_most ~msg: msg ~printer: string_of_int expected_depth actual_depth

let assert_expr_all_depths (expected_depths: int list) (es: (int * expr) list): unit =
  let actual_depths = List.map (fun (_, e) -> depth e) es in
  let f d =
    if List.mem d actual_depths then ()
    else assert_failure (sprintf "Missing expression with depth %d." d)
  in
  List.iter f expected_depths

let assert_expr_max_width (expected_max_width: int) (seed: int) (e: expr): unit =
  let actual_max_width = max_width e in
  let msg = sprintf
    "Expected maximum width to be at most %d but found expression with maximum width %d:\n\
     - seed: %d\n\
     - expression: %s"
    expected_max_width actual_max_width seed (string_of_expr e)
  in
  assert_at_most ~msg: msg ~printer: string_of_int expected_max_width actual_max_width

let assert_no_div_by_zero (seed: int) (e: expr): unit =
  let msg = sprintf
    "Found expression with division by zero:\n\
     - seed: %d\n\
     - expression: %s"
    seed (string_of_expr e)
  in
  assert_bool
    msg
    (not (has_div_by_zero e))

let assert_expr_unsimplified (seed: int) (e: expr): unit =
  let msg = sprintf
    "Found expression that is already simplified:\n\
     - seed: %d\n\
     - expression: %s"
    seed (string_of_expr e)
  in
  assert_bool
    msg
    (not (is_simplified e))
