open Dobson.Base
open Dobson.Rational
open OUnit2

let assert_equal_string = assert_equal ~printer: (fun x -> x)

let assert_equal_float = assert_equal ~printer: string_of_float ~cmp: (cmp_float ~epsilon: 1e-15)

let assert_equal_expr = assert_equal ~printer: string_of_expr

let assert_equal_rational = assert_equal ~printer: string_of_rational ~cmp: (=:)

(* Calculates the depth of an expression tree. *)
let rec depth (e: expr): int =
  match e with
  | Z _
  | R _
  | Var _ -> 0
  | Neg e -> depth e
  | Add es
  | Mul es -> 1 + (List.fold_left (fun acc e -> max acc (depth e)) 0 es)
  | Div (e1, e2) -> 1 + max (depth e1) (depth e2)

(* Checks whether the given expression involves division by an expression that is identical to 0. *)
let rec has_div_by_zero (e: expr): bool =
  match e with
  | Z _
  | R _
  | Var _ -> false
  | Neg e -> has_div_by_zero e
  | Add es
  | Mul es -> List.exists (fun arg -> has_div_by_zero arg) es
  | Div (e1, e2) -> has_div_by_zero e1 || has_div_by_zero e2 || (try eval e2 [] = 0.0 with UndefinedVariable _ -> false)

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
