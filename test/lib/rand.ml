open Dobson.Base
open Dobson.Rand
open Dobson.Rational
open OUnit2
open Printf
open Test_helper

(* next_fractional: values ------------------------------------------------------------------------------------------ *)
let min_depth = 1
let max_depth = 3
let width = 3
let min_const = new_rational (-2) 1
let max_const = new_rational 3 1
let expected_max_denom = 10
let (random_exprs: (int * expr) list) =
  let generate_rand_expr s =
    seed s;
    (s, next_fractional min_depth max_depth width min_const max_const expected_max_denom)
  in
  List.map generate_rand_expr (tabulate 0 100)
let (random_consts: rational list) =
  let rec get_consts e = 
    match e with
    | Z (n) -> [new_rational n 1]
    | Div (Z n, Z d) -> [new_rational n d]
    | R x -> raise (NonRational (sprintf "%.12g" x))
    | Var _ -> []
    | Neg e -> get_consts e
    | Add es
    | Mul es -> List.fold_left (fun acc arg -> acc @ (get_consts arg)) [] es
    | Div (e1, e2) -> (get_consts e1) @ (get_consts e2)
  in
  List.fold_left (fun acc e -> acc @ (get_consts e)) [] (List.map (fun (_, e) -> e) random_exprs)

let next_fractional_min_depth _ =
  List.iter (fun (s, e) -> assert_expr_min_depth min_depth s e) random_exprs

let next_fractional_max_depth _ =
  List.iter (fun (s, e) -> assert_expr_max_depth max_depth s e) random_exprs

let next_fractional_all_depths _ =
  assert_expr_all_depths (tabulate min_depth max_depth) random_exprs

let next_fractional_max_width _ =
  List.iter (fun (s, e) -> assert_expr_max_width width s e) random_exprs

let next_fractional_min_const _ =
  let min_rational acc r = if r <: acc then r else acc in
  (* Assume there is at least 1 constant in the list. *)
  let actual_min = List.fold_left min_rational (List.hd random_consts) (List.tl random_consts) in
  assert_at_least ~geq: (>=:) ~printer: string_of_rational min_const actual_min

let next_fractional_max_const _ =
  let max_rational acc r = if r <: acc then acc else r in
  (* Assume there is at least 1 constant in the list. *)
  let actual_max = List.fold_left max_rational (List.hd random_consts) (List.tl random_consts) in
  assert_at_most ~leq: (<=:) ~printer: string_of_rational max_const actual_max

let next_fractional_max_denom _ =
  let actual_max_denom = List.fold_left (fun acc (_, e) -> max acc (max_denom e)) 0 random_exprs in
  assert_at_most ~printer: string_of_int expected_max_denom actual_max_denom

let next_fractional_not_simplified _ =
  List.iter (fun (s, e) -> assert_expr_unsimplified s e) random_exprs

(* Check that all expressions can be evaluated to a rational number. This should ensure there is no division by zero,
 * no variables, no non-rational parts, etc.
 *)
let next_fractional_eval_rational0 _ =
  let assert_evaluable (_, e) = let _ = eval_rational e [] in () in
  List.iter assert_evaluable random_exprs

(* Check that all expressions can be evaluated to a rational number in the case where the only allowed constant is 0.
 * Same purpose as the previous test, but with particular emphasis on division by zero.
 *)
let next_fractional_eval_rational1 _ =
  let generate_expr s = seed s; next_fractional 2 2 2 (new_rational 0 1) (new_rational 1 2) 2 in
  let random_exprs = List.map generate_expr (tabulate 0 100) in
  let assert_evaluable e = let _ = eval_rational e [] in () in
  List.iter assert_evaluable random_exprs

(* next_fractional: exceptions -------------------------------------------------------------------------------------- *)
let zero_r = new_rational 0 1
let one_r = new_rational 1 1

let next_fractional_exc_min_depth_invalid _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression cannot be negative.")
    (fun () -> next_fractional (-1) 3 2 zero_r one_r 2)

let next_fractional_exc_max_depth_invalid _ =
  assert_raises
    (Invalid_argument "Maximum depth of expression cannot be negative.")
    (fun () -> next_fractional 0 (-1) 2 zero_r one_r 2)

let next_fractional_exc_min_depth_greater_than_max_depth _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
    (fun () -> next_fractional 1 0 2 zero_r one_r 2)

let next_fractional_exc_width_invalid _ =
  assert_raises
    (Invalid_argument "Width of expression must be at least 2.")
    (fun () -> next_fractional 0 0 1 zero_r one_r 2)

let next_fractional_exc_min_const_equal_max_const _ =
  assert_raises
    (Invalid_argument "Minimum constant must be less than maximum constant.")
    (fun () -> next_fractional 0 0 2 zero_r zero_r 2)

let next_fractional_exc_no_possible_const _ =
  assert_raises
    (Invalid_argument "No constants satisfy the given conditions (>= 1/10, < 1/5, denominator <= 2).")
    (fun () -> next_fractional 0 0 2 (new_rational 1 10) (new_rational 2 10) 2)

let next_fractional_exc_max_denom_too_small _ =
  assert_raises
    (Invalid_argument "Maximum denominator must be at least 1.")
    (fun () -> next_fractional 0 0 2 zero_r one_r 0)

(* next_decimal: values --------------------------------------------------------------------------------------------- *)
let min_depth = 1
let max_depth = 3
let width = 3
let min_const = -2.0
let max_const = 3.0
let expected_max_decimal_places = 3
let (random_exprs: (int * expr) list) =
  let generate_rand_expr s =
    seed s;
    (s, next_decimal min_depth max_depth width min_const max_const expected_max_decimal_places)
  in
  List.map generate_rand_expr (tabulate 0 100)
let (random_consts: float list) =
  let rec get_consts e = 
    match e with
    | Z n -> [float_of_int n]
    | R x -> [x]
    | Var _ -> []
    | Neg e -> get_consts e
    | Add es
    | Mul es -> List.fold_left (fun acc arg -> acc @ (get_consts arg)) [] es
    | Div (e1, e2) -> (get_consts e1) @ (get_consts e2)
  in
  List.fold_left (fun acc e -> acc @ (get_consts e)) [] (List.map (fun (_, e) -> e) random_exprs)

let next_decimal_min_depth _ =
  List.iter (fun (s, e) -> assert_expr_min_depth min_depth s e) random_exprs

let next_decimal_max_depth _ =
  List.iter (fun (s, e) -> assert_expr_max_depth max_depth s e) random_exprs

let next_decimal_all_depths _ =
  assert_expr_all_depths (tabulate min_depth max_depth) random_exprs

let next_decimal_max_width _ =
  List.iter (fun (s, e) -> assert_expr_max_width width s e) random_exprs

let next_decimal_min_const _ =
  (* Assume there is at least 1 constant in the list. *)
  let actual_min = List.fold_left min (List.hd random_consts) (List.tl random_consts) in
  assert_at_least ~printer: string_of_float min_const actual_min

let next_decimal_max_const _ =
  (* Assume there is at least 1 constant in the list. *)
  let actual_max = List.fold_left max (List.hd random_consts) (List.tl random_consts) in
  assert_at_most ~printer: string_of_float max_const actual_max

let next_decimal_max_decimal_places _ =
  List.iter (fun (_, e) -> assert_max_decimal_places_expr expected_max_decimal_places e) random_exprs

let next_decimal_not_simplified _ =
  List.iter (fun (s, e) -> assert_expr_unsimplified s e) random_exprs

(* Check that all expressions can be evaluated to a finite real number. This should ensure there is no division by zero,
 * no variables, etc.
 *)
let next_decimal_eval0 _ =
  List.iter (fun (_, e) -> assert_finite_number (eval e [])) random_exprs

(* Check that all expressions can be evaluated to a finite real number in the case where the only allowed constant is
 * 0. Same purpose as the previous test, but with particular emphasis on division by zero.
 *)
let next_decimal_eval1 _ =
  let generate_expr s = seed s; next_decimal 2 2 2 0.0 0.1 1 in
  let random_exprs = List.map generate_expr (tabulate 0 100) in
  List.iter (fun e -> assert_finite_number (eval e [])) random_exprs

(* next_decimal: exceptions ----------------------------------------------------------------------------------------- *)
let next_decimal_exc_min_depth_invalid _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression cannot be negative.")
    (fun () -> next_decimal (-1) 3 2 0.0 1.0 1)

let next_decimal_exc_max_depth_invalid _ =
  assert_raises
    (Invalid_argument "Maximum depth of expression cannot be negative.")
    (fun () -> next_decimal 0 (-1) 2 0.0 1.0 1)

let next_decimal_exc_min_depth_greater_than_max_depth _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
    (fun () -> next_decimal 1 0 2 0.0 1.0 1)

let next_decimal_exc_width_invalid _ =
  assert_raises
    (Invalid_argument "Width of expression must be at least 2.")
    (fun () -> next_decimal 0 0 1 0.0 1.0 1)

let next_decimal_exc_min_const_equal_max_const _ =
  assert_raises
    (Invalid_argument "Minimum constant must be less than maximum constant.")
    (fun () -> next_decimal 0 0 2 0.0 0.0 1)

let next_decimal_exc_no_possible_const _ =
  assert_raises
    (Invalid_argument "No constants satisfy the given conditions (>= 0.1001, < 0.1002, <= 3 decimal places).")
    (fun () -> next_decimal 0 0 2 0.1001 0.1002 3)

let next_decimal_exc_decimal_places_too_small _ =
  assert_raises
    (Invalid_argument "Maximum number of decimal places must be at least 0.")
    (fun () -> next_decimal 0 0 2 0.0 1.0 (-1))

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "rand_tests">::: [
    "next_fractional_min_depth">:: next_fractional_min_depth;
    "next_fractional_max_depth">:: next_fractional_max_depth;
    "next_fractional_all_depths">:: next_fractional_all_depths;
    "next_fractional_max_width">:: next_fractional_max_width;
    "next_fractional_min_const">:: next_fractional_min_const;
    "next_fractional_max_const">:: next_fractional_max_const;
    "next_fractional_max_denom">:: next_fractional_max_denom;
    "next_fractional_not_simplified">:: next_fractional_not_simplified;
    "next_fractional_eval_rational0">:: next_fractional_eval_rational0;
    "next_fractional_eval_rational1">:: next_fractional_eval_rational1;
    "next_fractional_exc_min_depth_invalid">:: next_fractional_exc_min_depth_invalid;
    "next_fractional_exc_max_depth_invalid">:: next_fractional_exc_max_depth_invalid;
    "next_fractional_exc_min_depth_greater_than_max_depth">:: next_fractional_exc_min_depth_greater_than_max_depth;
    "next_fractional_exc_width_invalid">:: next_fractional_exc_width_invalid;
    "next_fractional_exc_min_const_equal_max_const">:: next_fractional_exc_min_const_equal_max_const;
    "next_fractional_exc_no_possible_const">:: next_fractional_exc_no_possible_const;
    "next_fractional_exc_max_denom_too_small">:: next_fractional_exc_max_denom_too_small;
    "next_decimal_min_depth">:: next_decimal_min_depth;
    "next_decimal_max_depth">:: next_decimal_max_depth;
    "next_decimal_all_depths">:: next_decimal_all_depths;
    "next_decimal_max_width">:: next_decimal_max_width;
    "next_decimal_min_const">:: next_decimal_min_const;
    "next_decimal_max_const">:: next_decimal_max_const;
    "next_decimal_max_decimal_places">:: next_decimal_max_decimal_places;
    "next_decimal_not_simplified">:: next_decimal_not_simplified;
    "next_decimal_eval0">:: next_decimal_eval0;
    "next_decimal_eval1">:: next_decimal_eval1;
    "next_decimal_exc_min_depth_invalid">:: next_decimal_exc_min_depth_invalid;
    "next_decimal_exc_max_depth_invalid">:: next_decimal_exc_max_depth_invalid;
    "next_decimal_exc_min_depth_greater_than_max_depth">:: next_decimal_exc_min_depth_greater_than_max_depth;
    "next_decimal_exc_width_invalid">:: next_decimal_exc_width_invalid;
    "next_decimal_exc_min_const_equal_max_const">:: next_decimal_exc_min_const_equal_max_const;
    "next_decimal_exc_no_possible_const">:: next_decimal_exc_no_possible_const;
    "next_decimal_exc_decimal_places_too_small">:: next_decimal_exc_decimal_places_too_small;
  ]

let () =
  run_test_tt_main tests
