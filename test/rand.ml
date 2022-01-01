open Dobson.Base
open Dobson.Rand
open OUnit2
open Printf
open Test_helper

(* next_fractional: values ------------------------------------------------------------------------------------------ *)
let min_depth = 1
let max_depth = 3
let width = 3
let min_const = -2
let max_const = 3
let (random_exprs: (int * expr) list) =
  let generate_rand_expr s =
    seed s;
    (s, next_fractional min_depth max_depth width min_const max_const)
  in
  List.map generate_rand_expr (tabulate 0 100)
let (random_consts: int list) =
  let rec get_consts e = 
    match e with
    | Z (n) -> [n]
    | Var _ -> []
    | Neg e -> get_consts e
    | Add es
    | Mul es -> List.fold_left (fun acc arg -> acc @ (get_consts arg)) [] es
    | Div (e1, e2) -> (get_consts e1) @ (get_consts e2)
  in
  List.fold_left (fun acc e -> acc @ (get_consts e)) [] (List.map (fun (_, e) -> e) random_exprs)

let next_rand_min_depth _ =
  let shallow_exprs = List.filter (fun (_, e) -> (depth e) < min_depth) random_exprs in
  match shallow_exprs with
  | [] -> ()
  | (s, e)::_ -> assert_failure
      (sprintf "Found shallow expression:\n \
                 - seed: %d\n \
                 - expression: %s\n \
                 - expected min. depth: %d\n \
                 - actual depth: %d"
                s (string_of_expr e) min_depth (depth e))

let next_rand_max_depth _ =
  let deep_exprs = List.filter (fun (_, e) -> (depth e) > max_depth) random_exprs in
  match deep_exprs with
  | [] -> ()
  | (s, e)::_ -> assert_failure
      (sprintf "Found expression that is too deep:\n \
                 - seed: %d\n \
                 - expression: %s\n \
                 - expected max. depth: %d\n \
                 - actual depth: %d"
               s (string_of_expr e) max_depth (depth e))

let next_rand_all_depths _ =
  (* Asserts a failure if there are no expressions with depth n *)
  let f n =
    let exprs_with_depth = List.filter (fun (_, e) -> n = (depth e)) random_exprs in
    match exprs_with_depth with
    | [] -> assert_failure (sprintf "Missing expression with depth %d." n)
    | _ -> ()
  in
  List.iter f (tabulate min_depth max_depth)

let next_rand_max_width _ =
  (* Asserts a failure if there is any operation in e with more than w arguments *)
  let rec f w e =
    match e with
    | Z _
    | Var _
    | Neg _ -> ()
    | Add es
    | Mul es ->
        if List.length es > w then 
          assert_failure (sprintf "Found expression that is too wide:\n \
             - expression: %s\n \
             - expected max. width: %d\n \
             - actual width: %d"
            (string_of_expr e) width (List.length es))
        else
          List.iter (f w) es
    | Div (e1, e2) -> f w e1; f w e2
  in
  List.iter (fun (_, e) -> f width e) random_exprs

let next_rand_min_const _ =
  let below_min_exprs = List.filter (fun n -> n < min_const) random_consts in
  match below_min_exprs with
  | [] -> ()
  | n::_ -> assert_failure 
      (sprintf "Found expression containing constant %d. Expected min. constant to be %d." n min_const)

let next_rand_max_const _ =
  let below_max_exprs = List.filter (fun n -> n >= max_const) random_consts in
  match below_max_exprs with
  | [] -> ()
  | n::_ -> assert_failure 
      (sprintf "Found expression containing constant %d. Expected max. constant to be %d (exclusive)." n max_const)

let next_rand_all_consts _ =
  let f n =
    if List.mem n random_consts then ()
    else assert_failure (sprintf "Missing expression with constant %d." n)
  in
  List.iter f (tabulate min_const (max_const - 1))

let next_rand_no_div_by_zero1 _ =
  let f (_, e) =
    if has_div_by_zero e then assert_failure (sprintf "Found expression with division by zero: %s." (string_of_expr e))
    else ()
  in
  List.iter f random_exprs

let next_rand_no_div_by_zero2 _ =
  let random_exprs = List.map (fun () -> next_fractional 2 2 2 0 1) (repeat () 100) in
  let f e =
    if has_div_by_zero e then assert_failure (sprintf "Found expression with division by zero: %s." (string_of_expr e))
    else ()
  in
  List.iter f random_exprs

let next_rand_no_div_by_zero3 _ =
  let random_exprs = List.map (fun () -> next_fractional 2 2 2 0 2) (repeat () 100) in
  let f e =
    if has_div_by_zero e then assert_failure (sprintf "Found expression with division by zero: %s." (string_of_expr e))
    else ()
  in
  List.iter f random_exprs

(* next_fractional: exceptions -------------------------------------------------------------------------------------- *)
let next_rand_exc_min_depth_invalid _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression cannot be negative.")
    (fun () -> next_fractional (-1) 3 2 0 1)

let next_rand_exc_max_depth_invalid _ =
  assert_raises
    (Invalid_argument "Maximum depth of expression cannot be negative.")
    (fun () -> next_fractional 0 (-1) 2 0 1)

let next_rand_exc_min_depth_greater_than_max_depth _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
    (fun () -> next_fractional 1 0 2 0 1)

let next_rand_exc_width_invalid _ =
  assert_raises
    (Invalid_argument "Width of expression must be at least 2.")
    (fun () -> next_fractional 0 0 1 0 1)

let next_rand_exc_min_const_equal_max_const _ =
  assert_raises
    (Invalid_argument "Minimum constant must be less than maximum constant.")
    (fun () -> next_fractional 0 0 2 0 0)

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "rand_tests">::: [
    "next_rand_min_depth">:: next_rand_min_depth;
    "next_rand_max_depth">:: next_rand_max_depth;
    "next_rand_all_depths">:: next_rand_all_depths;
    "next_rand_max_width">:: next_rand_max_width;
    "next_rand_min_const">:: next_rand_min_const;
    "next_rand_max_const">:: next_rand_max_const;
    "next_rand_all_consts">:: next_rand_all_consts;
    "next_rand_no_div_by_zero1">:: next_rand_no_div_by_zero1;
    "next_rand_no_div_by_zero2">:: next_rand_no_div_by_zero2;
    "next_rand_no_div_by_zero3">:: next_rand_no_div_by_zero3;
    "next_rand_exc_min_depth_invalid">:: next_rand_exc_min_depth_invalid;
    "next_rand_exc_max_depth_invalid">:: next_rand_exc_max_depth_invalid;
    "next_rand_exc_min_depth_greater_than_max_depth">:: next_rand_exc_min_depth_greater_than_max_depth;
    "next_rand_exc_width_invalid">:: next_rand_exc_width_invalid;
    "next_rand_exc_min_const_equal_max_const">:: next_rand_exc_min_const_equal_max_const;
  ]

let () =
  run_test_tt_main tests
