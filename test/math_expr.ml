open OUnit2
open Math_expr
open Printf

(* Helper functions ------------------------------------------------------------------------------------------------- *)
(* Calculates the depth of an expression tree *)
let rec depth (e: expr): int =
  match e with
  | Z _ -> 0
  | Add es
  | Sub es
  | Mul es -> 1 + (List.fold_left (fun acc e -> max acc (depth e)) 0 es)
  | Div (e1, e2) -> 1 + max (depth e1) (depth e2)

(* tabulate origin dest returns the list [origin; origin + 1; ...; dest - 1; dest] *)
let rec tabulate (origin: int) (dest: int): int list =
  if origin = dest then [dest]
  else origin :: (tabulate (origin + 1) dest)

(* next_rand: exceptions -------------------------------------------------------------------------------------------- *)
(* TODO *)

(* next_rand: values ------------------------------------------------------------------------------------------------ *)
let min_depth = 1
let max_depth = 3
let width = 3
let min_const = -2
let max_const = 3
let (random_exprs: (int * expr) list) =
  let generate_rand_expr s =
    seed s;
    (s, next_rand min_depth max_depth width min_const max_const)
  in
  List.map generate_rand_expr (tabulate 0 100)
let (random_consts: int list) =
  let rec get_consts e = 
    match e with
    | Z (n) -> [n]
    | Add es
    | Sub es 
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

(* TODO: Check width *)

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
  
(* string_of_expr: exceptions --------------------------------------------------------------------------------------- *)
(* TODO *)

(* string_of_expr --------------------------------------------------------------------------------------------------- *)
let string_of_expr_test0 _ =
  assert_equal
    ~printer: (fun x -> x)
    "1"
    (string_of_expr (Z 1))

let string_of_expr_test1 _ =
  assert_equal
    ~printer: (fun x -> x)
    "(-1)"
    (string_of_expr (Z (-1)))

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "math_expr_tests">::: [
    "next_rand_min_depth">:: next_rand_min_depth;
    "next_rand_max_depth">:: next_rand_max_depth;
    "next_rand_all_depths">:: next_rand_all_depths;
    "next_rand_min_const">:: next_rand_min_const;
    "next_rand_max_const">:: next_rand_max_const;
    "next_rand_all_consts">:: next_rand_all_consts;
    "next_rand_todo">:: (fun _ -> todo "Write next_rand tests.");
    "string_of_expr_test0">:: string_of_expr_test0;
    "string_of_expr_test1">:: string_of_expr_test1;
    "string_of_expr_todo">:: (fun _ -> todo "Write string_of_expr tests.");
  ]

let () =
  (* List.iter (fun (s, e) -> printf "%d: %s\n" s (string_of_expr e)) random_exprs *)
  run_test_tt_main tests
