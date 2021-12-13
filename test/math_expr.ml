open OUnit2
open Math_expr

let id x = x

(* next_rand ---------------------------------------------------------------- *)

(* string_of_expr ----------------------------------------------------------- *)
let string_of_expr_test0 _ =
  assert_equal
    ~printer: id
    "1"
    (string_of_expr (Z 1))

let string_of_expr_test1 _ =
  assert_equal
    ~printer: id
    "(-1)"
    (string_of_expr (Z (-1)))

(* List and run tests ------------------------------------------------------- *)
let tests =
  "math_expr_tests">::: [
    "next_rand_todo">:: (fun _ -> todo "Write next_rand tests.");
    "string_of_expr_test0">:: string_of_expr_test0;
    "string_of_expr_test1">:: string_of_expr_test1;
    "string_of_expr_todo">:: (fun _ -> todo "Write string_of_expr tests.");
  ]

let () =
  run_test_tt_main tests
