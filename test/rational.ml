open Dobson.Base
open Dobson.Rational
open OUnit2
open Printf
open Test_helper

(* new_rational: exceptions ----------------------------------------------------------------------------------------- *)
let new_rational_exc_div_by_zero _ =
  assert_raises
    (Undefined "Attempt to initialize rational number with denominator of zero (0/0).")
    (fun () -> new_rational 0 0)

(* string_of_rational: values --------------------------------------------------------------------------------------- *)
let string_of_rational_zero _ =
  assert_equal_string
    "0"
    (string_of_rational (new_rational 0 2))

let string_of_rational_int_negative _ =
  assert_equal_string
    "-2"
    (string_of_rational (new_rational (-4) 2))

let string_of_rational_frac_positive_positive _ =
  assert_equal_string
    "1/2"
    (string_of_rational (new_rational 2 4))

let string_of_rational_frac_positive_negative _ =
  assert_equal_string
    "-1/2"
    (string_of_rational (new_rational 2 (-4)))

let string_of_rational_frac_negative_positive _ =
  assert_equal_string
    "-1/2"
    (string_of_rational (new_rational (-2) 4))

let string_of_rational_frac_negative_negative _ =
  assert_equal_string
    "1/2"
    (string_of_rational (new_rational (-2) (-4)))

(* string_of_rational: exceptions ----------------------------------------------------------------------------------- *)
(* TODO *)

(* eval_rational: values -------------------------------------------------------------------------------------------- *)
let eval_rational_int _ =
  assert_equal_rational
    (new_rational 2 1)
    (eval_rational (Z 2) [])

let eval_rational_var _ =
  assert_equal_rational
    (new_rational 1 2)
    (eval_rational (Var "x") [("x", new_rational 2 4)])

let eval_rational_neg _ =
  assert_equal_rational
    (new_rational (-1) 1)
    (eval_rational (Neg (Z 1)) [])

let eval_rational_div_novars _ =
  assert_equal_rational
    (new_rational 1 3)
    (eval_rational (Div (Z 2, Z 6)) [])

let eval_rational_div_vars _ =
  assert_equal_rational
    (new_rational 5 2)
    (eval_rational (Div (Neg (Var "x"), Neg (Var "y"))) [("x", new_rational 1 3); ("y", new_rational 2 15)])

let eval_rational_add_novars _ =
  assert_equal_rational
    (new_rational 4 5)
    (eval_rational (Add [Div (Z 6, Z 10); Div (Z 3, Z 10); Neg (Div (Z 1, Z 10))]) [])

let eval_rational_add_vars _ =
  assert_equal_rational
    (new_rational 3 2)
    (eval_rational (Add [Div (Z 1, Z 4); Var "x"; Var "y"]) [("x", new_rational 1 4); ("y", new_rational 1 1)])

let eval_rational_mul_novars _ =
  assert_equal_rational
    (new_rational (-3) 5)
    (eval_rational (Mul [Z 2; Div (Z (-1), Z 4); Div (Z 6, Z 5)]) [])

let eval_rational_mul_vars _ =
  assert_equal_rational
    (new_rational 1 1)
    (eval_rational (Mul [Var "x"; Z 3; Neg (Var "y")]) [("x", new_rational 1 2); ("y", new_rational (-2) 3)])

(* eval_rational: exceptions ---------------------------------------------------------------------------------------- *)
let eval_rational_exc_div_by_zero _ =
  assert_raises
    (Undefined (sprintf "Attempt to divide by zero in expression %s."
      (string_of_expr (Div (Z 1, Add [Z 1; Neg (Z 1)])))))
    (fun () -> eval_rational (Div (Z 1, Add [Z 1; Neg (Z 1)])) [])

let eval_rational_exc_add_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> eval_rational (Add [Z 1]) [])

let eval_rational_exc_mul_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> eval_rational (Mul [Z 1]) [])

let eval_rational_exc_unknown_var0 _ =
  assert_raises
    (UndefinedVariable "No definition provided for variable 'y'.")
    (fun () -> eval_rational (Add [Var "x"; Z 1; Var "y"]) [("x", new_rational 1 1)])

let eval_rational_exc_unknown_var1 _ =
  assert_raises
    (MultipleDefinitions "Multiple definitions provided for variable 'y' (e.g. 5/2, 5/2).")
    (fun () -> eval_rational (Add [Var "x"; Z 1; Var "y"]) 
      [("x", new_rational 1 1); ("y", new_rational 5 2); ("y", new_rational 5 2)])

(* equal_rational: values ------------------------------------------------------------------------------------------- *)
let equal_rational_true_zero _ =
  assert_bool
    "Expected 0/1 = 0/12 to be true but received false."
    (new_rational 0 1 =: new_rational 0 12)

let equal_rational_true_positive _ =
  assert_bool
    "Expected 1/10 = 12/120 to be true but received false."
    (new_rational 1 10 =: new_rational 12 120)

let equal_rational_true_negative _ =
  assert_bool
    "Expected -1/10 = 12/(-120) to be true but received false."
    (new_rational (-1) 10 =: new_rational 12 (-120))

let equal_rational_false0 _ =
  assert_bool
    "Expected 1/10 = 0/1 to be false but received true."
    (not (new_rational 1 10 =: new_rational 0 1))

let equal_rational_false1 _ =
  assert_bool
    "Expected -1/10 = 1/10 to be false but received true."
    (not (new_rational (-1) 10 =: new_rational 1 10))

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "rational_tests">::: [
    "new_rational_exc_div_by_zero">:: new_rational_exc_div_by_zero;
    "string_of_rational_zero">:: string_of_rational_zero;
    "string_of_rational_int_negative">:: string_of_rational_int_negative;
    "string_of_rational_frac_positive_positive">:: string_of_rational_frac_positive_positive;
    "string_of_rational_frac_positive_negative">:: string_of_rational_frac_positive_negative;
    "string_of_rational_frac_negative_positive">:: string_of_rational_frac_negative_positive;
    "string_of_rational_frac_negative_negative">:: string_of_rational_frac_negative_negative;
    "eval_rational_int">:: eval_rational_int;
    "eval_rational_var">:: eval_rational_var;
    "eval_rational_neg">:: eval_rational_neg;
    "eval_rational_div_novars">:: eval_rational_div_novars;
    "eval_rational_div_vars">:: eval_rational_div_vars;
    "eval_rational_add_novars">:: eval_rational_add_novars;
    "eval_rational_add_vars">:: eval_rational_add_vars;
    "eval_rational_add_novars">:: eval_rational_add_novars;
    "eval_rational_add_vars">:: eval_rational_add_vars;
    "eval_rational_exc_div_by_zero">:: eval_rational_exc_div_by_zero;
    "eval_rational_exc_add_num_args">:: eval_rational_exc_add_num_args;
    "eval_rational_exc_mul_num_args">:: eval_rational_exc_mul_num_args;
    "eval_rational_exc_unknown_var0">:: eval_rational_exc_unknown_var0;
    "eval_rational_exc_unknown_var1">:: eval_rational_exc_unknown_var1;
    "equal_rational_true_zero">:: equal_rational_true_zero;
    "equal_rational_true_positive">:: equal_rational_true_positive;
    "equal_rational_true_negative">:: equal_rational_true_negative;
    "equal_rational_false0">:: equal_rational_false0;
    "equal_rational_false1">:: equal_rational_false1;
  ]

let () =
  run_test_tt_main tests
