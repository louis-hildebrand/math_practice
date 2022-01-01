open Dobson.Base
open OUnit2
open Printf
open Test_helper

(* string_of_expr: single operations -------------------------------------------------------------------------------- *)
let string_of_expr_int_positive _ =
  assert_equal_string
    "1"
    (string_of_expr (Z 1))

let string_of_expr_int_negative _ =
  assert_equal_string
    "-1"
    (string_of_expr (Z (-1)))

let string_of_expr_neg_int_positive _ =
  assert_equal_string
    (string_of_expr (Z (-1)))
    (string_of_expr (Neg (Z 1)))

let string_of_expr_neg_int_negative _ =
  assert_equal_string
    "-(-1)"
    (string_of_expr (Neg (Z (-1))))

let string_of_expr_var _ =
  assert_equal_string
    "x"
    (string_of_expr (Var "x"))

let string_of_expr_neg_var _ =
  assert_equal_string
    "-x"
    (string_of_expr (Neg (Var "x")))

let string_of_expr_add0 _ =
  assert_equal_string
    "1 + 2 - 3"
    (string_of_expr (Add [Z 1; Z 2; Z (-3)]))

let string_of_expr_add1 _ =
  assert_equal_string
    "-1 + 2 + 3"
    (string_of_expr (Add [Z (-1); Z 2; Z 3]))

let string_of_expr_add_neg0 _ =
  assert_equal_string
    (string_of_expr (Add [Z 1; Z 2; Z (-3)]))
    (string_of_expr (Add [Z 1; Z 2; Neg (Z 3)]))

let string_of_expr_add_neg1 _ =
  assert_equal_string
    (string_of_expr (Add [Z (-1); Z 2; Z 3]))
    (string_of_expr (Add [Neg (Z 1); Z 2; Z 3]))

let string_of_expr_add_neg2 _ =
  assert_equal_string
    "1 - (-(-x))"
    (string_of_expr (Add [Z 1; Neg (Neg (Neg (Var "x")))]))

let string_of_expr_neg_add _ =
  assert_equal_string
    "-(1 + 2 - 3)"
    (string_of_expr (Neg (Add [Z 1; Z 2; Z (-3)])))

let string_of_expr_mul0 _ =
  assert_equal_string
    "(-x) * 2 * (-3)"
    (string_of_expr (Mul [Neg (Var "x"); Z 2; Z (-3)]))

let string_of_expr_mul1 _ =
  assert_equal_string
    "(-1) * (-x) * 3"
    (string_of_expr (Mul [Z (-1); Neg (Var "x"); Z 3]))

let string_of_expr_mul_neg0 _ =
  assert_equal_string
    (string_of_expr (Mul [Z 1; Z 2; Z (-3)]))
    (string_of_expr (Mul [Z 1; Z 2; Neg (Z 3)]))

let string_of_expr_mul_neg1 _ =
  assert_equal_string
    (string_of_expr (Mul [Z (-1); Z 2; Z 3]))
    (string_of_expr (Mul [Neg (Z 1); Z 2; Z 3]))

let string_of_expr_neg_mul _ =
  assert_equal_string
    "-(1 * 2 * (-3))"
    (string_of_expr (Neg (Mul [Z 1; Z 2; Neg (Z 3)])))

let string_of_expr_div0 _ =
  assert_equal_string
    "2 / (-1)"
    (string_of_expr (Div (Z 2, Z (-1))))

let string_of_expr_div1 _ =
  assert_equal_string
    "(-2) / 1"
    (string_of_expr (Div (Z (-2), Z 1)))

let string_of_expr_div2 _ =
  assert_equal_string
    "(-x) / 2"
    (string_of_expr (Div (Neg (Var "x"), Z 2)))

let string_of_expr_div3 _ =
  assert_equal_string
    "2 / (-x)"
    (string_of_expr (Div (Z 2, Neg (Var "x"))))

let string_of_expr_div_neg0 _ =
  assert_equal_string
    (string_of_expr (Div (Z 2, Z (-1))))
    (string_of_expr (Div (Z 2, Neg (Z 1))))

let string_of_expr_div_neg1 _ =
  assert_equal_string
    (string_of_expr (Div (Z (-2), Z 1)))
    (string_of_expr (Div (Neg (Z 2), Z 1)))

let string_of_expr_neg_div _ =
  assert_equal_string
    "-(2 / (-1))"
    (string_of_expr (Neg (Div (Z 2, Z (-1)))))

(* string_of_expr: order of operations ------------------------------------------------------------------------------ *)
(* Add / Add *)
let string_of_expr_order_add_add _ =
  assert_equal_string
    "1 + x + y + 4"
    (string_of_expr (Add [Add [Z 1; Var "x"]; Add [Var "y"; Z 4]]))

(* Add / Sub *)
let string_of_expr_order_add_sub _ =
  assert_equal_string
    "1 + 2 - 3 - 1 - (2 + 3)"
    (string_of_expr (Add [Add [Add [Z 1; Z 2]; Neg (Z 3)]; Add [Z (-1); Neg (Add [Z 2; Z 3])]]))

(* Add / Mul *)
let string_of_expr_order_add_mul _ =
  assert_equal_string
    "1 * 2 + (1 + 2) * (3 + 4)"
    (string_of_expr (Add [Mul [Z 1; Z 2]; Mul [Add [Z 1; Z 2]; Add [Z 3; Z 4]]]))

(* Add / Div *)
let string_of_expr_order_add_div _ =
  assert_equal_string
    "1 / 2 + (1 + 2) / (3 + 4)"
    (string_of_expr (Add [Div (Z 1, Z 2); Div (Add [Z 1; Z 2], Add [Z 3; Z 4])]))

(* Mul / Sub *)
let string_of_expr_order_mul_sub _ =
  assert_equal_string
    "-(1 * 2) - (1 - 2) * (-3 + 4)"
    (string_of_expr (Add [Neg (Mul [Z 1; Z 2]); Neg (Mul [Add [Z 1; Z (-2)]; Add [Z (-3); Z 4]])]))

(* Mul / Mul *)
let string_of_expr_order_mul_mul _ =
  assert_equal_string
    "x * 2 * 3 * y"
    (string_of_expr (Mul [Mul [Var "x"; Z 2]; Mul [Z 3; Var "y"]]))

(* Mul / Div *)
let string_of_expr_order_mul_div _ =
  assert_equal_string
    "1 / 2 * 1 * 2 / (3 * 4)"
    (string_of_expr (Mul [Div (Z 1, Z 2); Div (Mul [Z 1; Z 2], Mul [Z 3; Z 4])]))

(* Div / Sub *)
let string_of_expr_order_div_sub _ =
  assert_equal_string
    "-(1 / 2) - (1 - 2) / (-3 + 4)"
    (string_of_expr (Add [Neg (Div (Z 1, Z 2)); Neg (Div (Add [Z 1; Z (-2)], Add [Z (-3); Z 4]))]))

(* Div / Div *)
let string_of_expr_order_div_div _ =
  assert_equal_string
    "1 / x / (y / 4)"
    (string_of_expr (Div (Div (Z 1, Var "x"), Div (Var "y", Z 4))))

(* string_of_expr: exceptions --------------------------------------------------------------------------------------- *)
let string_of_expr_exc_test0 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> string_of_expr (Add [Z 1]))

let string_of_expr_exc_test1 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> string_of_expr (Mul [Z 1]))

(* eval: values ----------------------------------------------------------------------------------------------------- *)
let eval_int _ =
  assert_equal_float
    1.0
    (eval (Z 1) [])

let eval_neg_int _ =
  assert_equal_float
    (-1.0)
    (eval (Neg (Z 1)) [])

let eval_var _ =
  assert_equal_float
    2.1
    (eval (Var "x") [("x", 2.1)])

let eval_neg_var _ =
  assert_equal_float
    (-2.1)
    (eval (Neg (Var "x")) [("x", 2.1)])

let eval_add_novars _ =
  assert_equal_float
    8.0
    (eval (Add [Z (-1); Z 2; Add [Z 3; Z 4]]) [])

let eval_add_vars _ =
  assert_equal_float
    (-6.8)
    (eval (Add [Var "x"; Z (-1); Add [Z 2; Var "y"]]) [("x", 3.1); ("y", -10.9)])

let eval_mul_novars _ =
  assert_equal_float
    24.0
    (eval (Mul [Z 1; Z 2; Mul [Z 3; Z 4]]) [])

let eval_mul_vars _ =
  assert_equal_float
    24.0
    (eval (Mul [Var "x"; Var "y"; Var "z"; Z 4]) [("x", 1.0); ("y", 2.0); ("z", 3.0)])

let eval_div_novars _ =
  assert_equal_float
    0.5
    (eval (Div (Z 1, Div (Z 2, Z 1))) [])

let eval_div_vars _ =
  assert_equal_float
    6.0
    (eval (Div (Var "y", Div (Z 1, Var "x"))) [("x", 2.0); ("y", 3.0)])

(* eval: exceptions ------------------------------------------------------------------------------------------------- *)
let eval_exc_div_by_zero _ =
  assert_raises
    (Undefined (sprintf "Attempt to divide by zero in expression %s."
      (string_of_expr (Div (Z 1, Add [Z 1; Neg (Z 1)])))))
    (fun () -> eval (Div (Z 1, Add [Z 1; Neg (Z 1)])) [])

let eval_exc_add_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> eval (Add [Z 1]) [])

let eval_exc_mul_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> eval (Mul [Z 1]) [])

let eval_exc_unknown_var0 _ =
  assert_raises
    (UndefinedVariable "No definition provided for variable 'y'.")
    (fun () -> eval (Add [Var "x"; Z 1; Var "y"]) [("x", 1.0)])

let eval_exc_unknown_var1 _ =
  assert_raises
    (MultipleDefinitions "Multiple definitions provided for variable 'y' (e.g. 2.123, 2.123).")
    (fun () -> eval (Add [Var "x"; Z 1; Var "y"]) [("x", 1.0); ("y", 2.123); ("y", 2.123)])

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "base_tests">::: [
    "string_of_expr_int_positive">:: string_of_expr_int_positive;
    "string_of_expr_int_negative">:: string_of_expr_int_negative;
    "string_of_expr_neg_int_positive">:: string_of_expr_neg_int_positive;
    "string_of_expr_neg_int_negative">:: string_of_expr_neg_int_negative;
    "string_of_expr_var">:: string_of_expr_var;
    "string_of_expr_neg_var">:: string_of_expr_neg_var;
    "string_of_expr_add0">:: string_of_expr_add0;
    "string_of_expr_add1">:: string_of_expr_add1;
    "string_of_expr_add_neg0">:: string_of_expr_add_neg0;
    "string_of_expr_add_neg1">:: string_of_expr_add_neg1;
    "string_of_expr_add_neg2">:: string_of_expr_add_neg2;
    "string_of_expr_neg_add">:: string_of_expr_neg_add;
    "string_of_expr_mul0">:: string_of_expr_mul0;
    "string_of_expr_mul1">:: string_of_expr_mul1;
    "string_of_expr_mul_neg0">:: string_of_expr_mul_neg0;
    "string_of_expr_mul_neg1">:: string_of_expr_mul_neg1;
    "string_of_expr_neg_mul">:: string_of_expr_neg_mul;
    "string_of_expr_div0">:: string_of_expr_div0;
    "string_of_expr_div1">:: string_of_expr_div1;
    "string_of_expr_div2">:: string_of_expr_div2;
    "string_of_expr_div3">:: string_of_expr_div3;
    "string_of_expr_div_neg0">:: string_of_expr_div_neg0;
    "string_of_expr_div_neg1">:: string_of_expr_div_neg1;
    "string_of_expr_neg_div">:: string_of_expr_neg_div;
    "string_of_expr_order_add_add">:: string_of_expr_order_add_add;
    "string_of_expr_order_add_sub">:: string_of_expr_order_add_sub;
    "string_of_expr_order_add_mul">:: string_of_expr_order_add_mul;
    "string_of_expr_order_add_div">:: string_of_expr_order_add_div;
    "string_of_expr_order_mul_sub">:: string_of_expr_order_mul_sub;
    "string_of_expr_order_mul_mul">:: string_of_expr_order_mul_mul;
    "string_of_expr_order_mul_div">:: string_of_expr_order_mul_div;
    "string_of_expr_order_div_sub">:: string_of_expr_order_div_sub;
    "string_of_expr_order_div_div">:: string_of_expr_order_div_div;
    "string_of_expr_exc_test0">:: string_of_expr_exc_test0;
    "string_of_expr_exc_test1">:: string_of_expr_exc_test1;
    "eval_int">:: eval_int;
    "eval_neg_int">:: eval_neg_int;
    "eval_var">:: eval_var;
    "eval_neg_var">:: eval_neg_var;
    "eval_add_novars">:: eval_add_novars;
    "eval_add_vars">:: eval_add_vars;
    "eval_mul_novars">:: eval_mul_novars;
    "eval_mul_vars">:: eval_mul_vars;
    "eval_div_novars">:: eval_div_novars;
    "eval_div_vars">:: eval_div_vars;
    "eval_exc_div_by_zero">:: eval_exc_div_by_zero;
    "eval_exc_add_num_args">:: eval_exc_add_num_args;
    "eval_exc_mul_num_args">:: eval_exc_mul_num_args;
    "eval_exc_unknown_var0">:: eval_exc_unknown_var0;
    "eval_exc_unknown_var1">:: eval_exc_unknown_var1;
  ]

let () =
  run_test_tt_main tests
