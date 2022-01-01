open Dobson.Base
open OUnit2
open Printf
open Test_helper

(* new_rational: exceptions ----------------------------------------------------------------------------------------- *)
let new_rational_exc_div_by_zero _ =
  assert_raises
    (Undefined "Attempt to initialize rational number with denominator of zero (0/0).")
    (fun () -> new_rational 0 0)

(* equal_rational: values ------------------------------------------------------------------------------------------- *)
let equal_rational_true_zero _ =
  assert_bool
    "Expected 0/1 = 0/12 to be true but received false."
    (equal_rational (new_rational 0 1) (new_rational 0 12))

let equal_rational_true_positive _ =
  assert_bool
    "Expected 1/10 = 12/120 to be true but received false."
    (equal_rational (new_rational 1 10) (new_rational 12 120))

let equal_rational_true_negative _ =
  assert_bool
    "Expected -1/10 = 12/(-120) to be true but received false."
    (equal_rational (new_rational (-1) 10) (new_rational 12 (-120)))

let equal_rational_false0 _ =
  assert_bool
    "Expected 1/10 = 0/1 to be false but received true."
    (not (equal_rational (new_rational 1 10) (new_rational 0 1)))

let equal_rational_false1 _ =
  assert_bool
    "Expected -1/10 = 1/10 to be false but received true."
    (not (equal_rational (new_rational (-1) 10) (new_rational 1 10)))

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
    (MultipleDefinitions "Multiple definitions provided for variable 'y' (e.g. 2.123, 2.123).")
    (fun () -> eval_rational (Add [Var "x"; Z 1; Var "y"]) 
      [("x", new_rational 1 1); ("y", new_rational 5 2); ("y", new_rational 5 2)])

(* simplify: values ------------------------------------------------------------------------------------------------- *)
let simplify_neg_neg _ =
  assert_equal_expr
    (Var "x")
    (simplify (Neg (Neg (Var "x"))))

let simplify_int _ =
  assert_equal_expr
    (Z 1)
    (simplify (Z 1))

let simplify_neg_int _ =
  assert_equal_expr
    (Z (-1))
    (simplify (Neg (Z 1)))

let simplify_var _ =
  assert_equal_expr
    (Var "x")
    (simplify (Var "x"))

let simplify_neg_var _ =
  assert_equal_expr
    (Neg (Var "x"))
    (simplify (Neg (Var "x")))

let simplify_add0 _ =
  assert_equal_expr
    (Z 2)
    (simplify (Add [Z (-4); Neg (Z 1); Z 7]))

let simplify_add1 _ =
  assert_equal_expr
    (Div (Z 6, Z 5))
    (simplify (Add [Div (Z 3, Z 10); Z 1; Neg (Div (Z 1, Z 10))]))

let simplify_add2 _ =
  assert_equal_expr
    (Z 2)
    (simplify (Add [Z (-5); Mul [Z 2; Z 3]; Div (Z 1, Z 2); Div (Z 1, Z 2)]))

let simplify_add3 _ =
  assert_equal_expr
    (Add [Z 3; Var "x"])
    (simplify (Add [Z 1; Div (Z 3, Z 2); Add [Var "x"; Z 0]; Div (Z 1, Z 2)]))

let simplify_add4 _ =
  assert_equal_expr
    (Var "x")
    (simplify (Add [Z 1; Var "x"; Z (-1)]))

let simplify_add5 _ =
  assert_equal_expr
    (Add [Var "x"; Mul [Neg (Var "y"); Var "y"]])
    (simplify (Neg (Add [Neg (Var "x"); Mul [Var "y"; Var "y"]])))

let simplify_mul0 _ =
  assert_equal_expr
    (Z 6)
    (simplify (Mul [Z 1; Neg (Z 2); Z (-3)]))

let simplify_mul1 _ =
  assert_equal_expr
    (Div (Z (-5), Z 3))
    (simplify (Mul [Neg (Div (Z 1, Z 2)); Z 5; Div (Z 2, Z 3)]))

let simplify_mul2 _ =
  assert_equal_expr
    (Z 3)
    (simplify (Mul [Add [Z 1; Z 2]; Div (Z 5, Z 3); Div (Z 9, Z 15)]))

let simplify_mul3 _ =
  assert_equal_expr
    (Mul [Z 8; Var "x"])
    (simplify (Mul [Z 1; Z 2; Mul [Var "x"; Z 1]; Z 4]))

let simplify_mul4 _ =
  assert_equal_expr
    (Z 0)
    (simplify (Mul [Z 1; Var "x"; Z 0]))

let simplify_mul5 _ =
  assert_equal_expr
    (Var "x")
    (simplify (Mul [Div (Z 1, Z 2); Var "x"; Div (Z 2, Z 1)]))

let simplify_mul6 _ =
  assert_equal_expr
    (Mul [Var "x"; Var "y"])
    (simplify (Mul [Div (Z 1, Z 2); Var "x"; Div (Z 2, Z 1); Var "y"]))

let simplify_div0 _ =
  assert_equal_expr
    (Z (-2))
    (simplify (Div (Neg (Z 4), Z 2)))

let simplify_div1 _ =
  assert_equal_expr
    (Div (Z 2, Z 3))
    (simplify (Div (Div (Z 1, Div (Z 5, Z (-2))), Div (Div (Z 9, Z 5), Z (-3)))))

let simplify_div2 _ =
  assert_equal_expr
    (Div (Z 5, Add [Z 1; Neg (Var "x")]))
    (simplify (Div (Add [Z 2; Z 3], Add [Z 2; Neg (Var "x"); Neg (Z 1)])))

let simplify_div3 _ =
  assert_equal_expr
    (Z 0)
    (simplify (Div (Add [Z 1; Z (-1)], Var "x")))

let simplify_div4 _ =
  assert_equal_expr
    (Var "x")
    (simplify (Div (Div (Var "x", Z 1), Add [Z 2; Z (-1)])))

(* simplify: exceptions --------------------------------------------------------------------------------------------- *)
let simplify_exc_div_by_zero0 _ =
  let e = Div (Z 1, Add [Z 1; Neg (Z 1)]) in
  assert_raises
    (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
    (fun () -> simplify e)

let simplify_exc_div_by_zero1 _ =
  assert_raises
    (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr (Div (Var "x", Z 0)))))
    (fun () -> simplify (Div (Z 1, Div (Var "x", Z 0))))

let simplify_exc_add_num_args0 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> simplify (Add [Z 1]))

let simplify_exc_add_num_args1 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> simplify (Neg (Add [Z 1])))

let simplify_exc_mul_num_args0 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> simplify (Mul [Z 1]))

let simplify_exc_mul_num_args1 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> simplify (Neg (Mul [Z 1])))

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "base_tests">::: [
    "new_rational_exc_div_by_zero">:: new_rational_exc_div_by_zero;
    "equal_rational_true_zero">:: equal_rational_true_zero;
    "equal_rational_true_positive">:: equal_rational_true_positive;
    "equal_rational_true_negative">:: equal_rational_true_negative;
    "equal_rational_false0">:: equal_rational_false0;
    "equal_rational_false1">:: equal_rational_false1;
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
    "string_of_rational_zero">:: string_of_rational_zero;
    "string_of_rational_int_negative">:: string_of_rational_int_negative;
    "string_of_rational_frac_positive_positive">:: string_of_rational_frac_positive_positive;
    "string_of_rational_frac_positive_negative">:: string_of_rational_frac_positive_negative;
    "string_of_rational_frac_negative_positive">:: string_of_rational_frac_negative_positive;
    "string_of_rational_frac_negative_negative">:: string_of_rational_frac_negative_negative;
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
    "simplify_neg_neg">:: simplify_neg_neg;
    "simplify_int">:: simplify_int;
    "simplify_neg_int">:: simplify_neg_int;
    "simplify_var">:: simplify_var;
    "simplify_neg_var">:: simplify_neg_var;
    "simplify_add0">:: simplify_add0;
    "simplify_add1">:: simplify_add1;
    "simplify_add2">:: simplify_add2;
    "simplify_add3">:: simplify_add3;
    "simplify_add4">:: simplify_add4;
    "simplify_add5">:: simplify_add5;
    "simplify_mul0">:: simplify_mul0;
    "simplify_mul1">:: simplify_mul1;
    "simplify_mul2">:: simplify_mul2;
    "simplify_mul3">:: simplify_mul3;
    "simplify_mul4">:: simplify_mul4;
    "simplify_mul5">:: simplify_mul5;
    "simplify_mul6">:: simplify_mul6;
    "simplify_div0">:: simplify_div0;
    "simplify_div1">:: simplify_div1;
    "simplify_div2">:: simplify_div2;
    "simplify_div3">:: simplify_div3;
    "simplify_div4">:: simplify_div4;
    "simplify_exc_div_by_zero0">:: simplify_exc_div_by_zero0;
    "simplify_exc_div_by_zero1">:: simplify_exc_div_by_zero1;
    "simplify_exc_add_num_args0">:: simplify_exc_add_num_args0;
    "simplify_exc_add_num_args1">:: simplify_exc_add_num_args1;
    "simplify_exc_mul_num_args0">:: simplify_exc_mul_num_args0;
    "simplify_exc_mul_num_args1">:: simplify_exc_mul_num_args1;
  ]

let () =
  run_test_tt_main tests
