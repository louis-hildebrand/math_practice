open Dobson.Base
open OUnit2
open Printf
open Dobson.Rational
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

let string_of_expr_real_positive _ =
  assert_equal_string
    "2.5"
    (string_of_expr (R 2.50))

let string_of_expr_real_negative _ =
  assert_equal_string
    "-3"
    (string_of_expr (R (-3.0)))

let string_of_expr_neg_real_positive _ =
  assert_equal_string
    "-0.02"
    (string_of_expr (Neg (R 0.02)))

let string_of_expr_neg_real_negative _ =
  assert_equal_string
    "-(-10.52)"
    (string_of_expr (Neg (R (-10.52))))

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
    "1 + 2.2 - 3"
    (string_of_expr (Add [Z 1; R 2.2; Z (-3)]))

let string_of_expr_add1 _ =
  assert_equal_string
    "-1 - 2.25 + 3"
    (string_of_expr (Add [Z (-1); R (-2.25); Z 3]))

let string_of_expr_add_neg0 _ =
  assert_equal_string
    (string_of_expr (Add [Z 1; R 2.2; Z (-3)]))
    (string_of_expr (Add [Z 1; R 2.2; Neg (Z 3)]))

let string_of_expr_add_neg1 _ =
  assert_equal_string
    (string_of_expr (Add [Z (-1); R (-2.25); Z 3]))
    (string_of_expr (Add [Neg (Z 1); Neg (R 2.25); Z 3]))

let string_of_expr_add_neg_neg _ =
  assert_equal_string
    "1 - (-2) - (-3) - (-2.5) - (-3.66) - (-(-x))"
    (string_of_expr (Add [
        Z 1; 
        Neg (Z (-2)); 
        Neg (Neg (Z 3)); 
        Neg (R (-2.5));
        Neg (Neg (R 3.66)); 
        Neg (Neg (Neg (Var "x")))]))

let string_of_expr_neg_add _ =
  assert_equal_string
    "-(1 + 2 - 3)"
    (string_of_expr (Neg (Add [Z 1; Z 2; Z (-3)])))

let string_of_expr_mul0 _ =
  assert_equal_string
    "x * 2.1 * (-3)"
    (string_of_expr (Mul [Var "x"; R 2.1; Z (-3)]))

let string_of_expr_mul1 _ =
  assert_equal_string
    "(-4.321) * 1 * (-x)"
    (string_of_expr (Mul [R (-4.321); Z 1; Neg (Var "x")]))

let string_of_expr_mul_neg0 _ =
  assert_equal_string
    (string_of_expr (Mul [Var "x"; R 2.1; Z (-3)]))
    (string_of_expr (Mul [Var "x"; R 2.1; Neg (Z 3)]))

let string_of_expr_mul_neg1 _ =
  assert_equal_string
    (string_of_expr (Mul [R (-4.321); Z 1; Neg (Var "x")]))
    (string_of_expr (Mul [Neg (R 4.321); Z 1; Neg (Var "x")]))

let string_of_expr_neg_mul _ =
  assert_equal_string
    "-(1 * 2 * (-3))"
    (string_of_expr (Neg (Mul [Z 1; Z 2; Neg (Z 3)])))

let string_of_expr_div0 _ =
  assert_equal_string
    "2 / (-1.3)"
    (string_of_expr (Div (Z 2, R (-1.3))))

let string_of_expr_div1 _ =
  assert_equal_string
    "(-2) / 1.1"
    (string_of_expr (Div (Z (-2), R 1.1)))

let string_of_expr_div2 _ =
  assert_equal_string
    "(-x) / 2"
    (string_of_expr (Div (Neg (Var "x"), Z 2)))

let string_of_expr_div_neg0 _ =
  assert_equal_string
    (string_of_expr (Div (Z 2, R (-1.3))))
    (string_of_expr (Div (Z 2, Neg (R 1.3))))

let string_of_expr_div_neg1 _ =
  assert_equal_string
    (string_of_expr (Div (Z (-2), R 1.1)))
    (string_of_expr (Div (Neg (Z 2), R 1.1)))

let string_of_expr_neg_div _ =
  assert_equal_string
    "-(2 / (-1))"
    (string_of_expr (Neg (Div (Z 2, Z (-1)))))

let string_of_expr_pow0 _ =
  assert_equal_string
    "x^2"
    (string_of_expr (Pow (Var "x", Z 2)))

let string_of_expr_pow1 _ =
  assert_equal_string
    "(-1)^-(n + 1)"
    (string_of_expr (Pow (Neg (Z 1), Neg (Add [Var "n"; Z 1]))))

let string_of_expr_pow2 _ =
  assert_equal_string
    "(-(1 + 3))^z"
    (string_of_expr (Pow (Neg (Add [Z 1; Z 3]), Var "z")))

let string_of_expr_neg_pow _ =
  assert_equal_string
    "-2^x"
    (string_of_expr (Neg (Pow (Z 2, Var "x"))))

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

(* Add / Exp *)
let string_of_expr_order_add_pow _ =
  assert_equal_string
    "x^2 + (x + 1)^(k + 1)"
    (string_of_expr (Add [Pow (Var "x", Z 2); Pow (Add [Var "x"; Z 1], Add [Var "k"; Z 1])]))

(* Sub / Mul *)
let string_of_expr_order_sub_mul _ =
  assert_equal_string
    "-(1 * 2) - (1 - 2) * (-3 + 4)"
    (string_of_expr (Add [Neg (Mul [Z 1; Z 2]); Neg (Mul [Add [Z 1; Z (-2)]; Add [Z (-3); Z 4]])]))

(* Sub / Div *)
let string_of_expr_order_sub_div _ =
  assert_equal_string
    "-(1 / 2) - (1 - 2) / (-3 + 4)"
    (string_of_expr (Add [Neg (Div (Z 1, Z 2)); Neg (Div (Add [Z 1; Z (-2)], Add [Z (-3); Z 4]))]))

(* Sub / Exp *)
let string_of_expr_order_sub_pow _ =
  assert_equal_string
    "-x^3 - x^2"
    (string_of_expr (Add [Neg (Pow (Var "x", Z 3)); Neg (Pow (Var "x", Z 2))]))

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

(* Mul / Exp *)
let string_of_expr_order_mul_pow _ =
  assert_equal_string
    "x^2 * (3 * k)^(4 * x)"
    (string_of_expr (Mul [Pow (Var "x", Z 2); Pow (Mul [Z 3; Var "k"], Mul [Z 4; Var "x"])]))

(* Div / Div *)
let string_of_expr_order_div_div _ =
  assert_equal_string
    "1 / x / (y / 4)"
    (string_of_expr (Div (Div (Z 1, Var "x"), Div (Var "y", Z 4))))

(* Div / Exp *)
let string_of_expr_order_div_pow _ =
  assert_equal_string
    "x^2 / (1 / 2)^(x / 3)"
    (string_of_expr (Div (Pow (Var "x", Z 2), Pow (Div (Z 1, Z 2), Div (Var "x", Z 3)))))

(* Exp / Exp *)
let string_of_expr_order_pow_pow _ =
  assert_equal_string
    "(2^2)^2^2"
    (string_of_expr (Pow (Pow (Z 2, Z 2), Pow (Z 2, Z 2))))

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

let eval_real _ =
  assert_equal_float
    2.4
    (eval (R 2.4) [])

let eval_neg_real _ =
  assert_equal_float
    (-6.7)
    (eval (Neg (R 6.7)) [])

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
    8.7
    (eval (Add [Z (-1); R 2.7; Add [Z 3; Z 4]]) [])

let eval_add_vars _ =
  assert_equal_float
    (-6.8)
    (eval (Add [Var "x"; Z (-1); Add [Z 2; Var "y"]]) [("x", 3.1); ("y", -10.9)])

let eval_mul_novars _ =
  assert_equal_float
    28.0
    (eval (Mul [Z 1; Z 2; Mul [R 3.5; Z 4]]) [])

let eval_mul_vars _ =
  assert_equal_float
    24.0
    (eval (Mul [Var "x"; Var "y"; Var "z"; Z 4]) [("x", 1.0); ("y", 2.0); ("z", 3.0)])

let eval_div_novars _ =
  assert_equal_float
    0.75
    (eval (Div (R 1.5, Div (Z 2, Z 1))) [])

let eval_div_vars _ =
  assert_equal_float
    6.0
    (eval (Div (Var "y", Div (Z 1, Var "x"))) [("x", 2.0); ("y", 3.0)])

let eval_pow_novars _ =
  assert_equal_float
    (0.5 ** 1.5)
    (eval (Pow (R 0.5, R 1.5)) [])

let eval_pow_vars _ =
  assert_equal_float
    (2.0 ** 1.5)
    (eval (Pow (Add [Var "x"; Z 1], Mul [Z 3; Var "k"])) [("x", 1.0); ("k", 0.5)])

let eval_pow_zero_zero _ =
  assert_equal_float
    1.0
    (eval (Pow (Z 0, R 0.0)) [])

(* eval: exceptions ------------------------------------------------------------------------------------------------- *)
let eval_exc_div_by_zero0 _ =
  let e = Div (Z 1, Add [Z 1; Neg (R 0.75); R (-0.25)]) in
  assert_raises
    (Undefined (string_of_expr e, None))
    (fun () -> eval e [])

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

let eval_exc_div_by_zero1 _ =
  let e = Pow (Var "x", Add [Z 1; Z (-3)]) in
  assert_raises
    (Undefined (sprintf "%s evaluated to infinity." (string_of_expr e), None))
    (fun () -> eval e [("x", 0.0)])

let eval_exc_even_root_negative _ =
  let e = Pow (Var "x", Div (Z 1, Var "k")) in
  assert_raises
    (Undefined (sprintf "%s evaluated to nan." (string_of_expr e), None))
    (fun () -> eval e [("x", -1.5); ("k", 4.0)])

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

let eval_rational_pow_novars _ =
  assert_equal_rational
    (new_rational 27 1)
    (eval_rational (Pow (Z 9, Div (Z 3, Z 2))) [])

let eval_rational_pow_vars _ =
  assert_equal_rational
    (new_rational 4 9)
    (eval_rational (Pow (Var "b", Var "x")) [("b", new_rational 8 27); ("x", new_rational 2 3)])

let eval_rational_pow_neg_exponent _ =
  assert_equal_rational
    (new_rational 1 100)
    (eval_rational (Pow (Z 10, Z (-2))) [])

let eval_rational_pow_neg_base _ =
  assert_equal_rational
    (new_rational (-2) 1)
    (eval_rational (Pow (Z (-8), Div (Z 1, Z 3))) [])

let eval_rational_pow_zero_zero _ =
  assert_equal_rational
    (new_rational 1 1)
    (eval_rational (Pow (Add [Z 1; Z (-1)], Div (Z 0, Z 3))) [])

(* eval_rational: exceptions ---------------------------------------------------------------------------------------- *)
let eval_rational_exc_div_by_zero0 _ =
  let e = Div (Z 1, Add [Z 1; Neg (Z 1)]) in
  assert_raises
    (Undefined (string_of_expr e, None))
    (fun () -> eval_rational e [])

let eval_rational_exc_add_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> eval_rational (Add [Z 1]) [])

let eval_rational_exc_mul_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> eval_rational (Mul [Z 1]) [])

let eval_rational_exc_no_def _ =
  assert_raises
    (UndefinedVariable "No definition provided for variable 'y'.")
    (fun () -> eval_rational (Add [Var "x"; Z 1; Var "y"]) [("x", new_rational 1 1)])

let eval_rational_exc_multiple_defs _ =
  assert_raises
    (MultipleDefinitions "Multiple definitions provided for variable 'y' (e.g. 5/2, 5/2).")
    (fun () -> eval_rational (Add [Var "x"; Z 1; Var "y"]) 
      [("x", new_rational 1 1); ("y", new_rational 5 2); ("y", new_rational 5 2)])

let eval_rational_exc_real_num _ =
  assert_raises
    (NonRational "2.5")
    (fun () -> eval_rational (Add [Div (Z 5, Z 2); R 2.5]) [])

let eval_rational_exc_div_by_zero1 _ =
  let e = Pow (Var "x", Add [Z 1; Z (-3)]) in
  assert_raises
    (Undefined (string_of_expr e, Some Division_by_zero))
    (fun () -> eval_rational e [("x", new_rational 0 1)])

let eval_rational_exc_pow_neg_base _ =
  let e = Pow (Div (Z (-1), Z 2), Div (Z 1, Var "x")) in
  assert_raises
    (Undefined (string_of_expr e, Some (Root_negative (-1))))
    (fun () -> eval_rational e [("x", new_rational 4 1)])

let eval_rational_exc_pow_irrational _ =
  assert_raises
    (NonRational "2nd root of 7")
    (fun () -> eval_rational (Pow (Var "x", Div (Z 3, Z 2))) [("x", new_rational 7 1)])

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "base_tests">::: [
    "string_of_expr_int_positive">:: string_of_expr_int_positive;
    "string_of_expr_int_negative">:: string_of_expr_int_negative;
    "string_of_expr_neg_int_positive">:: string_of_expr_neg_int_positive;
    "string_of_expr_neg_int_negative">:: string_of_expr_neg_int_negative;
    "string_of_expr_real_positive">:: string_of_expr_real_positive;
    "string_of_expr_real_negative">:: string_of_expr_real_negative;
    "string_of_expr_neg_real_positive">:: string_of_expr_neg_real_positive;
    "string_of_expr_neg_real_negative">:: string_of_expr_neg_real_negative;
    "string_of_expr_var">:: string_of_expr_var;
    "string_of_expr_neg_var">:: string_of_expr_neg_var;
    "string_of_expr_add0">:: string_of_expr_add0;
    "string_of_expr_add1">:: string_of_expr_add1;
    "string_of_expr_add_neg0">:: string_of_expr_add_neg0;
    "string_of_expr_add_neg1">:: string_of_expr_add_neg1;
    "string_of_expr_add_neg_neg">:: string_of_expr_add_neg_neg;
    "string_of_expr_neg_add">:: string_of_expr_neg_add;
    "string_of_expr_mul0">:: string_of_expr_mul0;
    "string_of_expr_mul1">:: string_of_expr_mul1;
    "string_of_expr_mul_neg0">:: string_of_expr_mul_neg0;
    "string_of_expr_mul_neg1">:: string_of_expr_mul_neg1;
    "string_of_expr_neg_mul">:: string_of_expr_neg_mul;
    "string_of_expr_div0">:: string_of_expr_div0;
    "string_of_expr_div1">:: string_of_expr_div1;
    "string_of_expr_div2">:: string_of_expr_div2;
    "string_of_expr_div_neg0">:: string_of_expr_div_neg0;
    "string_of_expr_div_neg1">:: string_of_expr_div_neg1;
    "string_of_expr_neg_div">:: string_of_expr_neg_div;
    "string_of_expr_pow0">:: string_of_expr_pow0;
    "string_of_expr_pow1">:: string_of_expr_pow1;
    "string_of_expr_pow2">:: string_of_expr_pow2;
    "string_of_expr_neg_pow">:: string_of_expr_neg_pow;
    "string_of_expr_order_add_add">:: string_of_expr_order_add_add;
    "string_of_expr_order_add_sub">:: string_of_expr_order_add_sub;
    "string_of_expr_order_add_mul">:: string_of_expr_order_add_mul;
    "string_of_expr_order_add_div">:: string_of_expr_order_add_div;
    "string_of_expr_order_add_pow">:: string_of_expr_order_add_pow;
    "string_of_expr_order_sub_mul">:: string_of_expr_order_sub_mul;
    "string_of_expr_order_sub_div">:: string_of_expr_order_sub_div;
    "string_of_expr_order_sub_pow">:: string_of_expr_order_sub_pow;
    "string_of_expr_order_mul_mul">:: string_of_expr_order_mul_mul;
    "string_of_expr_order_mul_div">:: string_of_expr_order_mul_div;
    "string_of_expr_order_mul_pow">:: string_of_expr_order_mul_pow;
    "string_of_expr_order_div_div">:: string_of_expr_order_div_div;
    "string_of_expr_order_div_pow">:: string_of_expr_order_div_pow;
    "string_of_expr_order_pow_pow">:: string_of_expr_order_pow_pow;
    "string_of_expr_exc_test0">:: string_of_expr_exc_test0;
    "string_of_expr_exc_test1">:: string_of_expr_exc_test1;
    "eval_int">:: eval_int;
    "eval_neg_int">:: eval_neg_int;
    "eval_real">:: eval_real;
    "eval_neg_real">:: eval_neg_real;
    "eval_var">:: eval_var;
    "eval_neg_var">:: eval_neg_var;
    "eval_add_novars">:: eval_add_novars;
    "eval_add_vars">:: eval_add_vars;
    "eval_mul_novars">:: eval_mul_novars;
    "eval_mul_vars">:: eval_mul_vars;
    "eval_div_novars">:: eval_div_novars;
    "eval_div_vars">:: eval_div_vars;
    "eval_pow_novars">:: eval_pow_vars;
    "eval_pow_vars">:: eval_pow_vars;
    "eval_pow_zero_zero">:: eval_pow_zero_zero;
    "eval_exc_div_by_zero0">:: eval_exc_div_by_zero0;
    "eval_exc_add_num_args">:: eval_exc_add_num_args;
    "eval_exc_mul_num_args">:: eval_exc_mul_num_args;
    "eval_exc_unknown_var0">:: eval_exc_unknown_var0;
    "eval_exc_unknown_var1">:: eval_exc_unknown_var1;
    "eval_exc_div_by_zero1">:: eval_exc_div_by_zero1;
    "eval_exc_even_root_negative">:: eval_exc_even_root_negative;
    "eval_rational_int">:: eval_rational_int;
    "eval_rational_var">:: eval_rational_var;
    "eval_rational_neg">:: eval_rational_neg;
    "eval_rational_div_novars">:: eval_rational_div_novars;
    "eval_rational_div_vars">:: eval_rational_div_vars;
    "eval_rational_add_novars">:: eval_rational_add_novars;
    "eval_rational_add_vars">:: eval_rational_add_vars;
    "eval_rational_mul_novars">:: eval_rational_mul_novars;
    "eval_rational_mul_vars">:: eval_rational_mul_vars;
    "eval_rational_pow_novars">:: eval_rational_pow_novars;
    "eval_rational_pow_vars">:: eval_rational_pow_vars;
    "eval_rational_pow_neg_exponent">:: eval_rational_pow_neg_exponent;
    "eval_rational_pow_neg_base">:: eval_rational_pow_neg_base;
    "eval_rational_pow_zero_zero">:: eval_rational_pow_zero_zero;
    "eval_rational_exc_div_by_zero0">:: eval_rational_exc_div_by_zero0;
    "eval_rational_exc_add_num_args">:: eval_rational_exc_add_num_args;
    "eval_rational_exc_mul_num_args">:: eval_rational_exc_mul_num_args;
    "eval_rational_exc_no_def">:: eval_rational_exc_no_def;
    "eval_rational_exc_multiple_defs">:: eval_rational_exc_multiple_defs;
    "eval_rational_exc_real_num">:: eval_rational_exc_real_num;
    "eval_rational_exc_div_by_zero1">:: eval_rational_exc_div_by_zero1;
    "eval_rational_exc_pow_neg_base">:: eval_rational_exc_pow_neg_base;
    "eval_rational_exc_pow_irrational">:: eval_rational_exc_pow_irrational;
  ]

let () =
  run_test_tt_main tests
