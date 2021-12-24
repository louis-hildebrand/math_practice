open OUnit2
open Math_expr
open Printf

(* Helper functions ------------------------------------------------------------------------------------------------- *)
(* Calculates the depth of an expression tree *)
let rec depth (e: expr): int =
  match e with
  | Z _
  | Var _ -> 0
  | Add es
  | Sub es
  | Mul es -> 1 + (List.fold_left (fun acc e -> max acc (depth e)) 0 es)
  | Div (e1, e2) -> 1 + max (depth e1) (depth e2)

(* tabulate origin dest returns the list [origin; origin + 1; ...; dest - 1; dest] *)
let rec tabulate (origin: int) (dest: int): int list =
  if origin = dest then [dest]
  else origin :: (tabulate (origin + 1) dest)

let rec has_div_by_zero (e: expr): bool =
  match e with
  | Z _
  | Var _ -> false
  | Add es
  | Sub es
  | Mul es -> List.exists (fun arg -> has_div_by_zero arg) es
  | Div (e1, e2) -> has_div_by_zero e1 || has_div_by_zero e2 || (try eval e2 [] = 0.0 with UndefinedVariable _ -> false)

let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

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
    | Var _ -> []
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

let next_rand_max_width _ =
  (* Asserts a failure if there is any operation in e with more than w arguments *)
  let rec f w e =
    match e with
    | Z _
    | Var _ -> ()
    | Add es
    | Sub es
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
  let random_exprs = List.map (fun () -> next_rand 2 2 2 0 1) (repeat () 100) in
  let f e =
    if has_div_by_zero e then assert_failure (sprintf "Found expression with division by zero: %s." (string_of_expr e))
    else ()
  in
  List.iter f random_exprs

let next_rand_no_div_by_zero3 _ =
  let random_exprs = List.map (fun () -> next_rand 2 2 2 0 2) (repeat () 100) in
  let f e =
    if has_div_by_zero e then assert_failure (sprintf "Found expression with division by zero: %s." (string_of_expr e))
    else ()
  in
  List.iter f random_exprs

(* next_rand: exceptions -------------------------------------------------------------------------------------------- *)
let next_rand_exc_min_depth_invalid _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression cannot be negative.")
    (fun () -> next_rand (-1) 3 2 0 1)

let next_rand_exc_max_depth_invalid _ =
  assert_raises
    (Invalid_argument "Maximum depth of expression cannot be negative.")
    (fun () -> next_rand 0 (-1) 2 0 1)

let next_rand_exc_min_depth_greater_than_max_depth _ =
  assert_raises
    (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
    (fun () -> next_rand 1 0 2 0 1)

let next_rand_exc_width_invalid _ =
  assert_raises
    (Invalid_argument "Width of expression must be at least 2.")
    (fun () -> next_rand 0 0 1 0 1)

let next_rand_exc_min_const_equal_max_const _ =
  assert_raises
    (Invalid_argument "Minimum constant must be less than maximum constant.")
    (fun () -> next_rand 0 0 2 0 0)

(* string_of_expr: single operations -------------------------------------------------------------------------------- *)
(* Z (positive) *)
let string_of_expr_int_positive _ =
  assert_equal
    ~printer: (fun x -> x)
    "1"
    (string_of_expr (Z 1))

(* Z (negative) *)
let string_of_expr_int_negative _ =
  assert_equal
    ~printer: (fun x -> x)
    "-1"
    (string_of_expr (Z (-1)))

(* Variable *)
let string_of_expr_var _ =
  assert_equal
    ~printer: (fun x -> x)
    "x"
    (string_of_expr (Var "x"))

(* Add *)
let string_of_expr_add _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 + 2 + (-3)"
    (string_of_expr (Add [Z 1; Z 2; Z (-3)]))

(* Sub *)
let string_of_expr_sub _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 - 2 - (-3)"
    (string_of_expr (Sub [Z 1; Z 2; Z (-3)]))

(* Mul *)
let string_of_expr_mul _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 * 2 * (-3)"
    (string_of_expr (Mul [Z 1; Z 2; Z (-3)]))

(* Div *)
let string_of_expr_div _ =
  assert_equal
    ~printer: (fun x -> x)
    "2 / (-1)"
    (string_of_expr (Div (Z 2, Z (-1))))

(* string_of_expr: order of operations ------------------------------------------------------------------------------ *)
(* Add / Add *)
let string_of_expr_order_add_add _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 + x + y + 4"
    (string_of_expr (Add [Add [Z 1; Var "x"]; Add [Var "y"; Z 4]]))

(* Add / Sub *)
let string_of_expr_order_add_sub _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 + 2 - 3 + 1 - (2 + 3)"
    (string_of_expr (Add [Sub [Add [Z 1; Z 2]; Z 3]; Sub [Z 1; Add [Z 2; Z 3]]]))

(* Add / Mul *)
let string_of_expr_order_add_mul _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 * 2 + (1 + 2) * (3 + 4)"
    (string_of_expr (Add [Mul [Z 1; Z 2]; Mul [Add [Z 1; Z 2]; Add [Z 3; Z 4]]]))

(* Add / Div *)
let string_of_expr_order_add_div _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 / 2 + (1 + 2) / (3 + 4)"
    (string_of_expr (Add [Div (Z 1, Z 2); Div (Add [Z 1; Z 2], Add [Z 3; Z 4])]))

(* Sub / Sub *)
let string_of_expr_order_sub_sub _ =
  assert_equal
    ~printer: (fun x -> x)
    "x - 2 - (3 - y)"
    (string_of_expr (Sub [Sub [Var "x"; Z 2]; Sub [Z 3; Var "y"]]))

(* Sub / Mul *)
let string_of_expr_order_sub_mul _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 * 2 - (1 - 2) * (3 - 4)"
    (string_of_expr (Sub [Mul [Z 1; Z 2]; Mul [Sub [Z 1; Z 2]; Sub [Z 3; Z 4]]]))

(* Sub / Mul *)
let string_of_expr_order_sub_div _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 / 2 - (1 - 2) / (3 - 4)"
    (string_of_expr (Sub [Div (Z 1, Z 2); Div (Sub [Z 1; Z 2], Sub [Z 3; Z 4])]))

(* Mul / Mul *)
let string_of_expr_order_mul_mul _ =
  assert_equal
    ~printer: (fun x -> x)
    "x * 2 * 3 * y"
    (string_of_expr (Mul [Mul [Var "x"; Z 2]; Mul [Z 3; Var "y"]]))

(* Mul / Div *)
let string_of_expr_order_mul_div _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 / 2 * 1 * 2 / (3 * 4)"
    (string_of_expr (Mul [Div (Z 1, Z 2); Div (Mul [Z 1; Z 2], Mul [Z 3; Z 4])]))

(* Div / Div *)
let string_of_expr_order_div_div _ =
  assert_equal
    ~printer: (fun x -> x)
    "1 / x / (y / 4)"
    (string_of_expr (Div (Div (Z 1, Var "x"), Div (Var "y", Z 4))))

(* string_of_expr: exceptions --------------------------------------------------------------------------------------- *)
let string_of_expr_exc_test0 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> string_of_expr (Add [Z 1]))

let string_of_expr_exc_test1 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Sub.")
    (fun () -> string_of_expr (Sub [Z 1]))

let string_of_expr_exc_test2 _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> string_of_expr (Mul [Z 1]))

(* eval: values ----------------------------------------------------------------------------------------------------- *)
let eval_int _ =
  assert_equal
    ~printer: string_of_float
    1.0
    (eval (Z 1) [])

let eval_var _ =
  assert_equal
    ~printer: string_of_float
    2.1
    (eval (Var "x") [("x", 2.1)])

let eval_add_novars _ =
  assert_equal
    ~printer: string_of_float
    8.0
    (eval (Add [Z (-1); Z 2; Add [Z 3; Z 4]]) [])

let eval_add_vars _ =
  assert_equal
    ~printer: string_of_float
    ~cmp: (cmp_float ~epsilon:0.000000000000001)
    (-6.8)
    (eval (Add [Var "x"; Z (-1); Add [Z 2; Var "y"]]) [("x", 3.1); ("y", -10.9)])

let eval_sub_novars _ =
  assert_equal
    ~printer: string_of_float
    4.0
    (eval (Sub [Z 1; Z (-2); Sub [Z 3; Z 4]]) [])

let eval_sub_vars _ =
  assert_equal
    ~printer: string_of_float
    ~cmp: (cmp_float ~epsilon:0.000000000000001)
    (-0.8)
    (eval (Sub [Var "y"; Z 1; Var "x"]) [("x", 0.1); ("y", 0.3)])

let eval_mul_novars _ =
  assert_equal
    ~printer: string_of_float
    24.0
    (eval (Mul [Z 1; Z 2; Mul [Z 3; Z 4]]) [])

let eval_mul_vars _ =
  assert_equal
    ~printer: string_of_float
    24.0
    (eval (Mul [Var "x"; Var "y"; Var "z"; Z 4]) [("x", 1.0); ("y", 2.0); ("z", 3.0)])

let eval_div_novars _ =
  assert_equal
    ~printer: string_of_float
    0.5
    (eval (Div (Z 1, Div (Z 2, Z 1))) [])

let eval_div_vars _ =
  assert_equal
    ~printer: string_of_float
    6.0
    (eval (Div (Var "y", Div (Z 1, Var "x"))) [("x", 2.0); ("y", 3.0)])

(* eval: exceptions ------------------------------------------------------------------------------------------------- *)
let eval_exc_div_by_zero _ =
  assert_raises
    (Undefined "Attempt to divide by zero in expression 1 / (1 - 1).")
    (fun () -> eval (Div (Z 1, Sub [Z 1; Z 1])) [])

let eval_exc_add_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> eval (Add [Z 1]) [])

let eval_exc_sub_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Sub.")
    (fun () -> eval (Sub [Z 1]) [])

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

(* simplify: values ------------------------------------------------------------------------------------------------- *)
let simplify_int _ =
  assert_equal
    ~printer: string_of_expr
    (Z 1)
    (simplify (Z 1))

let simplify_var _ =
  assert_equal
    ~printer: string_of_expr
    (Var "x")
    (simplify (Var "x"))

let simplify_add0 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 2)
    (simplify (Add [Z (-4); Z 1; Z 5]))

let simplify_add1 _ =
  assert_equal
    ~printer: string_of_expr
    (Div (Z 9, Z 5))
    (simplify (Add [Div (Z 7, Z 10); Z 1; Div (Z 1, Z 10)]))

let simplify_add2 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 2)
    (simplify (Add [Z (-5); Mul [Z 2; Z 3]; Div (Z 1, Z 2); Div (Z 1, Z 2)]))

let simplify_add3 _ =
  assert_equal
    ~printer: string_of_expr
    (Add [Z 3; Var "x"])
    (simplify (Add [Z 1; Div (Z 3, Z 2); Var "x"; Div (Z 1, Z 2)]))

let simplify_sub0 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 2)
    (simplify (Sub [Z 5; Z 4; Z (-1)]))

let simplify_sub1 _ =
  assert_equal
    ~printer: string_of_expr
    (Div (Z (-4), Z 5))
    (simplify (Sub [Div (Z 3, Z 10); Z 1; Div (Z 1, Z 10)]))

let simplify_sub2 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 1)
    (simplify (Sub [Add [Z 1; Z 2]; Div (Z 1, Z 2); Div (Z 3, Z 2)]))

let simplify_sub3 _ =
  assert_equal
    ~printer: string_of_expr
    (Sub [Var "x"; Z 3])
    (simplify (Sub [Var "x"; Z 1; Div (Z 4, Z 2)]))

let simplify_sub4 _ =
  assert_equal
    ~printer: string_of_expr
    (Sub [Div (Z 1, Z 2); Var "x"])
    (simplify (Sub [Z 1; Var "x"; Div (Z 1, Z 2)]))

let simplify_mul0 _ =
  assert_equal
    ~printer: string_of_expr
    (Z (-6))
    (simplify (Mul [Z 1; Z 2; Z (-3)]))

let simplify_mul1 _ =
  assert_equal
    ~printer: string_of_expr
    (Div (Z 5, Z 3))
    (simplify (Mul [Div (Z 1, Z 2); Z 5; Div (Z 2, Z 3)]))

let simplify_mul2 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 3)
    (simplify (Mul [Add [Z 1; Z 2]; Div (Z 5, Z 3); Div (Z 9, Z 15)]))

let simplify_mul3 _ =
  assert_equal
    ~printer: string_of_expr
    (Mul [Z 4; Var "x"])
    (simplify (Mul [Z 1; Z 2; Var "x"; Z 4]))

let simplify_div0 _ =
  assert_equal
    ~printer: string_of_expr
    (Z 2)
    (simplify (Div (Z 4, Z 2)))

let simplify_div1 _ =
  assert_equal
    ~printer: string_of_expr
    (Div (Z 2, Z 3))
    (simplify (Div (Div (Z 1, Div (Z 5, Z (-2))), Div (Div (Z 9, Z 5), Z (-3)))))

let simplify_div2 _ =
  assert_equal
    ~printer: string_of_expr
    (Div (Z 2, Var "x"))
    (simplify (Div (Z 2, Var "x")))

(* simplify: exceptions --------------------------------------------------------------------------------------------- *)
let simplify_exc_div_by_zero _ =
  assert_raises
    (Undefined "Attempt to divide by zero in expression 1 / (1 - 1).")
    (fun () -> simplify (Div (Z 1, Sub [Z 1; Z 1])))

let simplify_exc_add_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Add.")
    (fun () -> simplify (Add [Z 1]))

let simplify_exc_sub_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Sub.")
    (fun () -> simplify (Sub [Z 1]))

let simplify_exc_mul_num_args _ =
  assert_raises
    (InvalidExpr "Wrong number of arguments for operation Mul.")
    (fun () -> simplify (Mul [Z 1]))

(* List and run tests ----------------------------------------------------------------------------------------------- *)
let tests =
  "math_expr_tests">::: [
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
    "string_of_expr_int_positive">:: string_of_expr_int_positive;
    "string_of_expr_int_negative">:: string_of_expr_int_negative;
    "string_of_expr_var">:: string_of_expr_var;
    "string_of_expr_add">:: string_of_expr_add;
    "string_of_expr_sub">:: string_of_expr_sub;
    "string_of_expr_mul">:: string_of_expr_mul;
    "string_of_expr_div">:: string_of_expr_div;
    "string_of_expr_order_add_add">:: string_of_expr_order_add_add;
    "string_of_expr_order_add_sub">:: string_of_expr_order_add_sub;
    "string_of_expr_order_add_mul">:: string_of_expr_order_add_mul;
    "string_of_expr_order_add_div">:: string_of_expr_order_add_div;
    "string_of_expr_order_sub_sub">:: string_of_expr_order_sub_sub;
    "string_of_expr_order_sub_mul">:: string_of_expr_order_sub_mul;
    "string_of_expr_order_sub_div">:: string_of_expr_order_sub_div;
    "string_of_expr_order_mul_mul">:: string_of_expr_order_mul_mul;
    "string_of_expr_order_mul_div">:: string_of_expr_order_mul_div;
    "string_of_expr_order_div_div">:: string_of_expr_order_div_div;
    "string_of_expr_exc_test0">:: string_of_expr_exc_test0;
    "string_of_expr_exc_test1">:: string_of_expr_exc_test1;
    "string_of_expr_exc_test2">:: string_of_expr_exc_test2;
    "eval_int">:: eval_int;
    "eval_var">:: eval_var;
    "eval_add_novars">:: eval_add_novars;
    "eval_add_vars">:: eval_add_vars;
    "eval_sub_novars">:: eval_sub_novars;
    "eval_sub_vars">:: eval_sub_vars;
    "eval_mul_novars">:: eval_mul_novars;
    "eval_mul_vars">:: eval_mul_vars;
    "eval_div_novars">:: eval_div_novars;
    "eval_div_vars">:: eval_div_vars;
    "eval_exc_div_by_zero">:: eval_exc_div_by_zero;
    "eval_exc_add_num_args">:: eval_exc_add_num_args;
    "eval_exc_sub_num_args">:: eval_exc_sub_num_args;
    "eval_exc_mul_num_args">:: eval_exc_mul_num_args;
    "eval_exc_unknown_var0">:: eval_exc_unknown_var0;
    "eval_exc_unknown_var1">:: eval_exc_unknown_var1;
    "simplify_int">:: simplify_int;
    "simplify_add0">:: simplify_add0;
    "simplify_add1">:: simplify_add1;
    "simplify_add2">:: simplify_add2;
    "simplify_add3">:: simplify_add3;
    "simplify_sub0">:: simplify_sub0;
    "simplify_sub1">:: simplify_sub1;
    "simplify_sub2">:: simplify_sub2;
    "simplify_sub3">:: simplify_sub3;
    "simplify_sub4">:: simplify_sub4;
    "simplify_mul0">:: simplify_mul0;
    "simplify_mul1">:: simplify_mul1;
    "simplify_mul2">:: simplify_mul2;
    "simplify_mul3">:: simplify_mul3;
    "simplify_div0">:: simplify_div0;
    "simplify_div1">:: simplify_div1;
    "simplify_div2">:: simplify_div2;
    "simplify_exc_div_by_zero">:: simplify_exc_div_by_zero;
    "simplify_exc_add_num_args">:: simplify_exc_add_num_args;
    "simplify_exc_sub_num_args">:: simplify_exc_sub_num_args;
    "simplify_exc_mul_num_args">:: simplify_exc_mul_num_args;
  ]

let () =
  (* List.iter (fun (s, e) -> printf "%d: %s\n" s (string_of_expr e)) random_exprs *)
  run_test_tt_main tests
