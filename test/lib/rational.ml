open Dobson.Rational
open OUnit2
open Printf
open Test_helper

(* new_rational: exceptions ----------------------------------------------------------------------------------------- *)
let new_rational_exc_div_by_zero _ =
  assert_raises
    Division_by_zero
    (fun () -> new_rational 0 0)

(* string_of_rational ----------------------------------------------------------------------------------------------- *)
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

(* float_of_rational ------------------------------------------------------------------------------------------------ *)
let float_of_rational0 _ =
  assert_equal_float
    2.5
    (float_of_rational (new_rational 10 4))

(* split ------------------------------------------------------------------------------------------------------------ *)
let split0 _ =
  assert_equal
    ~printer: (fun (n, d) -> sprintf "(%d, %d)" n d)
    (5, 2)
    (split (new_rational 10 4))

(* add -------------------------------------------------------------------------------------------------------------- *)
let add_positive _ =
  assert_equal_rational
    (new_rational 4 5)
    (new_rational 1 10 +: new_rational 7 10)

let add_negative _ =
  assert_equal_rational
    (new_rational (-3) 5)
    (new_rational 3 10 +: new_rational 9 (-10))

(* subtract --------------------------------------------------------------------------------------------------------- *)
let subtract_positive _ =
  assert_equal_rational
    (new_rational (-3) 5)
    (new_rational 3 10 -: new_rational 9 10)

let subtract_negative _ =
  assert_equal_rational
    (new_rational 4 5)
    (new_rational 1 10 -: new_rational 7 (-10))

(* negate ----------------------------------------------------------------------------------------------------------- *)
let negate_positive _ =
  assert_equal_rational
    (new_rational (-3) 10)
    (~-:(new_rational 12 40))

let negate_negative _ =
  assert_equal_rational
    (new_rational 3 10)
    (~-:(new_rational 12 (-40)))

(* multiply --------------------------------------------------------------------------------------------------------- *)
let multiply_positive _ =
  assert_equal_rational
    (new_rational 9 5)
    (new_rational 12 5 *: new_rational 3 4)

let multiply_negative _ =
  assert_equal_rational
    (new_rational (-14) 5)
    (new_rational 12 5 *: new_rational 7 (-6))

(* divide: values --------------------------------------------------------------------------------------------------- *)
let divide_positive _ =
  assert_equal_rational
    (new_rational 9 5)
    (new_rational 12 5 /: new_rational 4 3)

let divide_negative _ =
  assert_equal_rational
    (new_rational (-14) 5)
    (new_rational 12 5 /: new_rational 6 (-7))

(* divide: exceptions ----------------------------------------------------------------------------------------------- *)
let divide_exc_div_by_zero _ =
  assert_raises
    Division_by_zero
    (fun () -> new_rational 0 1 /: new_rational 0 1)

(* equal ------------------------------------------------------------------------------------------------------------ *)
let equal_true_zero _ =
  assert_bool
    "Expected 0/1 = 0/12 to be true but received false."
    (new_rational 0 1 =: new_rational 0 12)

let equal_true_positive _ =
  assert_bool
    "Expected 1/10 = 12/120 to be true but received false."
    (new_rational 1 10 =: new_rational 12 120)

let equal_true_negative _ =
  assert_bool
    "Expected -1/10 = 12/(-120) to be true but received false."
    (new_rational (-1) 10 =: new_rational 12 (-120))

let equal_false0 _ =
  assert_bool
    "Expected 1/10 = 0/1 to be false but received true."
    (not (new_rational 1 10 =: new_rational 0 1))

let equal_false1 _ =
  assert_bool
    "Expected -1/10 = 1/10 to be false but received true."
    (not (new_rational (-1) 10 =: new_rational 1 10))

(* not equal -------------------------------------------------------------------------------------------------------- *)
let not_equal_false_zero _ =
  assert_bool
    "Expected 0/1 != 0/12 to be false but received true."
    (not (new_rational 0 1 <>: new_rational 0 12))

let not_equal_false_positive _ =
  assert_bool
    "Expected 1/10 != 12/120 to be false but received true."
    (not (new_rational 1 10 <>: new_rational 12 120))

let not_equal_false_negative _ =
  assert_bool
    "Expected -1/10 != 12/(-120) to be false but received true."
    (not (new_rational (-1) 10 <>: new_rational 12 (-120)))

let not_equal_true0 _ =
  assert_bool
    "Expected 1/10 != 0/1 to be true but received false."
    (new_rational 1 10 <>: new_rational 0 1)

let not_equal_true1 _ =
  assert_bool
    "Expected -1/10 != 1/10 to be true but received false."
    (new_rational (-1) 10 <>: new_rational 1 10)

(* less than -------------------------------------------------------------------------------------------------------- *)
let lt_true _ =
  assert_bool
    "Expected 7/10 < 5/6 to be true but received false."
    (new_rational 7 10 <: new_rational 5 6)

let lt_false _ =
  assert_bool
    "Expected 5/6 < 7/10 to be false but received true."
    (not (new_rational 5 6 <: new_rational 7 10))

let lt_equal _ =
  assert_bool
    "Expected 3/6 < 5/10 to be false but received true."
    (not (new_rational 3 6 <: new_rational 5 10))

(* less than or equal ----------------------------------------------------------------------------------------------- *)
let leq_true _ =
  assert_bool
    "Expected 7/10 <= 5/6 to be true but received false."
    (new_rational 7 10 <=: new_rational 5 6)

let leq_false _ =
  assert_bool
    "Expected 5/6 <= 7/10 to be false but received true."
    (not (new_rational 5 6 <=: new_rational 7 10))

let leq_equal _ =
  assert_bool
    "Expected 3/6 <= 5/10 to be true but received false."
    (new_rational 3 6 <=: new_rational 5 10)

(* greater than ----------------------------------------------------------------------------------------------------- *)
(* (>:) is also defined in OUnit2, so use the prefix form of this operator here to avoid ambiguity. *)
let gt_true _ =
  assert_bool
    "Expected 5/6 > 7/10 to be true but received false."
    (Dobson.Rational.(>:) (new_rational 5 6) (new_rational 7 10))

let gt_false _ =
  assert_bool
    "Expected 7/10 > 5/6 to be false but received true."
    (not (Dobson.Rational.(>:) (new_rational 7 10) (new_rational 5 6)))

let gt_equal _ =
  assert_bool
    "Expected 5/10 > 3/6 to be false but received true."
    (not (Dobson.Rational.(>:) (new_rational 5 10) (new_rational 3 6)))

(* greater than or equal -------------------------------------------------------------------------------------------- *)
let geq_true _ =
  assert_bool
    "Expected 5/6 >= 7/10 to be true but received false."
    (new_rational 5 6 >=: new_rational 7 10)

let geq_false _ =
  assert_bool
    "Expected 7/10 >= 5/6 to be false but received true."
    (not (new_rational 7 10 >=: new_rational 5 6))

let geq_equal _ =
  assert_bool
    "Expected 3/6 >= 5/10 to be true but received false."
    (new_rational 3 6 >=: new_rational 5 10)

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
    "float_of_rational0">:: float_of_rational0;
    "split0">:: split0;
    "add_positive">:: add_positive;
    "add_negative">:: add_negative;
    "subtract_positive">:: subtract_positive;
    "subtract_negative">:: subtract_negative;
    "negate_positive">:: negate_positive;
    "negate_negative">:: negate_negative;
    "multiply_positive">:: multiply_positive;
    "multiply_negative">:: multiply_negative;
    "divide_positive">:: divide_positive;
    "divide_negative">:: divide_negative;
    "divide_exc_div_by_zero">:: divide_exc_div_by_zero;
    "equal_true_zero">:: equal_true_zero;
    "equal_true_positive">:: equal_true_positive;
    "equal_true_negative">:: equal_true_negative;
    "equal_false0">:: equal_false0;
    "equal_false1">:: equal_false1;
    "not_equal_false_zero">:: not_equal_false_zero;
    "not_equal_false_positive">:: not_equal_false_positive;
    "not_equal_false_negative">:: not_equal_false_negative;
    "not_equal_true0">:: not_equal_true0;
    "not_equal_true1">:: not_equal_true1;
    "lt_true">:: lt_true;
    "lt_false">:: lt_false;
    "lt_equal">:: lt_equal;
    "leq_true">:: leq_true;
    "leq_false">:: leq_false;
    "leq_equal">:: leq_equal;
    "gt_true">:: gt_true;
    "gt_false">:: gt_false;
    "gt_equal">:: gt_equal;
    "geq_true">:: geq_true;
    "geq_false">:: geq_false;
    "geq_equal">:: geq_equal;
  ]

let () =
  run_test_tt_main tests
