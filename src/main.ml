open Printf

let () =
  Math_expr.init ();
  let e = Math_expr.next_rand 0 1 3 (-19) 20 in
  printf "Random expression: %s\n" (Math_expr.string_of_expr e)
