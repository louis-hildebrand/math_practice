open Base

(* Helper functions *)
let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

let get_rand_from_list (options: 'a list): 'a =
  let i = Random.int (List.length options) in
  List.nth options i

(* Public functions *)
let init: unit -> unit =
  Random.self_init

let seed (n: int): unit =
  Random.init n

type arithmetic_element =
  | AInt
  | AAdd
  | AMul
  | ADiv

let next_fractional (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
  let choose_next_element (min_depth: int) (min_const: int) (max_const: int): arithmetic_element =
    let allowed = [AAdd; AMul] in
    (* Only allow constants if the minimum depth has been reached *)
    let allowed = if min_depth = 0 then AInt::allowed else allowed in
    (* Disallow division if 0 is the only valid constant *)
    let allowed = if min_const != 0 || max_const != 1 then ADiv::allowed else allowed in
    get_rand_from_list allowed
  in
  let rec next_fractional' (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
    if max_depth = 0 then 
      Z (min_const + Random.int (max_const - min_const))
    else
      (* Choose type of expression *)
      let r = choose_next_element min_depth min_const max_const in
      match r with
      | AInt -> Z (min_const + Random.int (max_const - min_const))
      | AAdd -> Add (next_args min_depth max_depth width min_const max_const)
      | AMul -> Mul (next_args min_depth max_depth width min_const max_const)
      | ADiv ->
          let min_depth' = max 0 (min_depth - 1) in
          let e1 = next_fractional' min_depth' (max_depth - 1) width min_const max_const in
          let e2 = get_nonzero min_depth' (max_depth - 1) width min_const max_const in
          Div (e1, e2)
  and next_args (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr list =
    (* Choose width (number of arguments) *)
    let w = 2 + Random.int (width - 1) in
    let min_depth' = max 0 (min_depth - 1) in
    let f = fun () -> next_fractional' min_depth' (max_depth - 1) width min_const max_const in
    List.map f (repeat () w)
  and get_nonzero (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
    let e = next_fractional' min_depth max_depth width min_const max_const in
    if eval e [] = 0.0 then get_nonzero min_depth max_depth width min_const max_const
    else e
  in
  if min_depth < 0 then
    raise (Invalid_argument "Minimum depth of expression cannot be negative.")
  else if max_depth < 0 then
    raise (Invalid_argument "Maximum depth of expression cannot be negative.")
  else if min_depth > max_depth then
    raise (Invalid_argument "Minimum depth of expression must be less than or equal to maximum depth.")
  else if width < 2 then
    raise (Invalid_argument "Width of expression must be at least 2.")
  else if min_const >= max_const then
    raise (Invalid_argument "Minimum constant must be less than maximum constant.")
  else
   next_fractional' min_depth max_depth width min_const max_const