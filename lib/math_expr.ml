(* Types -------------------------------------------------------------------- *)
type expr =
  | Z of int           (* Integer *)
  | Add of expr list   (* e1 + e2 + e3 + ... *)
  | Sub of expr list   (* e1 - e2 - e3 - ... *)
  | Mul of expr list   (* e1 * e2 * e3 * ... *)
  | Div of expr * expr (* e1 / e2 *)

(* Exceptions --------------------------------------------------------------- *)
exception InvalidExpr of string

(* Helper functions --------------------------------------------------------- *)
let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

(* Public functions --------------------------------------------------------- *)
let init: unit -> unit =
  Random.self_init

let seed (n: int): unit =
  Random.init n

let rec next_rand' (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
  if max_depth = 0 then 
    Z (min_const + Random.int (max_const - min_const))
  else
    (* Choose type of expression *)
    let r = if min_depth = 0 then Random.int 5 else 1 + Random.int 4 in
    if r = 0 then
      Z (min_const + Random.int (max_const - min_const))
    else if r = 1 then
      Add (next_args min_depth max_depth width min_const max_const)
    else if r = 2 then
      Sub (next_args min_depth max_depth width min_const max_const)
    else if r = 3 then
      Mul (next_args min_depth max_depth width min_const max_const)
    else
      let min_depth' = max 0 (min_depth - 1) in
      let e1 = (next_rand' min_depth' (max_depth - 1) width min_const max_const) in
      let e2 = (next_rand' min_depth' (max_depth - 1) width min_const max_const) in
      Div (e1, e2)
and next_args (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr list =
  (* Choose width (number of arguments) *)
  let w = 2 + Random.int (width - 1) in
  let min_depth' = max 0 (min_depth - 1) in
  let f = fun () -> next_rand' min_depth' (max_depth - 1) width min_const max_const in
  List.map f (repeat () w)

let next_rand (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
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
    next_rand' min_depth max_depth width min_const max_const

let rec string_of_expr (e: expr): string =
  match e with
  | Z (n) ->
      if n < 0 then "(" ^ (string_of_int n) ^ ")"
      else string_of_int n
  | Add (e1::e2::es) ->
      let init = "(" ^ (string_of_expr e1) ^ " + " ^ (string_of_expr e2) in
      let append = fun acc e -> acc ^ " + " ^ (string_of_expr e) in
      let final = List.fold_left append init es in
      final ^ ")"
  | Add _ ->
      raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Sub (e1::e2::es) ->
      let init = "(" ^ (string_of_expr e1) ^ " - " ^ (string_of_expr e2) in
      let append = fun acc e -> acc ^ " - " ^ (string_of_expr e) in
      let final = List.fold_left append init es in
      final ^ ")"
  | Sub _ ->
      raise (InvalidExpr "Wrong number of arguments for operation Sub.")
  | Mul (e1::e2::es) ->
      let init = "(" ^ (string_of_expr e1) ^ " * " ^ (string_of_expr e2) in
      let append = fun acc e -> acc ^ " * " ^ (string_of_expr e) in
      let final = List.fold_left append init es in
      final ^ ")"
  | Mul _ ->
      raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      "(" ^ (string_of_expr e1) ^ " / " ^ (string_of_expr e2) ^ ")"
