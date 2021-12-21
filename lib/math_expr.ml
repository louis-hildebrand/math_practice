open Printf

(* Types -------------------------------------------------------------------- *)
type expr =
  | Z of int           (* Integer *)
  | Add of expr list   (* e1 + e2 + e3 + ... *)
  | Sub of expr list   (* e1 - e2 - e3 - ... *)
  | Mul of expr list   (* e1 * e2 * e3 * ... *)
  | Div of expr * expr (* e1 / e2 *)

type operation =
  | OAdd
  | OSub
  | OMul
  | ODiv

type expr_context = operation * int (* Parent operation and position (starting at 0) *)

(* Exceptions --------------------------------------------------------------- *)
exception InvalidExpr of string
exception Undefined of string

(* Helper functions --------------------------------------------------------- *)
let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

let get_rand_from_list (options: 'a list): 'a =
  let i = Random.int (List.length options) in
  List.nth options i

(* Public functions --------------------------------------------------------- *)
let init: unit -> unit =
  Random.self_init

let seed (n: int): unit =
  Random.init n

let string_of_expr (e: expr): string =
  let rec string_of_expr' (ctxt: expr_context option) (e: expr): string =
    match e with
    | Z (n) ->
        if n < 0 then "(" ^ (string_of_int n) ^ ")"
        else string_of_int n
    | Add (e1::e2::es) ->
        let init = (string_of_expr' (Some (OAdd, 0)) e1) ^ " + " ^ (string_of_expr' (Some (OAdd, 1)) e2) in
        let append = fun (i, acc) e -> (i + 1, acc ^ " + " ^ (string_of_expr' (Some (OAdd, i)) e)) in
        let (_, final) = List.fold_left append (2, init) es in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OSub, 0) -> final
        | Some (OSub, _)
        | Some (OMul, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
    | Add _ ->
        raise (InvalidExpr "Wrong number of arguments for operation Add.")
    | Sub (e1::e2::es) ->
        let init = (string_of_expr' (Some (OSub, 0)) e1) ^ " - " ^ (string_of_expr' (Some (OSub, 1)) e2) in
        let append = fun (i, acc) e -> (i + 1, acc ^ " - " ^ (string_of_expr' (Some (OSub, i)) e)) in
        let (_, final) = List.fold_left append (2, init) es in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OSub, 0) -> final
        | Some (OSub, _)
        | Some (OMul, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
    | Sub _ ->
        raise (InvalidExpr "Wrong number of arguments for operation Sub.")
    | Mul (e1::e2::es) ->
        let init = (string_of_expr' (Some (OMul, 0)) e1) ^ " * " ^ (string_of_expr' (Some (OMul, 1)) e2) in
        let append = fun (i, acc) e -> (i + 1, acc ^ " * " ^ (string_of_expr' (Some (OMul, i)) e)) in
        let (_, final) = List.fold_left append (2, init) es in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OSub, _)
        | Some (OMul, _)
        | Some (ODiv, 0) -> final
        | Some (ODiv, _) -> "(" ^ final ^ ")")
    | Mul _ ->
        raise (InvalidExpr "Wrong number of arguments for operation Mul.")
    | Div (e1, e2) ->
        let final = (string_of_expr' (Some (ODiv, 0)) e1) ^ " / " ^ (string_of_expr' (Some (ODiv, 1)) e2) in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OSub, _)
        | Some (OMul, _)
        | Some (ODiv, 0) -> final
        | Some (ODiv, _) -> "(" ^ final ^ ")")
  in
  string_of_expr' None e

let rec eval (e: expr): float =
  match e with
  | Z (n) -> float_of_int n
  | Add (e1::e2::es) -> List.fold_left (fun acc e -> acc +. eval e) (eval e1 +. eval e2) es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Sub (e1::e2::es) -> List.fold_left (fun acc e -> acc -. eval e) (eval e1 -. eval e2) es
  | Sub _ -> raise (InvalidExpr "Wrong number of arguments for operation Sub.")
  | Mul (e1::e2::es) -> List.fold_left (fun acc e -> acc *. eval e) (eval e1 *. eval e2) es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      let denom = eval e2 in
      if denom = 0.0 then raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
      else let numer = eval e1 in
      numer /. denom

let choose_next_element (min_depth: int) (min_const: int) (max_const: int): int =
  let allowed = [1; 2; 3] in
  (* Only allow constants if the minimum depth has been reached *)
  let allowed = if min_depth = 0 then 0::allowed else allowed in
  (* Disallow division if 0 is the only valid constant *)
  let allowed = if min_const != 0 || max_const != 1 then 4::allowed else allowed in
  get_rand_from_list allowed

let rec next_rand' (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
  if max_depth = 0 then 
    Z (min_const + Random.int (max_const - min_const))
  else
    (* Choose type of expression *)
    let r = choose_next_element min_depth min_const max_const in
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
      let e1 = next_rand' min_depth' (max_depth - 1) width min_const max_const in
      let e2 = get_nonzero min_depth' (max_depth - 1) width min_const max_const in
      Div (e1, e2)
and next_args (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr list =
  (* Choose width (number of arguments) *)
  let w = 2 + Random.int (width - 1) in
  let min_depth' = max 0 (min_depth - 1) in
  let f = fun () -> next_rand' min_depth' (max_depth - 1) width min_const max_const in
  List.map f (repeat () w)
and get_nonzero (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
  let e = next_rand' min_depth max_depth width min_const max_const in
  if eval e = 0.0 then get_nonzero min_depth max_depth width min_const max_const
  else e

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

let gcd (n: int) (m: int): int =
  let rec gcd' big small =
    if small = 0 then big
    else gcd' small (big mod small)
  in
  let (n', m') = (abs n, abs m) in
  gcd' (max n' m') (min n' m')

let reduce_fraction ((numer: int), (denom: int)): (int * int) =
  if denom = 0 then
    raise (Undefined (sprintf "Attempt to divide by 0 in expression %s." (string_of_expr (Div (Z numer, Z denom)))))
  else
    let g =
      if denom > 0 then gcd numer denom 
      else ~- (gcd numer denom)
    in
    (numer / g, denom / g)

let add_rational ((n1: int), (d1: int)) ((n2: int), (d2: int)): (int * int) =
  (* n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2) *)
  (* If overflow ever becomes a problem, try finding the LCM of the denominators instead *)
  (n1*d2 + n2*d1, d1*d2)

let rational_of_ints ((n: int), (d: int)): expr =
  let (n', d') = reduce_fraction (n, d) in
  if d' = 1 then Z (n')
  else Div (Z n', Z d')

let simplify_add (args: expr list): expr =
  (* Add rationals and accumulate non-rational terms *)
  let f (r, nr) arg =
    match r, arg with
    | Z (n), Z (m) -> (rational_of_ints (add_rational (n, 1) (m, 1)), nr)
    | Z (n), Div (Z n1, Z d1) -> (rational_of_ints (add_rational (n, 1) (n1, d1)), nr)
    | Div (Z n1, Z d1), Z (n) -> (rational_of_ints (add_rational (n1, d1) (n, 1)), nr)
    | Div (Z n1, Z d1), Div (Z n2, Z d2) -> (rational_of_ints (add_rational (n1, d1) (n2, d2)), nr)
    | _ -> (r, arg::nr)
  in
  let (r, nr) = List.fold_left f (Z 0, []) args in
  if List.length nr = 0 then r
  else Add (r::nr)

exception NotImplemented
let simplify_sub (args: expr list): expr =
  raise NotImplemented

let simplify_mul (args: expr list): expr =
  raise NotImplemented

let simplify_div (e1: expr) (e2: expr): expr =
  raise NotImplemented

let simplify (e: expr): expr =
  match e with
  | Z (n) -> Z (n)
  | Add (es) -> simplify_add es
  | Sub (es) -> simplify_sub es
  | Mul (es) -> simplify_mul es
  | Div (e1, e2) -> simplify_div e1 e2
