open Printf

(* Types -------------------------------------------------------------------- *)
type expr =
  | Z of int           (* Integer *)
  | Var of string      (* Named variable *)
  | Neg of expr        (* -e1 *)
  | Add of expr list   (* e1 + e2 + e3 + ... *)
  | Mul of expr list   (* e1 * e2 * e3 * ... *)
  | Div of expr * expr (* e1 / e2 *)

type operation =
  | ONeg
  | OAdd
  | OMul
  | ODiv

type expr_context = operation * int (* Parent operation and position (starting at 0) *)

(* Exceptions --------------------------------------------------------------- *)
exception InvalidExpr of string
exception Undefined of string
exception UndefinedVariable of string
exception MultipleDefinitions of string

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

(* TODO: Add 'distribute' flag to allow for distributing negative sign to arguments in Neg (Add es) *)
let rec flatten (e: expr): expr =
  match e with
  | Z _
  | Var _ -> e
  | Neg e' -> Neg (flatten e')
  | Add es ->
      let collect_args e =
        let e' = flatten e in
        match e' with
        | Add es -> es
        | _ -> [e']
      in
      Add (List.fold_right (fun e acc -> (collect_args e) @ acc) es [])
  | Mul es -> Mul (List.map flatten es)
  | Div (e1, e2) -> Div (flatten e1, flatten e2)

(* Public functions --------------------------------------------------------- *)
let init: unit -> unit =
  Random.self_init

let seed (n: int): unit =
  Random.init n

let string_of_expr (e: expr): string =
  let rec string_of_expr' (ctxt: expr_context option) (e: expr): string =
    match e with
    | Z (n) ->
        if n < 0 && (ctxt != None && ctxt != Some (OAdd, 0)) then
          "(" ^ (string_of_int n) ^ ")"
        else
          string_of_int n
    | Var (name) ->
        name
    | Neg (Z n) ->
        if n < 0 then "-(" ^ (string_of_int n) ^ ")"
        else string_of_expr' ctxt (Z (-n))
    | Neg e ->
        "-" ^ (string_of_expr' (Some (ONeg, 0)) e)
    | Add (e1::es) when List.length es >= 1 ->
        let append (i, acc) e =
          let acc' = match e with
            | Z n when n < 0 -> acc ^ " - " ^ (string_of_int (-n))
            | Neg e' -> acc ^ " - " ^ (string_of_expr' (Some (OAdd, i)) e')
            | _ -> acc ^ " + " ^ (string_of_expr' (Some (OAdd, i)) e)
          in
          (i + 1, acc')
        in
        let init = string_of_expr' (Some (OAdd, 0)) e1 in
        let (_, final) = List.fold_left append (1, init) es in
        (match ctxt with
        | None -> final
        | Some (OAdd, _) (* Since the expression tree is flattened ahead of time, this must be a subtraction *)
        | Some (ONeg, _)
        | Some (OMul, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
    | Add _ ->
        raise (InvalidExpr "Wrong number of arguments for operation Add.")
    | Mul (e1::es) when List.length es >= 1 ->
        let init = string_of_expr' (Some (OMul, 0)) e1 in
        let append = fun (i, acc) e -> (i + 1, acc ^ " * " ^ (string_of_expr' (Some (OMul, i)) e)) in
        let (_, final) = List.fold_left append (1, init) es in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OMul, _)
        | Some (ODiv, 0) -> final
        | Some (ONeg, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
    | Mul _ ->
        raise (InvalidExpr "Wrong number of arguments for operation Mul.")
    | Div (e1, e2) ->
        let final = (string_of_expr' (Some (ODiv, 0)) e1) ^ " / " ^ (string_of_expr' (Some (ODiv, 1)) e2) in
        (match ctxt with
        | None
        | Some (OAdd, _)
        | Some (OMul, _)
        | Some (ODiv, 0) -> final
        | Some (ONeg, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
  in
  let e' = flatten e in
  string_of_expr' None e'

let rec eval (e: expr) (vals: (string * float) list): float =
  match e with
  | Z (n) -> float_of_int n
  | Var (name) ->
      let vs = List.map (fun (_, v) -> v) (List.filter (fun (n, _) -> n = name) vals) in
      (match vs with
      | [] -> raise (UndefinedVariable (sprintf "No definition provided for variable '%s'." name))
      | [v] -> v
      | v1::v2::_ -> raise (MultipleDefinitions
          (sprintf "Multiple definitions provided for variable '%s' (e.g. %g, %g)." name v1 v2)))
  | Add (es) when List.length es >= 2 -> List.fold_left (fun acc e -> acc +. eval e vals) 0.0 es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Mul (es) when List.length es >= 2 -> List.fold_left (fun acc e -> acc *. eval e vals) 1.0 es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      let denom = eval e2 vals in
      if denom = 0.0 then raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
      else let numer = eval e1 vals in
      numer /. denom

type arithmetic_element =
  | AInt
  | AAdd
  | AMul
  | ADiv

let choose_next_element (min_depth: int) (min_const: int) (max_const: int): arithmetic_element =
  let allowed = [AAdd; AMul] in
  (* Only allow constants if the minimum depth has been reached *)
  let allowed = if min_depth = 0 then AInt::allowed else allowed in
  (* Disallow division if 0 is the only valid constant *)
  let allowed = if min_const != 0 || max_const != 1 then ADiv::allowed else allowed in
  get_rand_from_list allowed

let rec next_rand' (min_depth: int) (max_depth: int) (width: int) (min_const: int) (max_const: int): expr =
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
  if eval e [] = 0.0 then get_nonzero min_depth max_depth width min_const max_const
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

(* Does NOT check for division by zero *)
let reduce_fraction ((numer: int), (denom: int)): (int * int) =
  let g =
    if denom > 0 then gcd numer denom 
    else ~- (gcd numer denom)
  in
  (numer / g, denom / g)

let add_rational ((n1: int), (d1: int)) ((n2: int), (d2: int)): (int * int) =
  (* n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2) *)
  (* If overflow ever becomes a problem, try finding the LCM of the denominators instead *)
  (n1*d2 + n2*d1, d1*d2)

let multiply_rational ((n1: int), (d1: int)) ((n2: int), (d2: int)): (int * int) =
  (* n1/d1 * n2/d2 = (n1*n2)/(d1*d2) *)
  (n1 * n2, d1 * d2)

let rational_of_ints ((n: int), (d: int)): expr =
  let (n', d') = reduce_fraction (n, d) in
  if d' = 1 then Z (n')
  else Div (Z n', Z d')

let equals (e1: expr) (e2: expr): bool =
  try
    eval e1 [] = eval e2 []
  (* TODO: Implement this properly *)
  with UndefinedVariable _ ->
    false

let rec simplify (e: expr): expr =
  match e with
  | Z _
  | Var _ -> e
  | Add (es) when List.length es >= 2 -> simplify_add es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Mul (es) when List.length es >= 2 -> simplify_mul es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) -> simplify_div e1 e2
and simplify_add (args: expr list): expr =
  (* Add rationals and accumulate anything else *)
  let f (r, nr) arg =
    let arg' = simplify arg in
    match r, arg' with
    | Z (n), Z (m) -> (rational_of_ints (add_rational (n, 1) (m, 1)), nr)
    | Z (n), Div (Z n1, Z d1) -> (rational_of_ints (add_rational (n, 1) (n1, d1)), nr)
    | Div (Z n1, Z d1), Z (n) -> (rational_of_ints (add_rational (n1, d1) (n, 1)), nr)
    | Div (Z n1, Z d1), Div (Z n2, Z d2) -> (rational_of_ints (add_rational (n1, d1) (n2, d2)), nr)
    | _ -> (r, nr @ [arg'])
  in
  let (r, nr) = List.fold_left f (Z 0, []) args in
  if nr = [] then r
  else
    let addends = if r = Z 0 then nr else r :: nr in
    if List.length addends = 1 then List.hd addends
    else Add (r :: nr)
and simplify_mul (args: expr list): expr =
  (* Multiply rationals and accumulate everything else *)
  let f (r, nr) arg =
    let arg' = simplify arg in
    match r, arg' with
    | Z (n), Z (m) -> (rational_of_ints (multiply_rational (n, 1) (m, 1)), nr)
    | Z (n), Div (Z n1, Z d1) -> (rational_of_ints (multiply_rational (n, 1) (n1, d1)), nr)
    | Div (Z n1, Z d1), Z (n) -> (rational_of_ints (multiply_rational (n1, d1) (n, 1)), nr)
    | Div (Z n1, Z d1), Div (Z n2, Z d2) -> (rational_of_ints (multiply_rational (n1, d1) (n2, d2)), nr)
    | _ -> (r, nr @ [arg'])
  in
  let (r, nr) = List.fold_left f (Z 1, []) args in
  if nr = [] then r
  else if r = Z 0 then Z 0
  else
    let factors = if r = Z 1 then nr else r :: nr in
    if List.length factors = 1 then List.hd factors
    else Mul factors
and simplify_div (e1: expr) (e2: expr): expr =
  let denom_identically_zero = equals e2 (Z 0) in
  if denom_identically_zero then
    raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr (Div (e1, e2)))))
  else
    let (e1', e2') = (simplify e1, simplify e2) in
    match e1', e2' with
    | Z (n), Z (m) -> rational_of_ints (n, m)
    | Z (n), Div (Z n1, Z d1) -> rational_of_ints (multiply_rational (n, 1) (d1, n1))
    | Div (Z n1, Z d1), Z (n) -> rational_of_ints (multiply_rational (n1, d1) (1, n))
    | Div (Z n1, Z d1), Div (Z n2, Z d2) -> rational_of_ints (multiply_rational (n1, d1) (d2, n2))
    | Z (0), _ -> Z (0)
    | _, Z (1) -> e1'
    | _ -> Div (e1', e2')
