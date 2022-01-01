open Printf

(* Types -------------------------------------------------------------------- *)
type expr =
  | Z of int           (* Integer *)
  | Var of string      (* Named variable *)
  | Neg of expr        (* -e1 *)
  | Add of expr list   (* e1 + e2 + e3 + ... *)
  | Mul of expr list   (* e1 * e2 * e3 * ... *)
  | Div of expr * expr (* e1 / e2 *)

type rational = int * int

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
let rec negate (e: expr): expr =
  match e with
  | Z n -> Z (-n)
  | Var x -> Neg (Var x)
  | Neg e' -> e'
  | Add es -> Add (List.map negate es)
  (* Assume there is at least 1 argument *)
  | Mul es -> Mul ((negate (List.hd es)) :: (List.tl es))
  | Div (e1, e2) -> Div (negate e1, e2)

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
(* TODO: Remove *)
exception NotImplemented
let new_rational (numerator: int) (denominator: int): rational =
  raise NotImplemented

let equal_rational (x: rational) (y: rational): bool =
  raise NotImplemented

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
        let final = "-" ^ (string_of_expr' (Some (ONeg, 0)) e) in
        (match ctxt with
        | None
        | Some (OAdd, 0) -> final
        | Some (ONeg, _)
        | Some (OAdd, _)
        | Some (OMul, _)
        | Some (ODiv, _) -> "(" ^ final ^ ")")
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

let string_of_rational (x: rational): string =
  raise NotImplemented

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
  | Neg e -> -.(eval e vals)
  | Add (es) when List.length es >= 2 -> List.fold_left (fun acc e -> acc +. eval e vals) 0.0 es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Mul (es) when List.length es >= 2 -> List.fold_left (fun acc e -> acc *. eval e vals) 1.0 es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      let denom = eval e2 vals in
      if denom = 0.0 then raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
      else let numer = eval e1 vals in
      numer /. denom

let eval_rational (e: expr) (vals: (string * rational) list): rational =
  raise NotImplemented

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
  | Neg e -> negate (simplify e)
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
    else Add addends
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
