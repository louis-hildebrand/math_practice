open Printf
open Rational

(* Types ------------------------------------------------------------------------------------------------------------ *)
type expr =
  | Z of int           (* Integer *)
  | R of float         (* Real number *)
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

(* Exceptions ------------------------------------------------------------------------------------------------------- *)
exception InvalidExpr of string
exception Undefined of string
exception UndefinedVariable of string
exception MultipleDefinitions of string

(* Helper functions ------------------------------------------------------------------------------------------------- *)
(* TODO: Add 'distribute' flag to allow for distributing negative sign to arguments in Neg (Add es) *)
let rec flatten (e: expr): expr =
  match e with
  | Z _
  | R _
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

(* Remove the trailing period (e.g. show 3.0 as 3 instead of 3.) *)
let string_of_float = sprintf "%.12g"

(* Public functions ------------------------------------------------------------------------------------------------- *)
let string_of_expr (e: expr): string =
  let rec string_of_expr' (ctxt: expr_context option) (e: expr): string =
    match e with
    | Z (n) ->
        if n < 0 && (ctxt != None && ctxt != Some (OAdd, 0)) then
          "(" ^ (string_of_int n) ^ ")"
        else
          string_of_int n
    | R x ->
        if x < 0.0 && (ctxt != None && ctxt != Some (OAdd, 0)) then
          "(" ^ (string_of_float x) ^ ")"
        else
          string_of_float x
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
            | R x when x < 0.0 -> acc ^ " - " ^ (string_of_float (-.x))
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
  | R x -> x
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

let rec eval_rational (e: expr) (vals: (string * rational) list): rational =
  match e with
  | Z n -> new_rational n 1
  | R x -> raise (NonRational (sprintf "Floating-point value %g is not an integer or a fraction." x))
  | Var name -> 
      let vs = List.map (fun (_, v) -> v) (List.filter (fun (n, _) -> n = name) vals) in
      (match vs with
      | [] -> raise (UndefinedVariable (sprintf "No definition provided for variable '%s'." name))
      | [v] -> v
      | v1::v2::_ ->
          raise (MultipleDefinitions (sprintf "Multiple definitions provided for variable '%s' (e.g. %s, %s)."
            name (string_of_rational v1) (string_of_rational v2))))
  | Neg e' -> ~-:(eval_rational e' vals)
  | Add es when List.length es >= 2 ->
      List.fold_left (fun acc e -> acc +: (eval_rational e vals)) (new_rational 0 1) es
  | Add _ -> raise (InvalidExpr "Wrong number of arguments for operation Add.")
  | Mul es when List.length es >= 2 ->
      List.fold_left (fun acc e -> acc *: (eval_rational e vals)) (new_rational 1 1) es
  | Mul _ -> raise (InvalidExpr "Wrong number of arguments for operation Mul.")
  | Div (e1, e2) ->
      let n = eval_rational e1 vals in
      let d = eval_rational e2 vals in
      if d =: (new_rational 0 1) then
        raise (Undefined (sprintf "Attempt to divide by zero in expression %s." (string_of_expr e)))
      else
        n /: d
