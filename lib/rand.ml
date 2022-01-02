open Base
open Rational

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

(* TODO: Remove *)
exception NotImplemented
let next_fractional (min_depth: int) (max_depth: int) (width: int) (min_const: rational) (max_const: rational)
    (max_denom: int): expr =
  raise NotImplemented
