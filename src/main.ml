open Printf

(* Command-line argument parsing ------------------------------------------------------------------------------------ *)
let usage_msg = "math_practice [-s | --silent] <subcommand> <options>"
let silent = ref false
let speclist = ref [
  ("-s", Arg.Set silent, "Do not print seed");
  ("--silent", Arg.Set silent, "Do not print seed");
]
let anon_args = ref []
let subcommand = ref None
let set_subcommand (arg: string): unit =
  match arg with
  | "order" -> subcommand := Some arg
  | _ -> raise (Arg.Bad (sprintf "Invalid subcommand %s" arg))

let anon_fun arg =
  match !subcommand with
  | None -> set_subcommand arg
  | Some _ -> anon_args := arg :: !anon_args

(* Subcommands ------------------------------------------------------------------------------------------------------ *)
let order_of_operations (): unit =
  (printf "Subcommand: order; silent: %B\n" !silent)

let invoke_subcommand (): unit =
  match !subcommand with
  | Some "order" -> order_of_operations ()
  | Some s -> raise (Arg.Bad (sprintf "Invalid subcommand %s" s))
  | None -> raise (Arg.Bad (sprintf "No subcommand provided"))

let () =
  Arg.parse_dynamic speclist anon_fun usage_msg;
  invoke_subcommand ()
