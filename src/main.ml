open Printf
open Math_expr

(* tabulate origin dest returns the list [origin; origin + 1; ...; dest - 1; dest] *)
let rec tabulate (origin: int) (dest: int): int list =
  if origin = dest then [dest]
  else origin :: (tabulate (origin + 1) dest)

(* Command-line argument parsing ------------------------------------------------------------------------------------ *)
let usage_msg = 
  "Usage: math_practice [<options>] <subcommand> [<subcommand-options>]\n\
  \       Valid subcommands:\n\
  \       - arithmetic  Practice order of operations and basic arithmetic\n"

(* Global options *)
let quiet = ref false
let num_questions = ref 10

let speclist = ref [
  ("-q", Arg.Set quiet, "Do not print seed or question numbers");
  ("--quiet", Arg.Set quiet, "Do not print seed or question numbers");
  ("-n", Arg.Set_int num_questions, "Number of questions to generate");
  ("--num-questions", Arg.Set_int num_questions, "Number of questions to generate");
]
let anon_args = ref []
let subcommand = ref None
let set_subcommand (arg: string): unit =
  match arg with
  | "arithmetic" ->
      subcommand := Some "arithmetic"
  | _ -> raise (Arg.Bad (sprintf "Invalid subcommand %s" arg))

let anon_fun arg =
  match !subcommand with
  | None -> set_subcommand arg
  | Some _ -> anon_args := arg :: !anon_args

let print_error (error_msg: string): unit =
  eprintf "%s.\n" error_msg;
  Arg.usage !speclist usage_msg;
  exit 1

(* Subcommands ------------------------------------------------------------------------------------------------------ *)
let arithmetic quiet num_questions: unit =
  let f quiet n =
    if not quiet then (printf "%d. " n) else ();
    printf "%s\n" (string_of_expr (next_rand 1 2 2 (-99) 100))
  in
  Random.self_init ();
  (* 1073741823 = 2^30 - 1 *)
  let s = Random.int 1073741823 in
  if not quiet then (printf "Seed: %d\n" s) else ();
  seed s;
  List.iter (f quiet) (tabulate 1 num_questions)

let invoke_subcommand (): unit =
  let quiet = !quiet in
  let num_questions = !num_questions in
  if num_questions < 0 then print_error (sprintf "Invalid number of questions %d" num_questions);
  match !subcommand with
  | Some "arithmetic" -> arithmetic quiet num_questions
  | Some s -> print_error (sprintf "Invalid subcommand %s" s)
  | None -> print_error (sprintf "No subcommand provided")

let () =
  Arg.parse_dynamic speclist anon_fun usage_msg;
  invoke_subcommand ()
