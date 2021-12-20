open Printf
open Math_expr

(* tabulate origin dest returns the list [origin; origin + 1; ...; dest - 1; dest] *)
let rec tabulate (origin: int) (dest: int): int list =
  if origin = dest then [dest]
  else origin :: (tabulate (origin + 1) dest)

let repeat (x: 'a) (n: int): 'a list =
  let rec repeat' n acc =
    if n = 0 then acc
    else repeat' (n - 1) (x :: acc)
  in
  repeat' n []

(* Command-line argument parsing ------------------------------------------------------------------------------------ *)
let usage_msg = 
  "Usage: math_practice [<options>] <subcommand> [<subcommand-options>]\n\
  \       Valid subcommands:\n\
  \       - arithmetic  Practice order of operations and basic arithmetic\n"

(* Global options *)
let quiet = ref false
let num_questions = ref 10
let user_seed = ref None
let show_answers = ref false

let speclist = ref [
  ("-a", Arg.Set show_answers, "Show answers to previous questions");
  ("--answers", Arg.Set show_answers, "Show answers to previous questions");
  ("-n", Arg.Set_int num_questions, "Number of questions to generate");
  ("--num-questions", Arg.Set_int num_questions, "Number of questions to generate");
  ("-q", Arg.Set quiet, "Do not print seed or question numbers");
  ("--quiet", Arg.Set quiet, "Do not print seed or question numbers");
  ("-s", Arg.Int (fun s -> user_seed := Some s), "Seed for the random number generator");
  ("--seed", Arg.Int (fun s -> user_seed := Some s), "Seed for the random number generator");
]

let print_error (error_msg: string): 'a =
  eprintf "%s.\n" error_msg;
  Arg.usage !speclist usage_msg;
  exit 1

let anon_args = ref []

let subcommand = ref None
let set_subcommand (arg: string): unit =
  match arg with
  | "arithmetic" ->
      subcommand := Some "arithmetic"
  | _ -> print_error (sprintf "Invalid subcommand %s" arg)

let anon_fun arg =
  match !subcommand with
  | None -> set_subcommand arg
  | Some _ -> anon_args := arg :: !anon_args

(* Subcommands ------------------------------------------------------------------------------------------------------ *)
let generate_arithmetic_questions (num_questions: int) (sd: int): expr list =
  seed sd;
  List.map (fun () -> next_rand 1 1 2 (-99) 100) (repeat () num_questions)

let ask_arithmetic quiet num_questions sd: unit =
  let print_question n e =
    if not quiet then (printf "%d. " n) else ();
    printf "%s\n" (string_of_expr e)
  in
  if not quiet then (printf "Seed: %d\n" sd) else ();
  let questions = generate_arithmetic_questions num_questions sd in
  let n = ref 1 in
  List.iter (fun e -> print_question !n e; n := !n + 1) questions

let answer_arithmetic (quiet: bool) (num_questions: int) (sd: int): unit =
  let print_answer n e =
    if not quiet then (printf "%d. %s = " n (string_of_expr e)) else ();
    printf "%f\n" (eval e)
  in
  let questions = generate_arithmetic_questions num_questions sd in
  let n = ref 1 in
  List.iter (fun e -> print_answer !n e; n := !n + 1) questions

let invoke_subcommand (): unit =
  let quiet = !quiet in
  let num_questions = !num_questions in
  if num_questions < 0 then print_error (sprintf "Invalid number of questions %d" num_questions);
  let show_answers = !show_answers in
  let seed = match !user_seed, show_answers with
    (* 1073741823 = 2^30 - 1, the maximum allowable bound for Random.int *)
    | None, false -> Random.self_init (); Random.int 1073741823
    | None, true -> print_error ("Seed is required when viewing answers. Provide a seed using the -s option")
    | Some s, _ -> s
  in
  match !subcommand, show_answers with
  | Some "arithmetic", false -> ask_arithmetic quiet num_questions seed
  | Some "arithmetic", true -> answer_arithmetic quiet num_questions seed
  | Some s, _ -> print_error (sprintf "Invalid subcommand %s" s)
  | None, _ -> print_error (sprintf "No subcommand provided")

let () =
  Arg.parse_dynamic speclist anon_fun usage_msg;
  invoke_subcommand ()
