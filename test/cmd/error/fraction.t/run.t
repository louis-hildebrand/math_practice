Missing seed when asking for answers
  $ math_practice fraction -a
  Seed is required when viewing answers. Provide a seed using the -s option.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ math_practice fraction --answers
  Seed is required when viewing answers. Provide a seed using the -s option.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

Number of questions invalid
  $ math_practice fraction -n 0
  Invalid number of questions 0. The number of questions must be at least 1.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ math_practice fraction --num-questions 0
  Invalid number of questions 0. The number of questions must be at least 1.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ math_practice fraction -n -1
  Invalid number of questions -1. The number of questions must be at least 1.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ math_practice fraction --num-questions -1
  Invalid number of questions -1. The number of questions must be at least 1.
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]

Extra anonymous arguments
  $ math_practice fraction -n 5 extra args -q
  Unrecognized argument(s) [args,extra].
  Usage: math_practice [<options>] <subcommand> [<subcommand-options>]
         Valid subcommands:
         - fraction  Practice order of operations and basic arithmetic with fractions
         - decimal   Practice order of operations and basic arithmetic with decimal numbers
  
    -a Show answers to previous questions
    --answers Show answers to previous questions
    -n Number of questions to generate
    --num-questions Number of questions to generate
    -q Do not print seed or question numbers
    --quiet Do not print seed or question numbers
    -s Seed for the random number generator
    --seed Seed for the random number generator
    -help  Display this list of options
    --help  Display this list of options
  [1]
