# math_practice
![Build](https://github.com/louis-hildebrand/math_practice/actions/workflows/main.yml/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/louis-hildebrand/math_practice/badge.svg?branch=main)](https://coveralls.io/github/louis-hildebrand/math_practice?branch=main)

`math_practice` is a command-line tool to generate math exercises.

## Manual installation
#### Prerequisites
- opam, the OCaml package manager: https://opam.ocaml.org/doc/Install.html
    - `math_practice` requires at least OCaml 4.05.0. Check your version using `opam switch`. If your OCaml compiler is out of date, you can install a newer one using `opam switch create <version>` (e.g. `opam switch create 4.05.0`).
- dune, the OCaml build system: https://dune.build/install
    - It is easiest to install dune using opam.

#### Steps
1. Download and extract the [latest release](https://github.com/louis-hildebrand/math_practice/releases/latest).
2. From the root of the `math_practice` repository, run the command `dune build`.
3. Locate the executable at _build/default/src/math_practice.exe.

## Usage
The examples below assume the executable is named `math_practice`. You may need to rename it to remove the `.exe` extension that is automatically added by dune.

General usage information can be viewed using `math_practice --help`.

The following subcommands are available:
- `fraction` generates arithmetic questions involving fractions.
- `decimal` generates arithmetic questions involving decimal numbers.

The following global options are available:
- `-a`, `--answers` reveals the answer to each question. A seed must be specified if this option is used.
- `-n`, `--num-questions` sets the number of questions generated. Minimum 1.
- `-q`, `--quiet` suppresses unnecessary output. For example, the question numbers and seed will not be displayed.
- `-s`, `--seed` sets the seed for the random number generator.
- `-h`, `--help` displays the command usage.

### Examples
#### fraction
Generate 15 random questions with minimal output.
```sh
$ math_practice fraction -n 15 -q
```

Show the answers to a previously generated set of questions whose seed was 42.
```sh
$ math_practice fraction -a -s 42
```

#### decimal
Generate 15 random questions with minimal output.
```sh
$ math_practice decimal -n 15 -q
```

Show the answers to a previously generated set of questions whose seed was 42.
```sh
$ math_practice decimal -a -s 42
```
