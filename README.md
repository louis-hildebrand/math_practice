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
