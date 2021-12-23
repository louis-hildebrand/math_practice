(lang dune 2.9)

; Don't forget to regenerate opam files by running `dune build`
(generate_opam_files true)

(name math_practice)
(version 1.0.0)

(package
 (name math_practice)
 (authors "Louis Hildebrand <louis.hildebrand@mail.mcgill.ca>")
 (maintainers "Louis Hildebrand <louis.hildebrand@mail.mcgill.ca>")
 (homepage https://github.com/louis-hildebrand/math_practice)
 (bug_reports https://github.com/louis-hildebrand/math_practice/issues)
 (synopsis "A command-line tool to generate math exercises")
 (depends
   (ounit2 (>= 2.2.4))
   (bisect_ppx (>= 2.7.0))
  ))
