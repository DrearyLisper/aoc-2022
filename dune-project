(lang dune 3.6)

(name aoc-2022)

(generate_opam_files true)

(source
 (github ld86/aoc-2022))

(authors "Dreary Lisper")

(maintainers "Dreary Lisper")

(license MIT)

(package
 (name aoc-2022)
 (synopsis "Advent Of Code 2022 in OCaml")
 (description "Solving Advent of Code 2022 in OCaml")
 (depends ocaml dune core)
 (tags
  (aoc aoc-2022 ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
