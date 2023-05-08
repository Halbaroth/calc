(lang dune 3.7)

(name calc)

(generate_opam_files true)

(source
 (github Halbaroth/calc))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name calc)
 (synopsis "A naive calculator in OCaml")
 (description "A naive calculator in OCaml")
 (depends
   ocaml
   dune
   ppx_inline_test)
 (tags
  (topics "to describe" your project)))
