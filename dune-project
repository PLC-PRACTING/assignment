(lang dune 3.14)

(name toycc)

(generate_opam_files true)

(package
 (name toycc)
 (synopsis "A simple compiler for the ToyC language")
 (description "A compiler for a subset of C, targeting RISC-V assembly.")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  (menhir (>= 20220210))))