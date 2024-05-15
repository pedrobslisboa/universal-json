(lang dune 3.8)

(using melange 0.1)

(name universal-json)

(license MIT)

(maintainers "Pedro B S Lisboa <pedrobslisboa@gmail.com>")

(authors "Pedro B S Lisboa <pedrobslisboa@gmail.com>")

(source
 (github pedrobslisboa/universal-json))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name universal-json)
 (synopsis "Universal Json for ssr natively ReasonReact apps")
 (depends
  (ocaml
   (>= 5.0.0))
  (yojson (>= 2.1.2))
  ;; Test dependencies
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (fmt :with-test)
  ;; Dev dependencies
  (ocamlformat
   (and
    (= 0.26.1)
    :with-test))
  (melange-testing-library :with-test)
  (melange-jest :with-test)
  (ocaml-lsp-server :with-test)))
