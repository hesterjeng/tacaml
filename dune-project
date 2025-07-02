(lang dune 3.17)
(using ctypes 0.3)

(generate_opam_files true)

(authors "John Hester")

(maintainers "John Hester")

(name tacaml)

(source
 (github username/reponame))

(package
 (name tacaml)
 (allow_empty)
 (synopsis "Bindings to TA-lib")
 (license "GPL-3.0-or-later")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
