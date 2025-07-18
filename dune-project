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
 (synopsis "Bindings to TA-lib")
 (license "GPL-3.0-or-later")
 (homepage "https://github.com/hesterjeng/longleaf")
 (description "A longer description")
 (bug_reports "Github Issue")
 (depends ocaml ctypes containers)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
