(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system dune)
             ((guix licenses) #:prefix license:)
             (gnu packages ocaml)
             (gnu packages pkg-config)
             (guix gexp)
             (gnu packages finance)
             (gnu packages maths))

(package
  (name "tacaml")
  (version "1.0.0")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/hesterjeng/tacaml")
                  (commit "HEAD")))
            (file-name (git-file-name name version))
            (sha256
             (base32
	      "0d45rjr2rc39i53fdsczd7pxw5rb5i2a2vcjd0k08vrjxac5ys0s"
	      ))))
  (build-system dune-build-system)
  (arguments
   `(#:test-target "."))
  ;; (native-inputs
  ;;  (list ocaml-odoc))
  (propagated-inputs
   (list ocaml
         dune
	 ocaml-containers
         ocaml-ctypes
         ocaml-ppx-deriving
         ocaml-ppx-hash
         ta-lib
         pkg-config))
  (home-page "https://github.com/hesterjeng/tacaml")
  (synopsis "OCaml bindings for TA-Lib technical analysis library")
  (description
   "tacaml provides OCaml bindings to the TA-Lib (Technical Analysis Library).
This project offers both raw C bindings and higher-level, type-safe wrappers
for over 160 technical analysis functions commonly used in financial markets.
Features include comprehensive bindings, type safety with GADTs, efficient
data handling with Bigarray integration, modular design, and robust error
handling with Result types.")
  (license license:gpl3+))
