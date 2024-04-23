(lang dune 3.11)

(using melange 0.1)

(generate_opam_files true)

(source
 (github andreypopp/ppx_deriving_router))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(package
 (name ppx_deriving_router)
 (depends
  (ocaml
   (>= 4.14))
  dune
  ppxlib
  containers
  routes
  dream
  melange
  melange-fetch
  ppx_deriving_json
  uri))
