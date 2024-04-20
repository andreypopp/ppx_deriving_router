(lang dune 3.11)

(using melange 0.1)

(generate_opam_files true)

(source
 (github andreypopp/ppx_router))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(package
 (name ppx_router)
 (depends
  (ocaml
   (>= 4.14))
  dune
  ppxlib
  containers
  routes
  dream
  melange
  ppx_deriving_json
  uri))
