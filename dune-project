(lang dune 3.11)

(using melange 0.1)

(generate_opam_files true)

(source
 (github andreypopp/ppx_deriving_router))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license MIT)

(package
 (name ppx_deriving_router)
 (synopsis "Derive typesafe router from type declarations")
 (depends
  (ocaml
   (>= 4.14))
  dune
  ppxlib
  containers
  routes
  melange
  melange-fetch
  ppx_deriving_json
  uri))

(package
 (name ppx_deriving_router_dream)
 (synopsis "Dream runtime")
 (depends
  (ocaml
   (>= 4.14))
  dune
  routes
  dream
  ppx_deriving_router
  ppx_deriving_json
  uri))
