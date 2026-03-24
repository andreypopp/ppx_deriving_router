(lang dune 3.15)

(name ppx_deriving_router)

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
 (depopts dream http cohttp cohttp-lwt cohttp-lwt-unix)
 (depends
  (ocaml
   (>= 4.14))
  dune
  lwt
  (ppxlib (>= 0.36))
  (containers (>= 3.18))
  routes
  melange
  melange-fetch
  melange-json
  melange-json-native
  uri))
