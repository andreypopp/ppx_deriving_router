open! ContainersLabels
open! Ppxlib
open! Ast_builder.Default
open Ppx_router_common

let derive_router_td td =
  let _param, ctors = extract td in
  [ Derive_href.derive td ctors ]

let _ : Deriving.t = register derive_router_td
