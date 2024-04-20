open! ContainersLabels
open! Ppxlib
open! Ast_builder.Default
open Ppx_router_common
open Expansion_helpers

let derive_decode_response td param ctors =
  let loc = td.ptype_loc in
  let name = mangle_type_decl (Suffix "decode_response") td in
  match param with
  | None ->
      [%stri
        let [%p pvar ~loc name] :
            [%t td_to_ty param td] -> string -> string =
         fun _route response -> response]
  | Some _ ->
      let cases =
        List.map ctors ~f:(fun ctor ->
            let lid = { txt = Lident ctor.ctor.pcd_name.txt; loc } in
            let arg =
              match ctor.ctor.pcd_args with
              | Pcstr_tuple [] -> None
              | _ -> Some (ppat_any ~loc)
            in
            let p = ppat_construct ~loc lid arg in
            let e =
              match ctor.response with
              | None | Some [%type: Dream.response] -> [%expr data]
              | Some t ->
                  [%expr
                    let json = Js.Json.parseExn data in
                    [%of_json: [%t t]] json]
            in
            p --> e)
      in
      [%stri
        let [%p pvar ~loc name] =
         fun (type a) (route : a t) (data : string) : a ->
          [%e pexp_match ~loc [%expr route] cases]]

let derive_router_td td =
  let _param, ctors = extract td in
  [ Derive_href.derive td ctors; derive_decode_response td _param ctors ]

let _ : Deriving.t = register derive_router_td
