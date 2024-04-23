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
            [%t td_to_ty param td] ->
            Fetch.Response.t ->
            Fetch.Response.t Js.Promise.t =
         fun _route response -> Js.Promise.resolve response]
  | Some _ ->
      let cases =
        List.map ctors ~f:(function
          | Mount
              {
                m_ctor;
                m_prefix = _;
                m_typ;
                m_typ_param = _;
                m_response = _;
              } ->
              let p, x = match_ctor m_ctor in
              let next =
                mangle_lid (Suffix "decode_response") m_typ.txt
              in
              let next = evar ~loc (Longident.name next) in
              p --> [%expr [%e next] [%e x] response]
          | Leaf ctor ->
              let arg =
                match ctor.ctor.pcd_args with
                | Pcstr_tuple [] -> None
                | _ -> Some (ppat_any ~loc)
              in
              let p = pat_ctor ctor.ctor arg in
              let e =
                match ctor.response with
                | `response -> [%expr Js.Promise.resolve response]
                | `json_response t ->
                    [%expr
                      Fetch.Response.json response
                      |> Js.Promise.then_ (fun json ->
                             Js.Promise.resolve ([%of_json: [%t t]] json))]
              in
              p --> e)
      in
      let t =
        td_to_ty
          (Some (ptyp_constr ~loc { loc; txt = Longident.parse "a" } []))
          td
      in
      [%stri
        let [%p pvar ~loc name] =
         fun (type a) (route : [%t t]) (response : Fetch.Response.t) :
             a Js.Promise.t ->
          [%e pexp_match ~loc [%expr route] cases]]

let derive_router_td td =
  let _param, ctors = extract td in
  [
    Derive_href.derive td ctors;
    Derive_method.derive td ctors;
    derive_decode_response td _param ctors;
  ]

let () = register () ~derive:derive_router_td
