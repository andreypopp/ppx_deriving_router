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
                match ctor.l_ctor.pcd_args with
                | Pcstr_tuple [] -> None
                | _ -> Some (ppat_any ~loc)
              in
              let p = pat_ctor ctor.l_ctor arg in
              let e =
                match ctor.l_response with
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

let derive_body td routes =
  let loc = td.ptype_loc in
  let name = td.ptype_name.txt in
  let t, we = td_newtype td [%type: string option] in
  let name = mangle (Suffix "body") name in
  let cases =
    List.map routes ~f:(function
      | Mount m ->
          let loc = m.m_ctor.pcd_loc in
          let p, x = match_ctor m.m_ctor in
          let f =
            evar ~loc
              (Longident.name (mangle_lid (Suffix "body") m.m_typ.txt))
          in
          p --> [%expr [%e f] [%e x]]
      | Leaf c ->
          let loc = c.l_ctor.pcd_loc in
          let p, x = match_ctor c.l_ctor in
          let e =
            match c.l_body with
            | None -> [%expr None]
            | Some (name, typ) ->
                let body = pexp_field ~loc x { loc; txt = Lident name } in
                [%expr
                  let json = [%to_json: [%t typ]] [%e body] in
                  let body = Js.Json.stringify json in
                  Some body]
          in

          p --> e)
  in
  let e = we [%expr ([%e pexp_function ~loc cases] : [%t t])] in
  [%stri let [%p pvar ~loc name] = [%e e]]

let derive_router_td td =
  let _param, ctors = extract td in
  [
    Derive_href.derive td ctors;
    Derive_method.derive td ctors;
    derive_body td ctors;
    derive_decode_response td _param ctors;
  ]

let () = register () ~derive:derive_router_td
