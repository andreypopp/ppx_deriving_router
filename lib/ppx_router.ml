open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_router_common
open Expansion_helpers

let derive_path_name (ctor : ctor) =
  let name = ctor.ctor.pcd_name.txt in
  mangle (Prefix "path") name

let td_to_ty param td =
  let name = td.ptype_name.txt in
  let loc = td.ptype_loc in
  let args = match param with Some param -> [ param ] | None -> [] in
  ptyp_constr ~loc (Located.lident ~loc name) args

let td_to_ty_handler param td =
  let loc = td.ptype_loc in
  match param with
  | Some param ->
      let param_name =
        match param.ptyp_desc with
        | Ptyp_var name -> name
        | _ -> assert false
      in
      ptyp_poly ~loc
        [ { loc; txt = param_name } ]
        [%type:
          [%t td_to_ty (Some param) td] ->
          Dream.request ->
          [%t param] Lwt.t]
  | None ->
      [%type:
        [%t td_to_ty param td] -> Dream.request -> Dream.response Lwt.t]

let td_to_ty_enc param td =
  let loc = td.ptype_loc in
  let result =
    match param with
    | None -> [%type: Dream.response]
    | Some param -> param
  in
  [%type: [%t result] -> Dream.response Lwt.t]

let derive_path (ctor, ctors) =
  let loc = ctor.ctor.pcd_loc in
  let name = derive_path_name ctor in
  let body =
    match ctor.path with
    | [] -> [%expr Routes.nil]
    | init :: params ->
        let body =
          let f = function
            | Pparam (name, ty) ->
                let to_url = derive_conv "to_url_path" ty in
                let of_url = derive_conv "of_url_path" ty in
                [%expr
                  Routes.pattern [%e to_url] [%e of_url]
                    [%e estring ~loc name]]
            | Ppath path -> [%expr Routes.s [%e estring ~loc path]]
          in
          List.fold_left params ~init:(f init) ~f:(fun body param ->
              let param = f param in
              [%expr Routes.( / ) [%e body] [%e param]])
        in
        [%expr Routes.( /? ) [%e body] Routes.nil]
  in
  let make =
    let params =
      List.filter ctor.path ~f:(function
        | Pparam _ -> true
        | Ppath _ -> false)
      |> List.mapi ~f:(fun idx _ -> Printf.sprintf "_param%d" idx)
    in
    let preq, req = patt_and_expr ~loc (gen_symbol ~prefix:"_req" ()) in
    let by_method =
      let init =
        [
          ppat_any ~loc
          --> [%expr raise Ppx_router_runtime.Method_not_allowed];
        ]
      in
      List.fold_left ctors ~init ~f:(fun cases ctor ->
          let loc = ctor.ctor.pcd_loc in
          let name = ctor.ctor.pcd_name.txt in
          let method_ = method_to_string ctor.method_ in
          let pat = ppat_variant ~loc method_ None in
          let lname = { loc; txt = Lident name } in
          let path_params =
            List.filter_map ctor.path ~f:(function
              | Pparam (name, _) -> Some name
              | Ppath _ -> None)
          in
          let args =
            List.map2 path_params params ~f:(fun name value ->
                { loc; txt = Lident name }, evar ~loc value)
          in
          let args =
            args
            @ List.filter_map ctor.query ~f:(fun (name, typ) ->
                  let field_name = { loc; txt = Lident name } in
                  let of_url = derive_conv "of_url_query" typ in
                  let value =
                    [%expr
                      let v =
                        Dream.queries [%e req] [%e estring ~loc name]
                      in
                      match [%e of_url] v with
                      | Some v -> v
                      | None ->
                          raise
                            (Ppx_router_runtime.Invalid_query_parameter
                               ([%e estring ~loc name], v))]
                  in
                  Some (field_name, value))
          in
          let args =
            match args with
            | [] -> None
            | args -> Some (pexp_record ~loc args None)
          in
          let expr = pexp_construct ~loc lname args in
          let to_response =
            match ctor.response with
            | None -> [%expr Lwt.return]
            | Some [%type: Dream.response] -> [%expr Lwt.return]
            | Some t ->
                [%expr
                  fun v ->
                    Dream.json
                      (Yojson.Basic.to_string ([%to_json: [%t t]] v))]
          in
          (pat --> [%expr P ([%e expr], [%e to_response])]) :: cases)
    in
    let make =
      [%expr
        fun ([%p preq] : Dream.request) ->
          [%e pexp_match ~loc [%expr Dream.method_ [%e req]] by_method]]
    in
    List.fold_left (List.rev params) ~init:make ~f:(fun body param ->
        pexp_fun ~loc Nolabel None (pvar ~loc param) body)
  in
  let pat = pvar ~loc name in
  let expr = [%expr Routes.route [%e body] [%e make]] in
  value_binding ~loc ~pat ~expr

let derive_router td ctors =
  let loc = td.ptype_loc in
  let paths = List.rev_map ctors ~f:derive_path in
  pexp_let ~loc Nonrecursive paths
    (let loc = td.ptype_loc in
     let paths =
       List.map ctors ~f:(fun (ctor, _ctors) ->
           let name = derive_path_name ctor in
           let loc = ctor.ctor.pcd_loc in
           evar ~loc name)
     in
     [%expr Ppx_router_runtime.make (Routes.one_of [%e elist ~loc paths])])

let derive_router_td td =
  let param, ctors = extract td in
  let ctors_by_path =
    List.group_by ~eq:equal_route_by_path ~hash:hash_route_by_path ctors
    |> List.map ~f:(fun ctors ->
           let ctor = List.hd ctors in
           ctor, ctors)
  in
  let loc = td.ptype_loc in
  let router_name = mangle_type_decl (Suffix "router") td in
  let handle_name = mangle_type_decl (Suffix "handle") td in
  Derive_href.derive td ctors
  :: [%str
       type handler = { f : [%t td_to_ty_handler param td] }

       let [%p pvar ~loc handle_name] =
         let open struct
           type p =
             | P :
                 [%t td_to_ty param td] * [%t td_to_ty_enc param td]
                 -> p

           let [%p pvar ~loc router_name] =
             [%e derive_router td ctors_by_path]
         end in
         fun ({ f } : handler) ->
           Ppx_router_runtime.handle [%e evar ~loc router_name]
             (fun (P (p, to_response)) req ->
               Lwt.bind (f p req) to_response)

       let [%p pvar ~loc handle_name] =
         [%e
           match param with
           | Some _ -> evar ~loc handle_name
           | None -> [%expr fun f -> [%e evar ~loc handle_name] { f }]]]

let _ : Deriving.t = register derive_router_td
