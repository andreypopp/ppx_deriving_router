open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_router_common
open Expansion_helpers

let derive_path_name ctor =
  let name = ctor.pcd_name.txt in
  mangle (Prefix "path") name

let routes_name = mangle_type_decl (Suffix "routes")
let routes_name_lid = mangle_lid (Suffix "routes")
let handle_name = mangle_type_decl (Suffix "handle")
let packed_name = mangle_type_decl (Prefix "packed")
let handler_name = mangle_type_decl (Prefix "handler")
let packed_ctor_name = mangle_type_decl (Prefix "Packed")
let packed_ctor_name_lid = mangle_lid (Prefix "Packed")

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
  [%type: [%t result] Ppx_deriving_router_runtime.encode]

let derive_mount td m =
  let loc = m.m_ctor.pcd_loc in
  let name = derive_path_name m.m_ctor in
  let pat = pvar ~loc name in
  let expr =
    let routes =
      evar ~loc (Longident.name (routes_name_lid m.m_typ.txt))
    in
    let p inner =
      ppat_construct ~loc
        { loc; txt = packed_ctor_name_lid m.m_typ.txt }
        (Some inner)
    in
    let make =
      pexp_construct ~loc
        { loc; txt = Lident m.m_ctor.pcd_name.txt }
        (Some [%expr x])
    in
    let make_with_encode encode =
      pexp_construct ~loc
        { loc; txt = Lident (packed_ctor_name td) }
        (Some [%expr [%e make], [%e encode]])
    in
    let encode =
      match m.m_response with
      | `passthrough -> [%expr _encode]
      | `response -> [%expr Ppx_deriving_router_runtime.Encode_raw]
    in
    [%expr
      Stdlib.List.map
        (fun route ->
          let f f req =
            Lwt.bind (f req) (fun [%p p [%pat? x, _encode]] ->
                Lwt.return [%e make_with_encode encode])
          in
          Ppx_deriving_router_runtime.prefix_route
            [%e
              match m.m_prefix with
              | Some p -> [%expr Some [%e estring ~loc p]]
              | None -> [%expr None]]
            f route)
        [%e routes]]
  in
  value_binding ~loc ~pat ~expr

let derive_path td (exemplar, ctors) =
  let loc = exemplar.l_ctor.pcd_loc in
  let name = derive_path_name exemplar.l_ctor in
  let body =
    match exemplar.l_path with
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
      List.filter exemplar.l_path ~f:(function
        | Pparam _ -> true
        | Ppath _ -> false)
      |> List.mapi ~f:(fun idx _ -> Printf.sprintf "_param%d" idx)
    in
    let preq, req = patt_and_expr ~loc (gen_symbol ~prefix:"_req" ()) in
    let by_method =
      let init =
        [
          ppat_any ~loc
          --> [%expr raise Ppx_deriving_router_runtime.Method_not_allowed];
        ]
      in
      List.fold_left ctors ~init ~f:(fun cases leaf ->
          let loc = leaf.l_ctor.pcd_loc in
          let name = leaf.l_ctor.pcd_name.txt in
          let method_ = method_to_string leaf.l_method_ in
          let pat = ppat_variant ~loc method_ None in
          let lname = { loc; txt = Lident name } in
          let path_params =
            List.filter_map leaf.l_path ~f:(function
              | Pparam (name, _) -> Some name
              | Ppath _ -> None)
          in
          let args =
            List.map2 path_params params ~f:(fun name value ->
                { loc; txt = Lident name }, evar ~loc value)
          in
          let args =
            args
            @ List.filter_map leaf.l_query ~f:(fun (name, typ) ->
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
                            (Ppx_deriving_router_runtime
                             .Invalid_query_parameter
                               ([%e estring ~loc name], v))]
                  in
                  Some (field_name, value))
          in
          let to_response =
            match leaf.l_response with
            | `response -> [%expr Ppx_deriving_router_runtime.Encode_raw]
            | `json_response t ->
                [%expr
                  Ppx_deriving_router_runtime.Encode_json
                    [%to_json: [%t t]]]
          in
          let make args =
            let args =
              match args with
              | [] -> None
              | args -> Some (pexp_record ~loc args None)
            in
            let expr = pexp_construct ~loc lname args in
            pexp_construct ~loc
              { loc; txt = Lident (packed_ctor_name td) }
              (Some [%expr [%e expr], [%e to_response]])
          in
          (* handle body *)
          let pbody, ebody = patt_and_expr ~loc "_body" in
          let expr =
            match leaf.l_body with
            | None -> [%expr Lwt.return [%e make args]]
            | Some (name, body) ->
                let name = { loc; txt = Lident name } in
                let args = (name, ebody) :: args in
                [%expr
                  Lwt.bind
                    (Dream.body [%e req])
                    (fun [%p pbody] ->
                      let [%p pbody] =
                        try Yojson.Basic.from_string [%e ebody]
                        with Yojson.Json_error msg ->
                          raise
                            (Ppx_deriving_router_runtime.Invalid_body msg)
                      in
                      let [%p pbody] =
                        try [%of_json: [%t body]] [%e ebody]
                        with Yojson.Basic.Util.Type_error (msg, _) ->
                          raise
                            (Ppx_deriving_router_runtime.Invalid_body msg)
                      in
                      Lwt.return [%e make args])]
          in

          (pat --> expr) :: cases)
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
  let expr =
    [%expr
      [
        Ppx_deriving_router_runtime.Route
          ([%e body], [%e make], Stdlib.Fun.id);
      ]]
  in
  value_binding ~loc ~pat ~expr

let derive_routes td ctors mounts =
  let loc = td.ptype_loc in
  pexp_let ~loc Nonrecursive
    (let paths = List.rev_map ctors ~f:(derive_path td) in
     let mounts = List.rev_map mounts ~f:(derive_mount td) in
     paths @ mounts)
    (let loc = td.ptype_loc in
     let paths =
       List.map ctors ~f:(fun (ctor, _ctors) ->
           let name = derive_path_name ctor.l_ctor in
           let loc = ctor.l_ctor.pcd_loc in
           evar ~loc name)
     in
     let mounts =
       List.map mounts ~f:(fun m ->
           let name = derive_path_name m.m_ctor in
           let loc = m.m_ctor.pcd_loc in
           evar ~loc name)
     in
     [%expr Stdlib.List.flatten [%e elist ~loc (paths @ mounts)]])

let derive_router_td td =
  let param, ctors = extract td in
  let leafs, mounts =
    List.partition_filter_map ctors ~f:(function
      | Leaf x -> `Left x
      | Mount x -> `Right x)
  in
  let leafs =
    List.group_by ~eq:equal_route_by_path ~hash:hash_route_by_path leafs
    |> List.map ~f:(fun ctors ->
           let ctor = List.hd ctors in
           ctor, ctors)
  in
  let loc = td.ptype_loc in
  let packed_ty =
    ptyp_constr ~loc { loc; txt = Lident (packed_name td) } []
  in
  let packed_stri =
    pstr_type ~loc Recursive
      [
        type_declaration ~loc
          ~name:{ txt = packed_name td; loc }
          ~params:[] ~cstrs:[]
          ~kind:
            (Ptype_variant
               [
                 {
                   pcd_name = { txt = packed_ctor_name td; loc };
                   pcd_args =
                     Pcstr_tuple
                       [ td_to_ty param td; td_to_ty_enc param td ];
                   pcd_res = Some packed_ty;
                   pcd_loc = loc;
                   pcd_vars = [];
                   pcd_attributes = [];
                 };
               ])
          ~private_:Public ~manifest:None;
      ]
  in
  let handler_stri =
    pstr_type ~loc Recursive
      [
        type_declaration ~loc
          ~name:{ txt = handler_name td; loc }
          ~params:[] ~cstrs:[]
          ~kind:
            (Ptype_record
               [
                 {
                   pld_name = { txt = "f"; loc };
                   pld_mutable = Immutable;
                   pld_type = td_to_ty_handler param td;
                   pld_loc = loc;
                   pld_attributes = [];
                 };
               ])
          ~private_:Public ~manifest:None;
      ]
  in

  [%str
    [%%i Derive_href.derive td ctors]
    [%%i Derive_method.derive td ctors]
    [%%i packed_stri]
    [%%i handler_stri]

    let [%p pvar ~loc (routes_name td)] =
      [%e derive_routes td leafs mounts]

    let [%p pvar ~loc (handle_name td)] =
      let router =
        Ppx_deriving_router_runtime.make
          (let routes =
             Stdlib.List.map Ppx_deriving_router_runtime.to_route
               [%e evar ~loc (routes_name td)]
           in
           Routes.one_of routes)
      in
      fun ({ f } :
            [%t
              ptyp_constr ~loc { loc; txt = Lident (handler_name td) } []]) ->
        Ppx_deriving_router_runtime.handle router
          (fun
            [%p
              ppat_construct ~loc
                { loc; txt = Lident (packed_ctor_name td) }
                (Some [%pat? p, encode])]
            req
          ->
            Lwt.bind (f p req) (Ppx_deriving_router_runtime.encode encode))

    let [%p pvar ~loc (handle_name td)] =
      [%e
        match param with
        | Some _ -> evar ~loc (handle_name td)
        | None -> [%expr fun f -> [%e evar ~loc (handle_name td)] { f }]]]

let () = register () ~derive:derive_router_td
