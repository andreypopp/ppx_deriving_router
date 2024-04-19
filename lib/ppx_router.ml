open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_router_common

let derive_path_name (ctor : ctor) =
  let name = ctor.ctor.pcd_name.txt in
  Ppxlib.Expansion_helpers.mangle (Prefix "path") name

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
          (pat --> [%expr [%e expr]]) :: cases)
    in
    let make =
      [%expr
        fun ([%p preq] : Dream.request) ->
          [%e pexp_match ~loc [%expr Dream.method_ [%e req]] by_method]]
    in
    List.fold_left (List.rev params) ~init:make ~f:(fun body param ->
        pexp_fun ~loc Nolabel None (pvar ~loc param) body)
  in
  let body = [%expr Routes.route [%e body] [%e make]] in
  [%stri let [%p pvar ~loc name] = [%e body]]

let derive_router td ctors =
  let loc = td.ptype_loc in
  let name = td.ptype_name.txt in
  let name = Ppxlib.Expansion_helpers.mangle (Suffix "router") name in
  let paths =
    List.map ctors ~f:(fun (ctor, _ctors) ->
        let name = derive_path_name ctor in
        let loc = ctor.ctor.pcd_loc in
        evar ~loc name)
  in
  [%stri
    let [%p pvar ~loc name] =
      Ppx_router_runtime.make (Routes.one_of [%e elist ~loc paths])]

let derive_handle td =
  let loc = td.ptype_loc in
  let name = td.ptype_name.txt in
  let router = Ppxlib.Expansion_helpers.mangle (Suffix "router") name in
  let name = Ppxlib.Expansion_helpers.mangle (Suffix "handle") name in
  [%stri
    let [%p pvar ~loc name] =
      Ppx_router_runtime.handle [%e evar ~loc router]]

let derive_router_td td =
  let loc = td.ptype_loc in
  let ctors = extract td in
  let ctors_by_path =
    List.group_by ~eq:equal_route_by_path ~hash:hash_route_by_path ctors
    |> List.map ~f:(fun ctors ->
           let ctor = List.hd ctors in
           ctor, ctors)
  in
  let paths = List.rev_map ctors_by_path ~f:derive_path in
  let paths =
    pstr_open ~loc
      {
        popen_override = Fresh;
        popen_expr = pmod_structure ~loc paths;
        popen_loc = loc;
        popen_attributes = [];
      }
  in
  paths
  :: [
       derive_router td ctors_by_path;
       derive_handle td;
       Derive_href.derive td ctors;
     ]

let _ : Deriving.t = register derive_router_td
