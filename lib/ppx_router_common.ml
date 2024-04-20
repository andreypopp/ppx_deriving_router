open ContainersLabels
open Ppxlib
open Ast_builder.Default

let patt_and_expr ~loc label = pvar ~loc label, evar ~loc label
let ( --> ) pc_lhs pc_rhs = { pc_lhs; pc_rhs; pc_guard = None }

type method_ = [ `GET | `POST | `PUT | `DELETE ]

let method_to_string : method_ -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"

let collect_params_rev ~loc:_ uri =
  let rec aux acc = function
    | [] -> acc
    | "" :: xs -> aux acc xs
    | x :: xs -> (
        match String.chop_prefix x ~pre:":" with
        | None -> aux (`path x :: acc) xs
        | Some name -> aux (`param name :: acc) xs)
  in
  aux [] (Uri.path uri |> String.split_on_char ~by:'/')

type ctor = {
  ctor : constructor_declaration;
  method_ : method_;
  path : path;
  query : (string * core_type) list;
  response : core_type option;
}

and path = path_segment list
and path_segment = Ppath of string | Pparam of string * core_type

let equal_path : path Equal.t =
  let eq_param a b =
    match a, b with
    | Ppath a, Ppath b -> String.equal a b
    | Pparam _, Pparam _ -> true
    | _ -> false
  in
  Equal.list eq_param

let equal_route_by_path_method : ctor Equal.t =
 fun a b -> Equal.poly a.method_ b.method_ && equal_path a.path b.path

let equal_route_by_path : ctor Equal.t =
 fun a b -> equal_path a.path b.path

let hash_route_by_path : ctor Hash.t =
 fun ctor ->
  Hash.list
    (function
      | Pparam _ -> 0 | Ppath x -> Hash.combine2 1 (Hash.string x))
    ctor.path

let declare_router_attr method_ =
  let name = Printf.sprintf "router.%s" (method_to_string method_) in
  let pattern =
    let open Ast_pattern in
    single_expr_payload (map (estring __') ~f:(fun _ x -> Some x))
    ||| map (pstr nil) ~f:(fun _ -> None)
  in
  ( method_,
    Attribute.declare name Attribute.Context.Constructor_declaration
      pattern (fun x -> x) )

let attr_GET = declare_router_attr `GET
let attr_POST = declare_router_attr `POST
let attr_PUT = declare_router_attr `PUT
let attr_DELETE = declare_router_attr `DELETE
let attrs = [ attr_GET; attr_POST; attr_PUT; attr_DELETE ]

let to_supported_arg_type (t : core_type) =
  let loc = t.ptyp_loc in
  match t.ptyp_desc with
  | Ptyp_constr (t, args) -> `constr (t.txt, args)
  | Ptyp_tuple xs -> `tuple xs
  | Ptyp_any | Ptyp_var _
  | Ptyp_arrow (_, _, _)
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ ->
      Location.raise_errorf ~loc
        "cannot automatically derive type parameter decoding/encoding" ()

let rec derive_conv suffix t =
  let loc = t.ptyp_loc in
  match to_supported_arg_type t with
  | `tuple ts ->
      let n = List.length ts in
      let name = Printf.sprintf "tuple%d" n in
      [%expr
        [%e evar ~loc name]
          [%e elist ~loc (List.map ts ~f:(derive_conv suffix))]]
  | `constr (name, args) ->
      let txt = Expansion_helpers.mangle_lid (Suffix suffix) name in
      let init = pexp_ident ~loc { loc; txt } in
      List.fold_left args ~init ~f:(fun acc arg ->
          pexp_apply ~loc acc [ Nolabel, derive_conv suffix arg ])

let td_to_ty param td =
  let name = td.ptype_name.txt in
  let loc = td.ptype_loc in
  let args = match param with Some param -> [ param ] | None -> [] in
  ptyp_constr ~loc (Located.lident ~loc name) args

let extract td =
  let loc = td.ptype_loc in
  let param =
    match td.ptype_params with
    | [] -> None
    | [ _ ] -> Some (ptyp_var ~loc "value")
    | _ ->
        Location.raise_errorf ~loc
          "only no or a single type parameter is supported"
  in
  let ctors =
    match td.ptype_kind with
    | Ptype_variant ctors -> ctors
    | Ptype_abstract | Ptype_record _ | Ptype_open ->
        Location.raise_errorf ~loc
          "only variant types are supported for by [@@deriving router]"
  in
  let ctors =
    List.fold_left ctors ~init:[] ~f:(fun ctors ctor ->
        let loc = ctor.pcd_loc in
        let lds =
          match ctor.pcd_args with
          | Pcstr_record lds -> lds
          | Pcstr_tuple [] -> []
          | Pcstr_tuple _ ->
              Location.raise_errorf ~loc
                "only record constructors are supported"
        in
        let info =
          List.find_map attrs ~f:(fun (method_, attr) ->
              match Attribute.get attr ctor with
              | None -> None
              | Some None -> Some (method_, None)
              | Some (Some uri) ->
                  let uri = Uri.of_string uri.txt in
                  Some (method_, Some uri))
        in
        let method_, uri =
          match info with
          | None -> `GET, Uri.of_string ("/" ^ ctor.pcd_name.txt)
          | Some (method_, Some uri) -> method_, uri
          | Some (method_, None) ->
              method_, Uri.of_string ("/" ^ ctor.pcd_name.txt)
        in
        let resolve_type name =
          let typ =
            List.find_map lds ~f:(fun ld ->
                match String.equal ld.pld_name.txt name with
                | true -> Some ld.pld_type
                | false -> None)
          in
          match typ with
          | None ->
              Location.raise_errorf ~loc "missing type for param: %S" name
          | Some typ -> typ
        in
        let path = List.rev (collect_params_rev ~loc uri) in
        let path =
          List.map path ~f:(function
            | `path x -> Ppath x
            | `param x -> Pparam (x, resolve_type x))
        in
        let query =
          List.filter_map lds ~f:(fun ld ->
              let is_path_param =
                List.exists path ~f:(function
                  | Pparam (name, _) -> String.equal name ld.pld_name.txt
                  | Ppath _ -> false)
              in
              if is_path_param then None
              else Some (ld.pld_name.txt, ld.pld_type))
        in
        let response =
          Option.map
            (fun x ->
              match x.ptyp_desc with
              | Ptyp_constr (_, [ x ]) -> x
              | _ -> assert false)
            ctor.pcd_res
        in
        let ctor = { ctor; method_; path; query; response } in
        (match
           List.find_opt ctors ~f:(equal_route_by_path_method ctor)
         with
        | None -> ()
        | Some conflict ->
            Location.raise_errorf ~loc
              "route %s %s is already defined in %s constructor"
              (method_to_string method_)
              (Uri.path uri) conflict.ctor.pcd_name.txt);
        ctor :: ctors)
  in
  param, List.rev ctors

let register derive =
  let derive_router ~ctxt (_rec_flag, type_decls) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match type_decls with
    | [ td ] -> derive td
    | [] -> assert false
    | _ ->
        Location.raise_errorf ~loc "expected exactly one type declaration"
  in
  let args = Deriving.Args.(empty) in
  let str_type_decl = Deriving.Generator.V2.make args derive_router in
  Deriving.add ~str_type_decl "router"

module Derive_href = struct
  let case ~loc (path : path) query x =
    match path, query with
    | [], [] -> [%expr "/"]
    | path, query -> (
        let pout, out =
          patt_and_expr ~loc (gen_symbol ~prefix:"out" ())
        in
        let psep, sep =
          patt_and_expr ~loc (gen_symbol ~prefix:"_sep" ())
        in
        let body = [%expr Buffer.contents [%e out]] in
        let body =
          match query with
          | [] -> body
          | q :: qs ->
              let f acc (name, typ) =
                let _pvalue, value = patt_and_expr ~loc name in
                let write =
                  [%expr
                    Stdlib.List.iter
                      (fun value ->
                        Buffer.add_char [%e out] ![%e sep];
                        Ppx_router_runtime.encode_query_key [%e out]
                          [%e estring ~loc name];
                        [%e sep] := '&';
                        Buffer.add_char [%e out] '=';
                        Ppx_router_runtime.encode_query_value [%e out]
                          value)
                      ([%e derive_conv "to_url_query" typ] [%e value])]
                in
                [%expr
                  [%e write];
                  [%e acc]]
              in
              let body = f body q in
              List.fold_left qs ~init:body ~f
        in
        let body =
          List.fold_left (List.rev path) ~init:body ~f:(fun acc param ->
              match param with
              | Ppath x ->
                  [%expr
                    Buffer.add_char [%e out] '/';
                    Ppx_router_runtime.encode_path [%e out]
                      [%e estring ~loc x];
                    [%e acc]]
              | Pparam (x, typ) ->
                  let to_url = derive_conv "to_url_path" typ in
                  [%expr
                    Buffer.add_char [%e out] '/';
                    Ppx_router_runtime.encode_path [%e out]
                      ([%e to_url] [%e evar ~loc x]);
                    [%e acc]])
        in
        let body =
          [%expr
            let [%p pout] = Buffer.create 16 in
            let [%p psep] = ref '?' in
            [%e body]]
        in
        let bnds =
          let make name =
            let pat = pvar ~loc name in
            let expr = pexp_field ~loc x { loc; txt = Lident name } in
            value_binding ~loc ~pat ~expr
          in
          List.filter_map path ~f:(fun param ->
              match param with
              | Ppath _ -> None
              | Pparam (name, _typ) -> Some (make name))
          @ List.map query ~f:(fun (name, _typ) -> make name)
        in
        match bnds with
        | [] -> body
        | bnds -> pexp_let ~loc Nonrecursive bnds body)

  let derive td (ctors : ctor list) =
    let loc = td.ptype_loc in
    let name = td.ptype_name.txt in
    let t, we =
      let vs =
        List.map td.ptype_params ~f:(fun _ -> gen_symbol ~prefix:"t" ())
      in
      let t =
        ptyp_constr ~loc
          { txt = Lident name; loc }
          (List.map vs ~f:(fun txt ->
               ptyp_constr ~loc { txt = Lident txt; loc } []))
      in
      let t = [%type: [%t t] -> string] in
      let we e =
        List.fold_left vs ~init:e ~f:(fun acc txt ->
            pexp_newtype ~loc { txt; loc } acc)
      in
      t, we
    in
    let name = Ppxlib.Expansion_helpers.mangle (Suffix "href") name in
    let cases =
      List.map ctors ~f:(fun ctor ->
          let loc = ctor.ctor.pcd_loc in
          let name = ctor.ctor.pcd_name in
          let lid = { loc; txt = Lident name.txt } in
          let has_params =
            List.exists ctor.path ~f:(function
              | Pparam _ -> true
              | _ -> false)
          in
          match has_params, ctor.query with
          | false, [] ->
              let p = ppat_construct ~loc lid None in
              p --> case ~loc ctor.path [] [%expr assert false]
          | _, query ->
              let px, x =
                patt_and_expr ~loc (gen_symbol ~prefix:"x" ())
              in
              let p = ppat_construct ~loc lid (Some px) in
              p --> case ~loc ctor.path query x)
    in
    [%stri
      let [%p pvar ~loc name] =
        [%e we [%expr ([%e pexp_function ~loc cases] : [%t t])]]]
end
