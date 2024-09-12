type modifier =
  | Uppercase
  | Lowercase
      (** this a custom type which we want to be able to serialize/deserialize
          from/to the URL query *)

let modifier_of_url_query k xs =
  match List.assoc_opt k xs with
  | Some "uppercase" -> Ok Uppercase
  | Some "lowercase" -> Ok Lowercase
  | Some _ -> Error "invalid modifier"
  | None -> Error "missing modifier"

let modifier_to_url_query k = function
  | Uppercase -> [ k, "uppercase" ]
  | Lowercase -> [ k, "lowercase" ]

module Options = struct
  open Ppx_deriving_json_runtime.Primitives

  type t = { a : int option } [@@deriving json, url_query_via_json]
end

module User_id : sig
  type t

  val inject : string -> t
  val project : t -> string
end = struct
  type t = string

  let inject x = x
  let project x = x
end

module Level = struct
  type t = Alert | Warning

  let to_int = function Alert -> 2 | Warning -> 1

  let of_int = function
    | 2 -> Alert
    | 1 -> Warning
    | _ -> failwith "invalid level"
end

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type user_id = User_id.t
  [@@deriving url_query_via_iso, url_path_via_iso]

  type level = Level.t
  [@@deriving
    url_query_via_iso { t = int; inject = of_int; project = to_int }]

  type t =
    | Home [@GET "/"]
    | Hello of {
        name : string;
        modifier : modifier option;
        greeting : string option;
      } [@GET "/hello/:name"]
    | Echo_options of { options : Options.t }
    | List_users of { user_ids : user_id list }
    | User_info of { user_id : user_id }
    | User_info_via_path of { user_id : user_id } [@GET "/user/:user_id"]
    | Signal of { level : level }
    | Route_with_implicit_path of { param : string option }
    | Route_with_implicit_path_post [@POST]
  [@@deriving router]
end

module Api = struct
  open Ppx_deriving_router_runtime.Primitives
  open Ppx_deriving_json_runtime.Primitives

  type user = { id : int } [@@deriving json]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : { id : int [@body] } -> user t [@POST "/"]
    | Get_user : { id : int } -> user t [@GET "/:id"]
    | Raw_response : Ppx_deriving_router_runtime.response t
        [@GET "/raw-response"]
  [@@deriving router]
end

module All = struct
  type _ t =
    | Pages : Pages.t -> Ppx_deriving_router_runtime.response t
        [@prefix "/"]
    | Api : 'a Api.t -> 'a t [@prefix "/nested/api"]
    | Static : { path : string } -> Ppx_deriving_router_runtime.response t
        [@GET "/static/...path"]
  [@@deriving router]
end
