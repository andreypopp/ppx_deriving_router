type modifier =
  | Uppercase
  | Lowercase
      (** this a custom type which we want to be able to serialize/deserialize
          from/to the URL query *)

let rec modifier_of_url_query = function
  | [] -> None
  | [ "uppercase" ] -> Some Uppercase
  | [ "lowercase" ] -> Some Lowercase
  | _ :: rest -> modifier_of_url_query rest (* last wins, if multiple *)

let modifier_to_url_query = function
  | Uppercase -> [ "uppercase" ]
  | Lowercase -> [ "lowercase" ]

module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/"]
    | Hello of { name : string; modifier : modifier option }
        [@GET "/hello/:name"]
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
  [@@deriving router]
end
