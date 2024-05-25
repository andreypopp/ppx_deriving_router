type http_method = [ `DELETE | `GET | `POST | `PUT ]

type http_status =
  [ `Not_Found | `Bad_Request | `Method_Not_Allowed | `OK ]

module type REQUEST = sig
  type t

  val path : t -> string
  val queries : t -> (string * string) list
  val body : t -> string Lwt.t
  val method_ : t -> http_method
end

module type RESPONSE = sig
  type t

  val respond :
    status:http_status ->
    headers:(string * string) list ->
    string ->
    t Lwt.t
end

module type S = sig
  type json = Yojson.Basic.t

  module Request : REQUEST

  type request = Request.t

  module Response : RESPONSE

  type response = Response.t

  module Encode : module type of Ppx_deriving_router_encode
  module Decode : module type of Ppx_deriving_router_decode
  module Primitives : module type of Ppx_deriving_router_primitives
  module Witness : module type of Ppx_deriving_router_witness

  module Handle : sig
    exception Method_not_allowed
    exception Invalid_query_parameter of string * string
    exception Invalid_body of string

    type _ encode =
      | Encode_raw : response encode
      | Encode_json : ('a -> json) -> 'a encode

    val encode : 'a encode -> 'a -> response Lwt.t

    type 'v route =
      | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

    val prefix_route : string list -> ('a -> 'b) -> 'a route -> 'b route
    val to_route : 'a route -> 'a Routes.route

    (** ROUTER *)

    type 'a router

    val make : (request -> 'a Lwt.t) Routes.router -> 'a router

    val handle :
      'a router ->
      ('a -> request -> response Lwt.t) ->
      request ->
      response Lwt.t
    (** handle request given a router and a dispatcher *)

    val dispatch :
      'a router ->
      request ->
      [ `Invalid_query_parameter of string * string
      | `Invalid_body of string
      | `Method_not_allowed
      | `Not_found
      | `Ok of 'a ]
      Lwt.t
  end
end

module Make (Request : REQUEST) (Response : RESPONSE) :
  S with type Request.t = Request.t and type Response.t = Response.t =
struct
  type json = Yojson.Basic.t
  type request = Request.t
  type response = Response.t

  module Request = Request
  module Response = Response
  module Encode = Ppx_deriving_router_encode
  module Decode = Ppx_deriving_router_decode
  module Primitives = Ppx_deriving_router_primitives
  module Witness = Ppx_deriving_router_witness

  module Handle = struct
    exception Method_not_allowed
    exception Invalid_query_parameter of string * string
    exception Invalid_body of string

    type _ encode =
      | Encode_raw : response encode
      | Encode_json : ('a -> json) -> 'a encode

    let encode : type a. a encode -> a -> response Lwt.t =
     fun enc x ->
      match enc, x with
      | Encode_raw, x -> Lwt.return x
      | Encode_json to_json, x ->
          Response.respond ~status:`OK
            ~headers:[ "Content-Type", "application/json" ]
            (Yojson.Basic.to_string (to_json x))

    type 'v route =
      | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

    let prefix_route prefix f (Route (path, a, g)) =
      match prefix with
      | [] -> Route (path, a, fun x -> f (g x))
      | prefix ->
          let rec prefix_path p = function
            | [] -> p
            | x :: xs -> prefix_path Routes.(s x /~ p) xs
          in
          Route (prefix_path path (List.rev prefix), a, fun x -> f (g x))

    let to_route (Route (path, a, f)) = Routes.(map f (route path a))

    type 'a router = (Request.t -> 'a Lwt.t) Routes.router

    let make x = x

    let dispatch (router : _ router) req =
      let target = Request.path req in
      match Routes.match' router ~target with
      | Routes.FullMatch v | Routes.MatchWithTrailingSlash v ->
          Lwt.bind
            (Lwt_result.catch (fun () -> v req))
            (function
              | Ok v -> Lwt.return (`Ok v)
              | Error (Invalid_query_parameter (x, y)) ->
                  Lwt.return (`Invalid_query_parameter (x, y))
              | Error (Invalid_body reason) ->
                  Lwt.return (`Invalid_body reason)
              | Error Method_not_allowed -> Lwt.return `Method_not_allowed
              | Error exn -> Lwt.fail exn)
      | Routes.NoMatch -> Lwt.return `Not_found

    let handle (router : _ router) f req =
      Lwt.bind (dispatch router req) (function
        | `Ok v -> f v req
        | `Invalid_query_parameter (param, msg) ->
            Response.respond ~status:`Bad_Request ~headers:[]
              (Printf.sprintf "error processing query parameter %S: %s"
                 param msg)
        | `Invalid_body reason ->
            Response.respond ~status:`Bad_Request ~headers:[]
              (Printf.sprintf "Invalid or missing request body: %s" reason)
        | `Method_not_allowed ->
            Response.respond ~status:`Method_Not_Allowed ~headers:[]
              "Method not allowed"
        | `Not_found ->
            Response.respond ~status:`Not_Found ~headers:[] "Not found")
  end
end
