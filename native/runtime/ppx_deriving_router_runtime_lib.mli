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
  S with type Request.t = Request.t and type Response.t = Response.t
