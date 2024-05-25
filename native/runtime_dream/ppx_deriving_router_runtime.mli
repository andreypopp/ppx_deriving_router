type json = Ppx_deriving_json_runtime.t
type request = Dream.request
type response = Dream.response

val queries : request -> (string * string) list
val body : request -> string Lwt.t
val method_ : request -> [ `DELETE | `GET | `POST | `PUT ]

(** REQUEST DECODING *)

type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = string -> 'a -> (string * string) list

type 'a url_query_decoder =
  string -> (string * string) list -> ('a, string) result

module Primitives : module type of Ppx_deriving_router_primitives
module Witness : module type of Ppx_deriving_router_witness

exception Method_not_allowed
exception Invalid_query_parameter of string * string
exception Invalid_body of string

(** RESPONSE ENCODING *)

val encode_path : Buffer.t -> string -> unit
val encode_query_key : Buffer.t -> string -> unit
val encode_query_value : Buffer.t -> string -> unit

type _ encode =
  | Encode_raw : response encode
  | Encode_json : ('a -> json) -> 'a encode

val encode : 'a encode -> 'a -> response Lwt.t

(** ROUTING *)

type 'v route =
  | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

val prefix_route : string list -> ('a -> 'b) -> 'a route -> 'b route
val to_route : 'a route -> 'a Routes.route

(** ROUTER *)

type 'a router

val make : (Dream.request -> 'a Lwt.t) Routes.router -> 'a router

val handle : 'a router -> ('a -> Dream.handler) -> Dream.handler
(** handle request given a router and a dispatcher *)

val dispatch :
  'a router ->
  Dream.request ->
  [ `Invalid_query_parameter of string * string
  | `Invalid_body of string
  | `Method_not_allowed
  | `Not_found
  | `Ok of 'a ]
  Lwt.t
