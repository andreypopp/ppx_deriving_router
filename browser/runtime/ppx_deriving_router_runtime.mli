type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

module Witness : module type of Ppx_deriving_router_witness
module Primitives : module type of Ppx_deriving_router_primitives

val encode_path : Buffer.t -> string -> unit
val encode_query_key : Buffer.t -> string -> unit
val encode_query_value : Buffer.t -> string -> unit

type response = Fetch.Response.t
type json = Js.Json.t

module Make_fetch (Route : sig
  type 'a t

  val http_method : 'a t -> [ `DELETE | `GET | `POST | `PUT ]
  val href : 'a t -> string
  val body : 'a t -> json option
  val decode_response : 'a t -> response -> 'a Js.Promise.t
end) : sig
  val fetch' :
    ?signal:Fetch.signal ->
    ?root:string ->
    'a Route.t ->
    Fetch.Response.t Js.Promise.t

  val fetch :
    ?signal:Fetch.signal -> ?root:string -> 'a Route.t -> 'a Js.Promise.t
end
