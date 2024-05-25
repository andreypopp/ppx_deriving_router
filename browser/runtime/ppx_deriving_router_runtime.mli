type json = Js.Json.t
type response = Fetch.Response.t

module Encode : sig
  type 'a encode_url_path = 'a -> string
  type 'a encode_url_query = 'a -> string list

  val encode_path : Buffer.t -> string -> unit
  val encode_query_key : Buffer.t -> string -> unit
  val encode_query_value : Buffer.t -> string -> unit
end

module Decode : sig
  type 'a decode_url_path = string -> 'a option
  type 'a decode_url_query = string list -> 'a option
end

module Witness : module type of Ppx_deriving_router_witness
module Primitives : module type of Ppx_deriving_router_primitives

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
