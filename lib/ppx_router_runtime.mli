(** REQUEST DECODING *)

type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

module Types : sig
  val string_to_url_path : string url_path_encoder
  val string_of_url_path : string url_path_decoder
  val int_to_url_path : int url_path_encoder
  val int_of_url_path : int url_path_decoder
  val bool_to_url_path : bool url_path_encoder
  val bool_of_url_path : bool url_path_decoder
  val string_to_url_query : string url_query_encoder
  val string_of_url_query : string url_query_decoder
  val int_to_url_query : int url_query_encoder
  val int_of_url_query : int url_query_decoder
  val bool_to_url_query : bool url_query_encoder
  val bool_of_url_query : bool url_query_decoder

  val option_to_url_query :
    'a url_query_encoder -> 'a option url_query_encoder

  val option_of_url_query :
    'a url_query_decoder -> 'a option url_query_decoder
end

val encode_path : Buffer.t -> string -> unit
val encode_query_key : Buffer.t -> string -> unit
val encode_query_value : Buffer.t -> string -> unit

exception Method_not_allowed
exception Invalid_query_parameter of string * string list

(** RESPONSE ENCODING *)

type json = Ppx_deriving_json_runtime.t
type response = Dream.response

type _ encode =
  | Encode_raw : response encode
  | Encode_json : ('a -> json) -> 'a encode

val encode : 'a. 'a encode -> 'a -> response Lwt.t

(** ROUTING *)

type 'v route =
  | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

val prefix_route : string -> ('a -> 'b) -> 'a route -> 'b route
val to_route : 'a route -> 'a Routes.route

(** ROUTER *)

type 'a router

val make : (Dream.request -> 'a) Routes.router -> 'a router

val handle : 'a router -> ('a -> Dream.handler) -> Dream.handler
(** handle request given a router and a dispatcher *)
