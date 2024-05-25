type json = Ppx_deriving_json_runtime.t

module Request : sig
  type t = Dream.request

  val queries : t -> (string * string) list
  val body : t -> string Lwt.t
  val method_ : t -> [ `DELETE | `GET | `POST | `PUT ]
end

type request = Request.t

module Response : sig
  type t = Dream.response

  type _ encode =
    | Encode_raw : t encode
    | Encode_json : ('a -> json) -> 'a encode

  val encode : 'a encode -> 'a -> t Lwt.t
end

type response = Response.t

module Encode : module type of Ppx_deriving_router_encode
module Decode : module type of Ppx_deriving_router_decode
module Primitives : module type of Ppx_deriving_router_primitives
module Witness : module type of Ppx_deriving_router_witness

module Handle :
  Ppx_deriving_router_handle.HANDLE
    with type request = Request.t
     and type response = Response.t
