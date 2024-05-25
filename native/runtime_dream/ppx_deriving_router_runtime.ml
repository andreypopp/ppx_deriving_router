type json = Ppx_deriving_json_runtime.t
type queries = (string * string) list

module Request = struct
  type t = Dream.request

  let queries : t -> queries = Dream.all_queries
  let body = Dream.body
  let path = Dream.target

  let method_ req =
    match Dream.method_ req with
    | `GET -> `GET
    | `POST -> `POST
    | `PUT -> `PUT
    | `DELETE -> `DELETE
    | _ -> failwith "Unsupported method"
end

type request = Request.t

module Response = struct
  type t = Dream.response

  type _ encode =
    | Encode_raw : t encode
    | Encode_json : ('a -> json) -> 'a encode

  let encode : type a. a encode -> a -> t Lwt.t =
   fun enc x ->
    match enc, x with
    | Encode_raw, x -> Lwt.return x
    | Encode_json to_json, x ->
        Dream.json (Yojson.Basic.to_string (to_json x))

  let respond ~status body = Dream.respond ~status body
end

type response = Response.t

module Witness = Ppx_deriving_router_witness
module Primitives = Ppx_deriving_router_primitives
module Encode = Ppx_deriving_router_encode
module Decode = Ppx_deriving_router_decode
module Handle = Ppx_deriving_router_handle.Make (Request) (Response)
