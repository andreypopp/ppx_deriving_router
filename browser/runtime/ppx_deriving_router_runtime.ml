type json = Js.Json.t

module Encode = struct
  type 'a encode_url_path = 'a -> string
  type 'a encode_url_query = 'a -> string list

  let encode_path out x =
    Buffer.add_string out (Js.Global.encodeURIComponent x)

  let encode_query_key out x =
    Buffer.add_string out (Js.Global.encodeURIComponent x)

  let encode_query_value out x =
    Buffer.add_string out (Js.Global.encodeURIComponent x)
end

module Decode = struct
  type 'a decode_url_path = string -> 'a option
  type 'a decode_url_query = string list -> 'a option
end

module Witness = Ppx_deriving_router_witness
module Primitives = Ppx_deriving_router_primitives

type response = Fetch.Response.t

module Make_fetch (Route : sig
  type 'a t

  val http_method : 'a t -> [ `GET | `POST | `PUT | `DELETE ]
  val href : 'a t -> string
  val body : 'a t -> Js.Json.t option
  val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
end) : sig
  val fetch' :
    ?signal:Fetch.signal ->
    ?root:string ->
    'a Route.t ->
    Fetch.Response.t Js.Promise.t

  val fetch :
    ?signal:Fetch.signal -> ?root:string -> 'a Route.t -> 'a Js.Promise.t
end = struct
  let fetch' ?signal ?root route =
    let href = Route.href route in
    let href =
      match root with None -> href | Some root -> root ^ href
    in
    let init =
      let body =
        match Route.body route with
        | None -> None
        | Some body -> Some (Fetch.BodyInit.make (Js.Json.stringify body))
      in
      let method_ =
        match Route.http_method route with
        | `GET -> Fetch.Get
        | `POST -> Fetch.Post
        | `PUT -> Fetch.Put
        | `DELETE -> Fetch.Delete
      in
      Fetch.RequestInit.make ~method_ ?signal ?body ()
    in
    let req = Fetch.Request.makeWithInit href init in
    Fetch.fetchWithRequest req

  let fetch ?signal ?root route =
    fetch' ?signal ?root route
    |> Js.Promise.then_ (Route.decode_response route)
end
