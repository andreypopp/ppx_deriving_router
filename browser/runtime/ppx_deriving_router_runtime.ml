type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

module Witness = Witness

module Types = struct
  let string_to_url_path x = x
  let string_of_url_path x = Some x
  let int_to_url_path x = string_of_int x
  let int_of_url_path x = int_of_string_opt x
  let bool_to_url_path x = if x then "true" else "false"

  let bool_of_url_path x =
    match x with "true" -> Some true | "false" -> Some false | _ -> None

  let rec last_wins f = function
    | [] -> None
    | [ x ] -> f x
    | _ :: xs -> last_wins f xs

  let string_to_url_query x = [ x ]
  let string_of_url_query = last_wins (fun x -> Some x)
  let int_to_url_query x = [ string_of_int x ]
  let int_of_url_query = last_wins int_of_string_opt
  let bool_to_url_query x = if x then [ "true" ] else []

  let bool_of_url_query =
    last_wins (function "true" -> Some true | _ -> Some false)

  let option_to_url_query :
      'a url_query_encoder -> 'a option url_query_encoder =
   fun f x -> match x with None -> [] | Some v -> f v

  let option_of_url_query :
      'a url_query_decoder -> 'a option url_query_decoder =
   fun f x ->
    match x with
    | [] -> Some None
    | x -> ( match f x with None -> None | Some v -> Some (Some v))
end

let encode_path out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)

let encode_query_key out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)

let encode_query_value out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)

type response = Fetch.Response.t
type json = Js.Json.t

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
