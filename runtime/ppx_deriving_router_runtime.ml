type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

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
  Buffer.add_string out (Uri.pct_encode ~component:`Path x)

let encode_query_key out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_key x)

let encode_query_value out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_value x)

exception Method_not_allowed
exception Invalid_query_parameter of string * string list
exception Invalid_body of string

type 'v route =
  | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

let prefix_route prefix f (Route (path, a, g)) =
  match prefix with
  | None -> Route (path, a, fun x -> f (g x))
  | Some prefix -> Route (Routes.(s prefix /~ path), a, fun x -> f (g x))

let to_route (Route (path, a, f)) = Routes.(map f (route path a))

type json = Ppx_deriving_json_runtime.t
type response = Dream.response

type _ encode =
  | Encode_raw : response encode
  | Encode_json : ('a -> json) -> 'a encode

let encode : type a. a encode -> a -> Dream.response Lwt.t =
 fun enc x ->
  match enc, x with
  | Encode_raw, x -> Lwt.return x
  | Encode_json to_json, x ->
      Dream.json (Yojson.Basic.to_string (to_json x))

type 'a router = (Dream.request -> 'a Lwt.t) Routes.router

let make x = x

let dispatch (router : _ router) req =
  let target = Dream.target req in
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
    | `Invalid_query_parameter (param, _) ->
        Dream.respond ~status:`Bad_Request
          (Printf.sprintf "Invalid or missing query parameter: %s" param)
    | `Invalid_body reason ->
        Dream.respond ~status:`Bad_Request
          (Printf.sprintf "Invalid or missing request body: %s" reason)
    | `Method_not_allowed ->
        Dream.respond ~status:`Method_Not_Allowed "Method not allowed"
    | `Not_found -> Dream.respond ~status:`Not_Found "Not found")
