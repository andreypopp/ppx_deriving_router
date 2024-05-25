module type HANDLE = sig
  type request
  type response

  exception Method_not_allowed
  exception Invalid_query_parameter of string * string
  exception Invalid_body of string

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

module Make (Request : sig
  type t

  val path : t -> string
end) (Response : sig
  type t

  val respond :
    status:[ `Not_Found | `Bad_Request | `Method_Not_Allowed | `OK ] ->
    string ->
    t Lwt.t
end) :
  HANDLE with type request = Request.t and type response = Response.t =
struct
  type request = Request.t
  type response = Response.t

  exception Method_not_allowed
  exception Invalid_query_parameter of string * string
  exception Invalid_body of string

  type 'v route =
    | Route : ('a, 'v) Routes.path * 'a * ('v -> 'w) -> 'w route

  let prefix_route prefix f (Route (path, a, g)) =
    match prefix with
    | [] -> Route (path, a, fun x -> f (g x))
    | prefix ->
        let rec prefix_path p = function
          | [] -> p
          | x :: xs -> prefix_path Routes.(s x /~ p) xs
        in
        Route (prefix_path path (List.rev prefix), a, fun x -> f (g x))

  let to_route (Route (path, a, f)) = Routes.(map f (route path a))

  type 'a router = (Request.t -> 'a Lwt.t) Routes.router

  let make x = x

  let dispatch (router : _ router) req =
    let target = Request.path req in
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
      | `Invalid_query_parameter (param, msg) ->
          Response.respond ~status:`Bad_Request
            (Printf.sprintf "error processing query parameter %S: %s"
               param msg)
      | `Invalid_body reason ->
          Response.respond ~status:`Bad_Request
            (Printf.sprintf "Invalid or missing request body: %s" reason)
      | `Method_not_allowed ->
          Response.respond ~status:`Method_Not_Allowed
            "Method not allowed"
      | `Not_found -> Response.respond ~status:`Not_Found "Not found")
end
