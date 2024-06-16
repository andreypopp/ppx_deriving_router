# `ppx_deriving_router`

Derive type safe routing from OCaml variant type declarations.

Supports Dream and Melange. Enables type safe client-server communication.

## Usage

Install (custom opam repo is required as for now):
```
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
opam install ppx_deriving_router
```

Add preprocessing configuration in `dune`:
```
(...
 (preprocess (pps ppx_deriving_router.dream))
 ...)
```

Define routes:
```ocaml
module Pages = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/"]
    | About
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
    [@@deriving router]
end
```

Note the `[@@deriving router]` annotation, which instructs to generate routing
code based on the variant type declaration it is attached to. Each constructor
corresponds to a separate route.

By default the route corresponds to a GET request, and path is inferred from
the constructor name.

By attaching `[@GET]`, `[@POST]`, `[@PUT]`, `[@DELETE]` attributes to the
constructor, one can specify the HTTP method for the route.

The attributes also allow to specify the path pattern for the route, e.g.
`[@GET "/hello/:name"]`. As seen, path patterns can contain named parameters,
like `:name` in the example above. In this case the parameter will be extracted
from the path and used in the route payload. All other fields from a route
payload are considered URL query parameters.

## Generating URLs

A function with signature:
```ocaml
val href : t -> string
```

is generated for each route type. Such function can be used to generate URLs based on routes:
```ocaml
let () =
  assert (Pages.href Home = "/");
  assert (Pages.href About = "/About");
  assert (Pages.href (Hello {name="world"; repeat=1} = "/hello/world?repeat=1")
```

## Handling routes

A function with signature:
```ocaml
val handle : (t -> Dream.request -> Dream.response Lwt.t) -> Dream.handler
```

is generated for each route type. Such function can be used to define a `Dream.handler`:
```ocaml
let pages_handle =
  Pages.handle (fun route _req ->
      match route with
      | Home -> Dream.respond "Home page!"
      | About -> Dream.respond "About page!"
      | Hello { name; repeat } ->
          let name =
            match repeat with
            | Some repeat ->
                List.init repeat (fun _ -> name) |> String.concat ", "
            | None -> name
          in
          Dream.respond (Printf.sprintf "Hello, %s" name))
```

## Using the handler in a Dream app

As a result of the `Pages.handle` call we get a `Dream.handler` which can be used in a Dream app:
```ocaml
let () = Dream.run ~interface:"0.0.0.0" ~port:8080 pages_handle
```

## Custom path/query parameter types

When generating parameter encoding/decoding code for a parameter of type `T`,
`ppx_deriving_router` will emit the code that uses the following functions.

If `T` is a path parameter:
```ocaml
val T_of_url_path : string -> T option
val T_to_url_path : T -> string
```

If `T` is a query parameter:
```ocaml
val T_of_url_query : string -> (string * string) list -> (T, string) result
val T_to_url_query : string -> T -> (string * string) list
```

The default encoders/decoders are provided in `Ppx_deriving_router_runtime.Primitives` module
(this is why we need to `open` the module when defining routes).

To provide custom encoders/decoders for a custom type, we can define own
functions, for example:

```ocaml
module Modifier = struct
  type t = Capitalize | Uppercase

  let rec of_url_query : t Ppx_deriving_router_runtime.url_query_decoder = fun k qs ->
    match List.assoc_opt k qs with
    | None -> Error "missing modifier"
    | Some "capitalize" -> Ok Capitalize
    | Some "uppercase" -> Ok Uppercase
    | Some _ -> Error "unknown modifier"

  let to_url_query : t Ppx_deriving_router_runtime.url_query_encoder = fun k v ->
    match v with
    | Capitalize -> [ k, "capitalize" ]
    | Uppercase -> [ k, "uppercase" ]
end
```

After that one can use `Modifier.t` in route definitions:

```ocaml
type t =
  | Hello of { name : string; modifier : Modifier.t } [@GET "/hello/:name"]
  [@@deriving router]
```

## Routes with typed responses

It is possible to define routes with typed responses, with code automatically
generated to turn such responses into JSON payloads and wrap into
`Dream.response` values.

In this case the route type should be defined as GADT with a parameter for the
response type:

```ocaml
module Api = struct
  open Ppx_deriving_router_runtime.Primitives
  open Ppx_deriving_json_runtime.Primitives

  type user = { id : int } [@@deriving json]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : user t [@POST "/"]
    | Get_user : { id : int } -> user t [@GET "/:id"]
    | Raw : Ppx_deriving_router_runtime.response t [@GET "/raw"]
  [@@deriving router]
end
```

Then handler can be defined as follows:
```ocaml
let api_handle : Dream.handler =
  let f : type a. a Api.t -> Dream.request -> a Lwt.t =
   fun x _req ->
    match x with
    | List_users -> Lwt.return []
    | Create_user -> Lwt.return { Api.id = 42 }
    | Get_user { id } -> Lwt.return { Api.id }
    | Raw -> Dream.respond "RAW"
  in
  Api.handle { f }
```

Notice that the type annotation for `f` is required, and it should be passed
within a record to `Api.handle` function.

Also notice the `Raw : Dream.response t` case, which allows to return a raw
Dream response, no encode will be generated in this case, but no type
information will be available either. This is useful, though, when one needs to
have API and non API routes together.

## Decoding request body

It is possible to designate a route parameter to be a request body, in this
case, its value is decoded from the request body as JSON. The JSON decoder is
generated automatically for the route parameter type:
```ocaml
open Ppx_deriving_json_runtime.Primitives
type user_spec = { name : string } [@@deriving json]
type _ api =
| Create_user : {spec: user_spec; [@body]} -> int t [@POST]
[@@deriving router]
```

## Wildcard path patterns

It is possible to capture the remaining part of the path as a parameter when
the `...name` path pattern is in the last position:
```ocaml
type t =
| Static : {path : string} -> t [@GET "/static/...path"]
[@@deriving router]
```

## Route composition

It is possible to compose routes by embedding other routes as arguments to
constructor, consider the example:
```ocaml
module Routes = struct

  type _ t =
    | Pages : Pages.t -> Dream.response t [@prefix "/"]
    | Api : 'a Api.t -> 'a t
  [@@deriving router]
end
```

In this case the URLs structure will be as follows:
```ocaml
let () =
  assert (Routes.href (Pages Home) = "/");
  assert (Routes.href (Pages About) = "/About");
  assert (Routes.href (Api (Get_user {id=1})) = "/Api/1");
```

Notice how `[@prefix]` attribute is used to specify the path for the routes
prefix (in its absence the path will be equal to the coresponding constructor
name).

The handler can be defined as follows:
```ocaml
let routes_handler : Dream.handler =
  let f : type a. a All.t -> Dream.request -> a Lwt.t =
   fun x req ->
    match x with
    | Pages p -> pages_handle p req
    | Api e -> api_handle e req
  in
  All.handle { f }
```

Note how we delegated request processing to corresponding handlers. We can also
run certain middlewares for certain routes, if we wish to do so.

## Deriving `to_url_query`/`of_url_query` through JSON representation

It is possible to derive `to_url_query` and `of_url_query` through a JSON
representation of a type by using `url_query_via_json` deriver:

```ocaml
type t = { a : int option } [@@deriving json, url_query_via_json]
```

This won't result in pretty URL params but useful to quickly get something
passed through URL.

## Deriving `to_url_query`/`of_url_query` through isomorphism

It is possible to derive `to_url_query` and `of_url_query` through an
isomorphism to/from other type by using `url_query_via_iso` deriver.

Conside the following type first:

```ocaml
module User_id : sig
  type t

  val inject : string -> t
  val project : t -> string
end = struct
  type t = string

  let inject x = x
  let project x = x
end
```

Now we know that its underlying representation is a `string`, and we know how
to convert it to/from a string. We can use `url_query_via_iso` deriver to
derive `to_url_query` and `of_url_query` for the type, for that we need to
define a type alias:

```ocaml
type user_id = User_id.t
[@@deriving url_query_via_iso]
```

It's possible to customize which functions and which underlying type to use:

```ocaml
module Level = struct
  type t = Alert | Warning

  let to_int = function Alert -> 2 | Warning -> 1

  let of_int = function
    | 2 -> Alert
    | 1 -> Warning
    | _ -> failwith "invalid level"
end

type level = Level.t
[@@deriving url_query_via_iso { t = int; inject = of_int; project = to_int }]
```

## Deriving `to_url_path`/`of_url_path` through isomorphism

Similar to the above a `url_path_via_iso` deriver is available, an example:

```ocaml
type user_id = User_id.t
[@@deriving url_path_via_iso]
```

## Using with Melange

`ppx_deriving_router` can be used with Melange, 

For that, one should use `ppx_deriving_router.browser` ppx in `dune` file:
```
(...
 (preprocess (pps ppx_deriving_router.browser))
 ...)
```

For Melange the ppx will emit:
```ocaml
val http_method : _ t -> [ `DELETE | `GET | `POST | `PUT ]
val href : 'a t -> string
val body : 'a t -> string option
val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
```

Then the following code could be used to generate a typesafe client from a
routes definition:
```ocaml
module Make_fetch (Route : sig
  type 'a t

  val http_method : 'a t -> [ `GET | `POST | `PUT | `DELETE ]
  val href : 'a t -> string
  val body : 'a t -> string option
  val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
end) : sig
  val fetch : root:string -> 'a Route.t -> 'a Js.Promise.t
end = struct
  let fetch ~root route =
    let href = root ^ Route.href route in
    let init =
      let method_ =
        match Route.http_method route with
        | `GET -> Fetch.Get
        | `POST -> Fetch.Post
        | `PUT -> Fetch.Put
        | `DELETE -> Fetch.Delete
      in
      let body = Option.map Fetch.BodyInit.make (Route.body route) in
      Fetch.RequestInit.make ~method_ ?body ()
    in
    let req = Fetch.Request.makeWithInit href init in
    Fetch.fetchWithRequest req >>= fun response ->
    Route.decode_response route response
end
```

Note that if routes mention `Dream.response` in its response parameter then it
won't compile with Melange (because Dream is not available for Melange). For
that one should use `Ppx_deriving_router_runtime.response` type instead which is an
alias for `Dream.response` in native and for `Fetch.Response.t` in Melange.

The common setup is to define routes in a separate dune library which is
compiled both for native and for browser.
