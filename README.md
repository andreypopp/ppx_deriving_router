# `ppx_router`

A typed router for Dream.

## Usage

Install (custom opam repo is required as for now):
```
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
opam install ppx_router
```

Put this into your `dune` file:
```
(...
 (preprocess (pps ppx_router))
 ...)
```

Define your routes:
```ocaml
module Routes = struct
  open Ppx_router_runtime.Types

  type t =
    | Home [@GET "/"]
    | About
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
    [@@deriving router]
end
```

Notice the `[@@deriving router]` annotation, which instructs to generate code
for routing based on the variant type definition.

Each branch in the variant type definition corresponds to a separate route.

It can have a `[@GET "/path"]` attribute (or `[@POST "/path"]`, etc.) which
specifies a path pattern for the route. If such attribute is missing, the path
is then inferred from the variant name, for example `About` will be routed to
`/About`, the method is GET by default (but one can supply `[@POST]`, etc.
attribute without any payload just to specify the method, leaving path
implicit).

The path pattern can contain named parameters, like `:name` in the example
above. In this case the parameter will be extracted from the path and used in
the route payload. All other fields from a route payload are considered query
parameters.

Now we can generate hrefs for these routes:
```ocaml
let () =
  assert (Routes.href Home = "/");
  assert (Routes.href About = "/About");
  assert (Routes.href (Hello {name="world"; repeat=1} = "/hello/world?repeat=1")
```

and define a handler for them:
```ocaml
let handle = Routes.handle (fun route _req ->
  match route with
  | Home -> Dream.html "Home page!"
  | About -> Dream.html "About page!"
  | Hello {name; repeat} ->
    let name =
      match repeat with
      | Some repeat ->
        List.init repeat (fun _ -> name) |> String.concat ", "
      | None -> name
    in
    Dream.html (Printf.sprintf "Hello, %s" name))
```

Finally we can use the handler in a Dream app:
```ocaml
let () = Dream.run ~interface:"0.0.0.0" ~port:8080 handle
```

## Custom path/query parameter types

When generating parameter encoding/decoding code for a parameter of type `T`,
`ppx_router` will emit the code that uses the following functions.

If `T` is a path parameter:
```ocaml
val T_of_url_path : string -> T option
val T_to_url_path : T -> string
```

If `T` is a query parameter:
```ocaml
val T_of_url_query : string list -> T option
val T_to_url_query : T -> string list
```

The default encoders/decoders are provided in `Ppx_router_runtime.Types` module
(this is why we need to `open` the module when defining routes).

To provide custom encoders/decoders for a custom type, we can define own
functions, for example:

```ocaml
module Modifier = struct
  type t = Capitalize | Uppercase

  let rec of_url_query : t Ppx_router_runtime.url_query_decoder = function
    | [] -> None
    | [ "capitalize" ] -> Some Capitalize
    | [ "uppercase" ] -> Some Uppercase
    | _ :: xs -> of_url_query xs (* let the last one win *)

  let to_url_query : t Ppx_router_runtime.url_query_encoder = function
    | Capitalize -> [ "capitalize" ]
    | Uppercase -> [ "uppercase" ]
end
```

After that one can use `Modifier.t` in route definitions:

```ocaml
type t =
  | Hello of { name : string; modifier : Modifier.t } [@GET "/hello/:name"]
  [@@deriving router]
```

## Defining routes with typed responses

It is possible to define routes with typed responses, with code automatically
generated to turn such responses into JSON payloads and wrap into
`Dream.response` values.

In this case the route type should be defined as GADT with a parameter for the
response type:

```ocaml
module Api_routes = struct
  open Ppx_router_runtime.Types
  open Ppx_deriving_json_runtime.Primitives

  type user = { id : int } [@@deriving json]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : user t [@POST "/"]
    | Get_user : { id : int } -> user t [@GET "/:id"]
    | Raw : Dream.response t [@GET "/raw"]
  [@@deriving router]
end
```

Then handler can be defined as follows:
```ocaml
let api_handler : Dream.handler =
  let f : type a. a Api_routes.t -> Dream.request -> a Lwt.t =
   fun x _req ->
    match x with
    | List_users -> Lwt.return []
    | Create_user -> Lwt.return { Api_routes.id = 42 }
    | Get_user { id } -> Lwt.return { Api_routes.id }
    | Raw -> Dream.respond "RAW"
  in
  Api_routes.handle { f }
```

Notice that the type annotation for `f` is required, and it should be passed
within a record to `Api_routes.handle` function.

Also notice the `Raw : Dream.response t` case, which allows to return a raw
Dream response, no encode will be generated in this case, but no type
information will be available either. This is useful, though, when one needs to
have API and non API routes together.

## Using with Melange

`ppx_router` can be used with Melange, 

For that, one should use `ppx_router.browser` ppx in `dune` file:
```
(...
 (preprocess (pps ppx_router.browser))
 ...)
```

For Melange the ppx will emit:
```ocaml
val href : 'a t -> string
val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
```

Note that if routes mention `Dream.response` in its response parameter then it
won't compile with Melange (because Dream is not available for Melange). For
that one should use `Ppx_router_runtime.response` type instead which is an
alias for `Dream.response` in native and for `Fetch.Response.t` in Melange.

The common setup is to define routes in a separate dune library which is
compiled both for native and for browser.
