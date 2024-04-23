open Routing

let ( >>= ) p f = Js.Promise.then_ f p

module Make_fetch (Route : sig
  type 'a t

  val http_method : 'a t -> [ `GET | `POST | `PUT | `DELETE ]
  val href : 'a t -> string
  val body : 'a t -> string option
  val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
end) : sig
  val fetch :
    ?headers:Fetch.headersInit ->
    ?referrer:string ->
    ?referrerPolicy:Fetch.referrerPolicy ->
    ?mode:Fetch.requestMode ->
    ?credentials:Fetch.requestCredentials ->
    ?cache:Fetch.requestCache ->
    ?redirect:Fetch.requestRedirect ->
    ?integrity:string ->
    ?keepalive:bool ->
    ?signal:Fetch.signal ->
    root:string ->
    'a Route.t ->
    'a Js.Promise.t
end = struct
  let fetch ?headers ?referrer ?referrerPolicy ?mode ?credentials ?cache
      ?redirect ?integrity ?keepalive ?signal ~root route =
    let href = root ^ Route.href route in
    let init =
      let body = Option.map Fetch.BodyInit.make (Route.body route) in
      let method_ =
        match Route.http_method route with
        | `GET -> Fetch.Get
        | `POST -> Fetch.Post
        | `PUT -> Fetch.Put
        | `DELETE -> Fetch.Delete
      in
      Fetch.RequestInit.make ~method_ ?headers ?referrer ?referrerPolicy
        ?mode ?credentials ?cache ?redirect ?integrity ?keepalive ?signal
        ?body ()
    in
    let req = Fetch.Request.makeWithInit href init in
    Fetch.fetchWithRequest req >>= fun response ->
    Route.decode_response route response
end

module Test_composition = struct
  open Ppx_router_runtime.Types
  open Ppx_deriving_json_runtime.Primitives

  type _ x = Home : int x | About : string x [@@deriving router]

  type _ t = Pages : 'a x -> 'a t | Api : { id : string } -> int t
  [@@deriving router]
end

module Fetch = Make_fetch (All)

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Pages.Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline (Pages.href (Hello { name = "world"; modifier = None }));
  print_endline
    (Pages.href (Hello { name = "world"; modifier = Some Uppercase }))

let fetch_and_log (req : _ All.t) =
  ignore
    ( Fetch.fetch ~root:"http://localhost:8080" req >>= fun user ->
      Js.log user;
      Js.Promise.resolve () )

let () =
  match Sys.argv.(2) with
  | exception Invalid_argument _ ->
      prerr_endline "missing subcommand";
      exit 1
  | "test" -> test ()
  | "get_user" -> fetch_and_log (Api (Get_user { id = 121 }))
  | "create_user" -> fetch_and_log (Api (Create_user { id = 42 }))
  | "raw" -> fetch_and_log (Api Raw_response)
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
