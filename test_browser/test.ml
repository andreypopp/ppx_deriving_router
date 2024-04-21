open Routing

let ( >>= ) p f = Js.Promise.then_ f p

module Make_fetch (Route : sig
  type 'a t

  val href : 'a t -> string
  val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
end) : sig
  val fetch : root:string -> 'a Route.t -> 'a Js.Promise.t
end = struct
  let fetch ~root route =
    let href = root ^ Route.href route in
    Fetch.fetch href >>= fun response ->
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
  | "raw" -> fetch_and_log (Api Raw_response)
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
