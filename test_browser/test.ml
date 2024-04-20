open Routing

let ( >>= ) p f = Js.Promise.then_ f p

module Make_fetch (Route : sig
  type 'a t

  val href : 'a t -> string
  val decode_response : 'a t -> string -> 'a
end) : sig
  val fetch : root:string -> 'a Route.t -> 'a Js.Promise.t
end = struct
  let fetch ~root route =
    let href = root ^ Route.href route in
    Fetch.fetch href >>= Fetch.Response.text >>= fun data ->
    let value = Route.decode_response route data in
    Js.Promise.resolve value
end

module Fetch = Make_fetch (Api)

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Pages.Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline (Pages.href (Hello { name = "world"; modifier = None }));
  print_endline
    (Pages.href (Hello { name = "world"; modifier = Some Uppercase }))

let () =
  match Sys.argv.(2) with
  | exception Invalid_argument _ ->
      prerr_endline "missing subcommand";
      exit 1
  | "test" -> test ()
  | "get_user" ->
      ignore
        ( Fetch.fetch ~root:"http://localhost:8080" (Get_user { id = 121 })
        >>= fun user ->
          Js.log user;
          Js.Promise.resolve () )
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
