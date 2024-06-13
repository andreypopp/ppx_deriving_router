open Routing

let ( >>= ) p f = Js.Promise.then_ f p

module F = Fetch
module Fetch = Ppx_deriving_router_runtime.Make_fetch (All)

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Pages.Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline
    (Pages.href
       (Hello { name = "world"; modifier = None; greeting = None }));
  print_endline
    (Pages.href
       (Hello
          { name = "world"; modifier = Some Uppercase; greeting = None }))

let fetch_and_log (req : _ All.t) =
  ignore
    (Fetch.fetch ~root:"http://localhost:8080" req >>= fun data ->
     Js.log data;
     Js.Promise.resolve ()
      : unit Js.Promise.t)

let fetch_and_log_response (req : F.response All.t) =
  ignore
    (Fetch.fetch ~root:"http://localhost:8080" req >>= fun resp ->
     F.Response.text resp >>= fun data ->
     Js.log data;
     Js.Promise.resolve ()
      : unit Js.Promise.t)

let () =
  match Sys.argv.(2) with
  | exception Invalid_argument _ ->
      prerr_endline "missing subcommand";
      exit 1
  | "test" -> test ()
  | "hello" ->
      fetch_and_log_response
        (Pages
           (Hello
              {
                name = "world";
                modifier = Some Uppercase;
                greeting = None;
              }))
  | "get_user" -> fetch_and_log (Api (Get_user { id = 121 }))
  | "create_user" -> fetch_and_log (Api (Create_user { id = 42 }))
  | "raw" -> fetch_and_log (Api Raw_response)
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
