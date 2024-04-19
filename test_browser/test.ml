type modifier = Uppercase | Lowercase

let rec modifier_of_url_query = function
  | [] -> None
  | [ "uppercase" ] -> Some Uppercase
  | [ "lowercase" ] -> Some Lowercase
  | _ :: rest -> modifier_of_url_query rest (* last wins, if multiple *)

let modifier_to_url_query = function
  | Uppercase -> [ "uppercase" ]
  | Lowercase -> [ "lowercase" ]

module Routes = struct
  open Ppx_router_runtime.Types

  type t =
    | Home [@GET "/"]
    | Hello of { name : string; modifier : modifier option }
        [@GET "/hello/:name"]
    | Route_with_implicit_path of { param : string option }
    | Route_with_implicit_path_post [@POST]
  [@@deriving router]
end

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Routes.href Routes.Home);
  print_endline
    (Routes.href (Routes.Route_with_implicit_path { param = None }));
  print_endline
    (Routes.href (Routes.Route_with_implicit_path { param = Some "ok" }));
  print_endline
    (Routes.href (Routes.Hello { name = "world"; modifier = None }));
  print_endline
    (Routes.href
       (Routes.Hello { name = "world"; modifier = Some Uppercase }))

let () =
  match Sys.argv.(2) with
  | exception Invalid_argument _ ->
      prerr_endline "missing subcommand";
      exit 1
  | "test" -> test ()
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
