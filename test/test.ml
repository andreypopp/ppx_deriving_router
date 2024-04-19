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
  [@@deriving router]
end

let handler =
  Routes.handle (fun route _req ->
      match route with
      | Home -> Dream.html "HOME PAGE"
      | Hello { name; modifier } ->
          let name =
            match modifier with
            | None -> name
            | Some Uppercase -> String.uppercase_ascii name
            | Some Lowercase -> String.lowercase_ascii name
          in
          let greeting = Printf.sprintf "Hello, %s!" name in
          Dream.html greeting)

let run () = Dream.run @@ Dream.logger @@ handler

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Routes.href Routes.Home);
  print_endline
    (Routes.href (Routes.Hello { name = "world"; modifier = None }));
  print_endline
    (Routes.href
       (Routes.Hello { name = "world"; modifier = Some Uppercase }));
  print_endline "# TESTING ROUTE MATCHING GENERATION";
  let test_req method_ target =
    print_endline
      (Printf.sprintf "## %s %s" (Dream.method_to_string method_) target);
    Lwt_main.run
      (let open Lwt.Infix in
       let req = Dream.request ~method_ ~target "" in
       handler req >>= fun resp ->
       Dream.body resp >>= fun body ->
       print_endline body;
       Lwt.return ())
  in
  test_req `GET "/";
  test_req `GET "/hello/world";
  test_req `GET "/hello/world?modifier=uppercase"

let () =
  match Sys.argv.(1) with
  | exception Invalid_argument _ -> run ()
  | "run" -> run ()
  | "test" -> test ()
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
