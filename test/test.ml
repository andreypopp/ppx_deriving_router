type modifier = Uppercase | Lowercase

let rec modifier_of_url_query = function
  | [] -> None
  | [ "uppercase" ] -> Some Uppercase
  | [ "lowercase" ] -> Some Lowercase
  | _ :: rest -> modifier_of_url_query rest (* last wins, if multiple *)

let modifier_to_url_query = function
  | Uppercase -> [ "uppercase" ]
  | Lowercase -> [ "lowercase" ]

module Page_routes = struct
  open Ppx_router_runtime.Types

  type t =
    | Home [@GET "/"]
    | Hello of { name : string; modifier : modifier option }
        [@GET "/hello/:name"]
    | Route_with_implicit_path of { param : string option }
    | Route_with_implicit_path_post [@POST]
  [@@deriving router]
end

let pages_handler =
  Page_routes.handle (fun route _req ->
      match route with
      | Home -> Dream.html "HOME PAGE"
      | Route_with_implicit_path { param } ->
          let param = Option.value ~default:"-" param in
          Dream.html ("works as well, param is: " ^ param)
      | Route_with_implicit_path_post -> Dream.html "posted"
      | Hello { name; modifier } ->
          let name =
            match modifier with
            | None -> name
            | Some Uppercase -> String.uppercase_ascii name
            | Some Lowercase -> String.lowercase_ascii name
          in
          let greeting = Printf.sprintf "Hello, %s!" name in
          Dream.html greeting)

module Api_routes = struct
  open Ppx_router_runtime.Types
  open Ppx_deriving_json_runtime.Primitives

  type user = { id : int } [@@deriving json]

  type _ t =
    | List_users : user list t [@GET "/"]
    | Create_user : user t [@POST "/"]
    | Get_user : { id : int } -> user t [@GET "/:id"]
    | Raw_response : Dream.response t [@GET "/raw-response"]
  [@@deriving router]
end

let api_handler : Dream.handler =
  let f : type a. a Api_routes.t -> Dream.request -> a Lwt.t =
   fun x _req ->
    match x with
    | Raw_response -> Dream.respond "RAW RESPONSE"
    | List_users -> Lwt.return []
    | Create_user -> Lwt.return { Api_routes.id = 42 }
    | Get_user { id } -> Lwt.return { Api_routes.id }
  in
  Api_routes.handle { f }

let run () = Dream.run @@ Dream.logger @@ pages_handler

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Page_routes.href Home);
  print_endline
    (Page_routes.href (Route_with_implicit_path { param = None }));
  print_endline
    (Page_routes.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline
    (Page_routes.href (Hello { name = "world"; modifier = None }));
  print_endline
    (Page_routes.href
       (Hello { name = "world"; modifier = Some Uppercase }));
  print_endline "# TESTING ROUTE MATCHING GENERATION";
  let test_req h method_ target =
    print_endline
      (Printf.sprintf "## %s %s" (Dream.method_to_string method_) target);
    Lwt_main.run
      (let open Lwt.Infix in
       let req = Dream.request ~method_ ~target "" in
       h req >>= fun resp ->
       Dream.body resp >>= fun body ->
       print_endline body;
       Lwt.return ())
  in
  test_req pages_handler `GET "/";
  test_req pages_handler `GET "/hello/world";
  test_req pages_handler `GET "/hello/world?modifier=uppercase";
  test_req pages_handler `GET "/Route_with_implicit_path";
  test_req pages_handler `GET "/Route_with_implicit_path?param=ok";
  test_req pages_handler `POST "/Route_with_implicit_path?param=ok";
  test_req pages_handler `GET "/Route_with_implicit_path_post";
  test_req pages_handler `POST "/Route_with_implicit_path_post";
  print_endline "# TESTING ROUTE MATCHING GENERATION (API)";
  test_req api_handler `GET "/";
  test_req api_handler `POST "/";
  test_req api_handler `GET "/121";
  test_req api_handler `GET "/raw-response"

let () =
  match Sys.argv.(1) with
  | exception Invalid_argument _ -> run ()
  | "run" -> run ()
  | "test" -> test ()
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
