open Routing
open Lwt.Infix

let pages_handle route _req =
  match route with
  | Pages.Home -> Dream.html "HOME PAGE"
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
      Dream.html greeting

let pages_handler = Pages.handle pages_handle

let api_handle : type a. a Api.t -> Dream.request -> a Lwt.t =
 fun x _req ->
  match x with
  | Raw_response -> Dream.respond "RAW RESPONSE"
  | List_users -> Lwt.return []
  | Create_user { id } -> Lwt.return { Api.id }
  | Get_user { id } -> Lwt.return { Api.id }

let api_handler : Dream.handler = Api.handle { f = api_handle }

let all_handler : Dream.handler =
  let f : type a. a All.t -> Dream.request -> a Lwt.t =
   fun x req ->
    match x with
    | Pages p -> pages_handle p req
    | Api e -> api_handle e req
  in
  All.handle { f }

let run () = Dream.run @@ Dream.logger @@ all_handler

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline (Pages.href (Hello { name = "world"; modifier = None }));
  print_endline
    (Pages.href (Hello { name = "world"; modifier = Some Uppercase }));
  print_endline (Api.href (Get_user { id = 121 }));
  print_endline
    (All.href (Pages (Hello { name = "world"; modifier = None })));
  print_endline (All.href (Api (Get_user { id = 121 })));
  print_endline "# TESTING ROUTE MATCHING GENERATION";
  let test_req ?body h method_ target =
    print_endline
      (Printf.sprintf "## %s %s" (Dream.method_to_string method_) target);
    Lwt_main.run
      (let req = Dream.request ~method_ ~target "" in
       Option.iter (Dream.set_body req) body;
       h req >>= fun resp ->
       Dream.body resp >>= fun body ->
       print_endline
         (Printf.sprintf "%s: %s"
            (Dream.status resp |> Dream.status_to_string)
            body);
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
  test_req api_handler ~body:"{}" `POST "/";
  test_req api_handler ~body:"1" `POST "/";
  test_req api_handler `GET "/121";
  test_req api_handler `GET "/raw-response";
  print_endline "# TESTING ROUTE MATCHING GENERATION (ALL)";
  test_req all_handler `GET "/hello/world";
  test_req all_handler `GET "/";
  test_req all_handler `GET "/api/121"

let () =
  match Sys.argv.(1) with
  | exception Invalid_argument _ -> run ()
  | "run" -> run ()
  | "test" -> test ()
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
