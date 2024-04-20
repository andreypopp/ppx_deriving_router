open Routing

let pages_handler =
  Pages.handle (fun route _req ->
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

let api_handler : Dream.handler =
  let f : type a. a Api.t -> Dream.request -> a Lwt.t =
   fun x _req ->
    match x with
    | Raw_response -> Dream.respond "RAW RESPONSE"
    | List_users -> Lwt.return []
    | Create_user -> Lwt.return { Api.id = 42 }
    | Get_user { id } -> Lwt.return { Api.id }
  in
  Api.handle { f }

(* TODO: need better routers composition *)
let ( ||| ) a b req =
  let open Lwt.Infix in
  a req >>= fun response ->
  match Dream.status response with
  | `Not_Found -> b req
  | _ -> Lwt.return response

let run () = Dream.run @@ Dream.logger @@ (pages_handler ||| api_handler)

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline (Pages.href (Hello { name = "world"; modifier = None }));
  print_endline
    (Pages.href (Hello { name = "world"; modifier = Some Uppercase }));
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
