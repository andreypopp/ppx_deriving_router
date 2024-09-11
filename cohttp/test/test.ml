open Routing
open Lwt.Infix

module Server = Cohttp_lwt_unix.Server

let respond body = Server.respond_string ~status:`OK ~body ()

let respond_json body =
  Server.respond_string
    ~status:`OK
    ~headers:(Cohttp.Header.of_list [ "Content-Type", "application/json" ])
    ~body ()

let pages_handle route _req =
  match route with
  | Pages.Home -> respond "HOME PAGE"
  | Route_with_implicit_path { param } ->
      let param = Option.value ~default:"-" param in
      respond ("works as well, param is: " ^ param)
  | Route_with_implicit_path_post -> respond "posted"
  | Echo_options { options } ->
      let json = Options.to_json options in
      let json = Yojson.Basic.to_string json in
      respond json
  | List_users { user_ids } ->
      let ids =
        match user_ids with
        | user_ids ->
            Printf.sprintf "[%s]"
              (user_ids |> List.map User_id.project |> String.concat ", ")
      in
      respond (Printf.sprintf "User ids = %s" ids)
  | User_info { user_id } | User_info_via_path { user_id } ->
      respond
        (Printf.sprintf "User info for %S" (User_id.project user_id))
  | Signal { level } ->
      respond (Printf.sprintf "Signal: %d" (Level.to_int level))
  | Hello { name; modifier; greeting } ->
      let greeting = Option.value greeting ~default:"Hello" in
      let name =
        match modifier with
        | None -> name
        | Some Uppercase -> String.uppercase_ascii name
        | Some Lowercase -> String.lowercase_ascii name
      in
      let greeting = Printf.sprintf "%s, %s!" greeting name in
      respond greeting

let pages_handler = Pages.handle pages_handle

let api_handle :
    type a.
    a Api.t -> .Request.t -> a Ppx_deriving_router_runtime.return Lwt.t
    =
 fun x _req ->
  match x with
  | Raw_response -> respond "RAW RESPONSE"
  | List_users -> Lwt.return []
  | Create_user { id } -> Lwt.return { Api.id }
  | Get_user { id } -> Lwt.return { Api.id }

let api_handler (* : Dream.handler *) = Api.handle { f = api_handle }

let all_handler (* : Dream.handler *) =
  let f :
      type a.
      a All.t ->
      Cohttp.Request.t ->
      a Ppx_deriving_router_runtime.return Lwt.t =
   fun x req ->
    match x with
    | Pages p -> pages_handle p req
    | Api e -> api_handle e req
    | Static { path } -> respond (Printf.sprintf "path=%S" path)
  in
  All.handle { f }

let run () =
  let callback _conn req _body = all_handler req in
  Lwt_main.run (Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ()))

let test () =
  print_endline "# TESTING HREF GENERATION";
  print_endline (Pages.href Home);
  print_endline (Pages.href (Route_with_implicit_path { param = None }));
  print_endline
    (Pages.href (Route_with_implicit_path { param = Some "ok" }));
  print_endline
    (Pages.href
       (Hello { name = "world"; modifier = None; greeting = None }));
  print_endline
    (Pages.href
       (Hello
          { name = "world"; modifier = Some Uppercase; greeting = None }));
  print_endline (Api.href (Get_user { id = 121 }));
  print_endline
    (All.href
       (Pages (Hello { name = "world"; modifier = None; greeting = None })));
  print_endline (All.href (Api (Get_user { id = 121 })));
  print_endline
    (Pages.href (User_info { user_id = User_id.inject "username" }));
  print_endline
    (Pages.href
       (User_info_via_path { user_id = User_id.inject "username" }));
  print_endline (Pages.href (Signal { level = Warning }));
  print_endline
    (Pages.href
       (List_users
          { user_ids = [ User_id.inject "u1"; User_id.inject "u2" ] }));
  print_endline (Pages.href (List_users { user_ids = [] }));
  print_endline (All.href (Static { path = "/js/main.js" }));
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
  test_req pages_handler `GET "/Echo_options?options={a:42}";
  test_req pages_handler `GET "/User_info?user_id=username";
  test_req pages_handler `GET "/user/username_via_path";
  test_req pages_handler `GET "/Signal?level=2";
  test_req pages_handler `GET "/List_users?user_ids=u1&user_ids=u2";
  test_req pages_handler `GET "/List_users";
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
  test_req all_handler `GET "/nested/api/121";
  test_req pages_handler `GET
    "/hello/pct%20encoded?greeting=pct%20encoded";
  test_req all_handler `GET "/static/js/main.js"

let () =
  match Sys.argv.(1) with
  | exception Invalid_argument _ -> run ()
  | "run" -> run ()
  | "test" -> test ()
  | _ ->
      prerr_endline "unknown subcommand";
      exit 1
