open Routing
open! Cohttp
open! Cohttp_eio

let pages_handle route _req =
  match route with
  | Pages.Home -> Server.respond_string ~status:`OK ~body:"HOME PAGE" ()
  | Route_with_implicit_path { param } ->
      let param = Option.value ~default:"-" param in
      Server.respond_string ~status:`OK
        ~body:("works as well, param is: " ^ param)
        ()
  | Route_with_implicit_path_post ->
      Server.respond_string ~status:`OK ~body:"posted" ()
  | Echo_options { options } ->
      let json = Options.to_json options in
      let json = Yojson.Basic.to_string json in
      Server.respond_string ~status:`OK ~body:json
        ~headers:
          (Http.Header.of_list [ "Content-Type", "application/json" ])
        ()
  | List_users { user_ids } ->
      let ids =
        match user_ids with
        | user_ids ->
            Printf.sprintf "[%s]"
              (user_ids |> List.map User_id.project |> String.concat ", ")
      in
      Server.respond_string ~status:`OK
        ~body:(Printf.sprintf "User ids = %s" ids)
        ()
  | User_info { user_id } | User_info_via_path { user_id } ->
      Server.respond_string ~status:`OK
        ~body:
          (Printf.sprintf "User info for %S" (User_id.project user_id))
        ()
  | Signal { level } ->
      Server.respond_string ~status:`OK
        ~body:(Printf.sprintf "Signal: %d" (Level.to_int level))
        ()
  | Hello { name; modifier; greeting } ->
      let greeting = Option.value greeting ~default:"Hello" in
      let name =
        match modifier with
        | None -> name
        | Some Uppercase -> String.uppercase_ascii name
        | Some Lowercase -> String.lowercase_ascii name
      in
      let greeting = Printf.sprintf "%s, %s!" greeting name in
      Server.respond_string ~status:`OK ~body:greeting ()

let api_handle :
    type a.
    a Api.t -> Cohttp.Request.t * Eio.Flow.source_ty Eio.Flow.source -> a
    =
 fun x _req ->
  match x with
  | Raw_response ->
      Server.respond_string ~status:`OK ~body:"RAW RESPONSE" ()
  | List_users -> []
  | Create_user { id } -> { Api.id }
  | Get_user { id } -> { Api.id }

let all_handler =
  let f :
      type a.
      a All.t ->
      Cohttp.Request.t * Eio.Flow.source_ty Eio.Flow.source ->
      a =
   fun x req ->
    match x with
    | Pages p -> pages_handle p req
    | Api e -> api_handle e req
    | Static { path } ->
        Server.respond_string ~status:`OK
          ~body:(Printf.sprintf "path=%S" path)
          ()
  in
  All.handle { f }

let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

let run () =
  let port = ref 8888 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8888 by default)" ]
    ignore "An HTTP/1.1 server";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let handler _conn req body = all_handler (req, body) in
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
  and server = Cohttp_eio.Server.make ~callback:handler () in
  Cohttp_eio.Server.run socket server ~on_error:log_warning

let pages_handler = Pages.handle pages_handle
let api_handler = Api.handle { f = api_handle }

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
      (Printf.sprintf "## %s %s" (Http.Method.to_string method_) target);
    let uri = Uri.of_string target in
    let req = Request.make ~meth:method_ uri in
    let body =
      Option.map Eio.Flow.string_source body
      |> Option.value ~default:(Eio.Flow.string_source "")
    in
    let resp, body = h (req, body) in
    let body_as_string = Eio.Flow.read_all body in
    print_endline
      (Printf.sprintf "%s: %s"
         (Cohttp.Code.string_of_status (Response.status resp))
         body_as_string)
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
