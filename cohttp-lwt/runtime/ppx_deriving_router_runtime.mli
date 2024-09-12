include
  Ppx_deriving_router_runtime_lib.S
    with type 'a IO.t = 'a Lwt.t
     and type Request.t = Cohttp_lwt_unix.Request.t * Cohttp_lwt.Body.t
     and type Response.t = Cohttp_lwt_unix.Response.t * Cohttp_lwt.Body.t
     and type Response.status = Cohttp.Code.status_code
     and type 'a Return.t = 'a
