include
  Ppx_deriving_router_runtime_lib.S
    with type 'a IO.t = 'a Lwt.t
     and type Request.t = Dream.request
     and type Response.t = Dream.response
     and type Response.status = Dream.status
     and type 'a Return.t = 'a
