include
  Ppx_deriving_router_runtime_lib.S
    with type Request.t = Dream.request
     and type Response.t = Dream.response
