include
  Ppx_deriving_router_runtime_lib.S
    with type Request.t =
      Http.Request.t * Eio.Flow.source_ty Eio.Flow.source
     and type Response.t = Http.Response.t * Cohttp_eio.Body.t
     and type Response.status = Http.Status.t
     and type 'a Return.t = 'a
     and type 'a IO.t = 'a
