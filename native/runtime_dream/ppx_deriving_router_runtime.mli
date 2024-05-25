type 'a v = {
  data : 'a option;
  headers : (string * string) list;
  status : Dream.status option;
}

include
  Ppx_deriving_router_runtime_lib.S
    with type Request.t = Dream.request
     and type Response.t = Dream.response
     and type Response.status = Dream.status
     and type 'a Return.t = 'a v

module Return : sig
  include Ppx_deriving_router_runtime_lib.RETURN with type 'a t = 'a v

  val return :
    ?status:Dream.status ->
    ?headers:(string * string) list ->
    'a ->
    'a return Lwt.t
end
