open struct
  module Request :
    Ppx_deriving_router_runtime_lib.REQUEST
      with type t = Cohttp_lwt_unix.Request.t * Cohttp_lwt.Body.t = struct
    type t = Cohttp_lwt_unix.Request.t * Cohttp_lwt.Body.t

    let queries (request, _body) =
      (* TODO: queries in Cohttp contains (string * string list) list while router expects (string * string) list *)
      let _ = request |> Cohttp_lwt_unix.Request.uri |> Uri.query in
      []

    let body ((_request, body): t) = Cohttp_lwt.Body.to_string body
    let path (request, _body) = Cohttp_lwt_unix.Request.resource request

    let method_ (request, _body) =
      match Cohttp_lwt_unix.Request.meth request with
      | `GET -> `GET
      | `POST -> `POST
      | `PUT -> `PUT
      | `DELETE -> `DELETE
      | `HEAD -> failwith "HEAD is not supported"
      | `PATCH -> failwith "PATCH is not supported"
      | `OPTIONS -> failwith "OPTIONS is not supported"
      | `TRACE -> failwith "TRACE is not supported"
      | `CONNECT -> failwith "CONNECT is not supported"
      | `Other other -> failwith (Printf.sprintf "%s is not supported" other)
  end

  module Response :
    Ppx_deriving_router_runtime_lib.RESPONSE
      with type t = Cohttp_lwt_unix.Response.t * Cohttp_lwt.Body.t
       and type status = Cohttp.Code.status_code = struct
    type t = Cohttp_lwt_unix.Response.t * Cohttp_lwt.Body.t
    type status = Cohttp.Code.status_code

    let status_ok : status = `OK
    let status_not_found : status = `Not_found
    let status_bad_request : status = `Bad_request
    let status_method_not_allowed : status = `Method_not_allowed

    let respond ~status ~headers body : t Lwt.t =
      let headers = Cohttp.Header.of_list headers in
      Cohttp_lwt_unix.Server.respond_string ~body ~status ~headers ()
  end

  module Return :
    Ppx_deriving_router_runtime_lib.RETURN
      with type status = Cohttp.Code.status_code
       and type 'a t = 'a = struct
    type 'a t = 'a
    type status = Cohttp.Code.status_code

    let data x = Some x
    let status _ = None
    let headers _ = []
  end
end

include Ppx_deriving_router_runtime_lib.Make (Request) (Response) (Return)
