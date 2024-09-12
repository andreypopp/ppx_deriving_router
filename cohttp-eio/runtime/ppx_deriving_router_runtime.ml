open struct
  module IO : Ppx_deriving_router_runtime_lib.IO with type 'a t = 'a =
  struct
    type 'a t = 'a

    let return = Fun.id
    let fail exn = raise exn
    let bind x f = f x
    let catch f = try Ok (f ()) with exn -> Error exn
  end

  module Request :
    Ppx_deriving_router_runtime_lib.REQUEST
      with type 'a IO.t = 'a IO.t
       and type t = Http.Request.t * Eio.Flow.source_ty Eio.Flow.source =
  struct
    module IO = IO

    type t = Http.Request.t * Eio.Flow.source_ty Eio.Flow.source

    let queries (req, _body) =
      let uri = Cohttp.Request.uri req in
      Uri.query uri
      |> List.map (fun (k, vs) -> List.map (fun v -> k, v) vs)
      |> List.flatten

    let body ((_req, body) : t) = Eio.Flow.read_all body

    let path (req, _body) =
      let uri = Cohttp.Request.uri req in
      Uri.path uri

    let method_ (req, _body) =
      match req.Http.Request.meth with
      | `GET -> `GET
      | `POST -> `POST
      | `PUT -> `PUT
      | `DELETE -> `DELETE
      | _ -> failwith "Unsupported method"
  end

  module Response :
    Ppx_deriving_router_runtime_lib.RESPONSE
      with type 'a IO.t = 'a IO.t
       and type status = Http.Status.t
       and type t = Http.Response.t * Cohttp_eio.Body.t = struct
    module IO = IO

    type status = Http.Status.t

    let status_ok : status = `OK
    let status_bad_request : status = `Bad_request
    let status_method_not_allowed : status = `Method_not_allowed
    let status_not_found : status = `Not_found

    type t = Http.Response.t * Cohttp_eio.Body.t

    let respond ~status ~headers body =
      let headers = Http.Header.of_list headers in
      Cohttp_eio.Server.respond_string ~headers ~status ~body ()
  end

  module Return :
    Ppx_deriving_router_runtime_lib.RETURN
      with type status = Http.Status.t
       and type 'a t = 'a = struct
    type status = Http.Status.t
    type 'a t = 'a

    let data x = Some x
    let status _ = None
    let headers _ = []
  end
end

include Ppx_deriving_router_runtime_lib.Make (Request) (Response) (Return)
