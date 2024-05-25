open struct
  module Request = struct
    type t = Dream.request

    let queries = Dream.all_queries
    let body = Dream.body
    let path = Dream.target

    let method_ req =
      match Dream.method_ req with
      | `GET -> `GET
      | `POST -> `POST
      | `PUT -> `PUT
      | `DELETE -> `DELETE
      | _ -> failwith "Unsupported method"
  end

  module Response = struct
    type t = Dream.response

    let respond ~status ~headers body =
      Dream.respond ~status ~headers body
  end
end

include Ppx_deriving_router_runtime_lib.Make (Request) (Response)
