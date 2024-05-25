type 'a v = {
  data : 'a option;
  headers : (string * string) list;
  status : Dream.status option;
}

open struct
  module Request :
    Ppx_deriving_router_runtime_lib.REQUEST with type t = Dream.request =
  struct
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

  module Response :
    Ppx_deriving_router_runtime_lib.RESPONSE
      with type status = Dream.status
       and type t = Dream.response = struct
    type status = Dream.status

    let status_ok : status = `OK
    let status_bad_request : status = `Bad_Request
    let status_method_not_allowed : status = `Method_Not_Allowed
    let status_not_found : status = `Not_Found

    type t = Dream.response

    let respond ~status ~headers body =
      Dream.respond ~status ~headers body
  end

  module Return :
    Ppx_deriving_router_runtime_lib.RETURN
      with type status = Dream.status
       and type 'a t = 'a v = struct
    type status = Dream.status

    type 'a t = 'a v = {
      data : 'a option;
      headers : (string * string) list;
      status : status option;
    }

    let data x = x.data
    let status x = x.status
    let headers x = x.headers
  end
end

include Ppx_deriving_router_runtime_lib.Make (Request) (Response) (Return)

module Return = struct
  include Return

  type 'a t = 'a v

  let return ?status ?(headers = []) data =
    Lwt.return { data = Some data; headers; status }
end
