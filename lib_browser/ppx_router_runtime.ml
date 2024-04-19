type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

module Types = struct
  let string_to_url_path x = x
  let string_of_url_path x = Some x
  let int_to_url_path x = string_of_int x
  let int_of_url_path x = int_of_string_opt x
  let bool_to_url_path x = if x then "true" else "false"

  let bool_of_url_path x =
    match x with "true" -> Some true | "false" -> Some false | _ -> None

  let rec last_wins f = function
    | [] -> None
    | [ x ] -> f x
    | _ :: xs -> last_wins f xs

  let string_to_url_query x = [ x ]
  let string_of_url_query = last_wins (fun x -> Some x)
  let int_to_url_query x = [ string_of_int x ]
  let int_of_url_query = last_wins int_of_string_opt
  let bool_to_url_query x = if x then [ "true" ] else []

  let bool_of_url_query =
    last_wins (function "true" -> Some true | _ -> Some false)

  let option_to_url_query :
      'a url_query_encoder -> 'a option url_query_encoder =
   fun f x -> match x with None -> [] | Some v -> f v

  let option_of_url_query :
      'a url_query_decoder -> 'a option url_query_decoder =
   fun f x ->
    match x with
    | [] -> Some None
    | x -> ( match f x with None -> None | Some v -> Some (Some v))
end

let encode_path out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)

let encode_query_key out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)

let encode_query_value out x =
  Buffer.add_string out (Js.Global.encodeURIComponent x)
