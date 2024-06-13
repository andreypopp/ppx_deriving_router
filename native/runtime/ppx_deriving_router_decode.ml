type queries = (string * string) list
type 'a decode_url_path = string -> 'a option
type 'a decode_url_query = string -> queries -> ('a, string) result

let decode_path x = Uri.pct_decode x
let decode_query_key x = Uri.pct_decode x
let decode_query_value x = Uri.pct_decode x
