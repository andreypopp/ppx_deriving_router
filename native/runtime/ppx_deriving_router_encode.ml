type queries = (string * string) list
type 'a encode_url_path = 'a -> string
type 'a encode_url_query = string -> 'a -> queries

let encode_path out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Path x)

let encode_query_key out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_key x)

let encode_query_value out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_value x)
