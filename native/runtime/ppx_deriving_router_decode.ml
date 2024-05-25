type queries = (string * string) list
type 'a decode_url_path = string -> 'a option
type 'a decode_url_query = string -> queries -> ('a, string) result
