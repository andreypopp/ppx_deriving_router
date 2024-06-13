let string_to_url_path x = x
let string_of_url_path x = Some x
let int_to_url_path x = string_of_int x
let int_of_url_path x = int_of_string_opt x
let bool_to_url_path x = if x then "true" else "false"

let bool_of_url_path x =
  match x with "true" -> Some true | "false" -> Some false | _ -> None

let rec last_wins acc k xs f =
  match xs with
  | (k', x) :: xs when String.equal k k' -> last_wins (Some x) k xs f
  | _ :: xs -> last_wins acc k xs f
  | [] -> f acc

let last_wins k xs f = last_wins None k xs f
let string_to_url_query k x = [ k, x ]

let string_of_url_query k xs =
  last_wins k xs (function
    | None -> Error "missing value"
    | Some x -> Ok x)

let float_to_url_query k x = [ k, string_of_float x ]

let float_of_url_query k xs =
  last_wins k xs (function
    | None -> Error "missing value"
    | Some x -> (
        match float_of_string_opt x with
        | None ->
            Error
              (Printf.sprintf
                 "expected a float value, instead received: %s" x)
        | Some x -> Ok x))

let int_to_url_query k x = [ k, string_of_int x ]

let int_of_url_query k xs =
  last_wins k xs (function
    | None -> Error "missing value"
    | Some x -> (
        match int_of_string_opt x with
        | None ->
            Error
              (Printf.sprintf
                 "expected an integer value, instead received: %s" x)
        | Some x -> Ok x))

let bool_to_url_query k x = if x then [ k, "true" ] else [ k, "false" ]

let bool_of_url_query k xs =
  last_wins k xs (function
    | None -> Error "missing value"
    | Some "true" -> Ok true
    | Some "false" -> Ok false
    | Some v ->
        Error
          (Printf.sprintf
             "expected a boolean value (true, false), instead received: \
              %s"
             v))

let option_to_url_query f k x =
  match x with None -> [] | Some v -> f k v

let option_of_url_query f k xs =
  last_wins k xs (function
    | None -> Ok None
    | Some v -> (
        match f k [ k, v ] with
        | Ok v -> Ok (Some v)
        | Error err -> Error err))

let list_to_url_query f k xs = List.concat_map (f k) xs

let list_of_url_query f k xs =
  let rec loop acc k xs f =
    match xs with
    | (k', x) :: xs when String.equal k k' -> (
        match f k [ k, x ] with
        | Error _ as e -> e
        | Ok r -> loop (r :: acc) k xs f)
    | _ :: xs -> loop acc k xs f
    | [] -> Ok (List.rev acc)
  in
  loop [] k xs f
