exception DecodeError of string

let decodeError expect got =
  DecodeError
    (Printf.sprintf "Expected %s, got %s" expect (Json_Util.typeof got))

let to_list value =
  match value with `List list -> list | a -> raise (decodeError "list" a)

let string value =
  match value with `String str -> str | a -> raise (decodeError "string" a)

let int value =
  match value with `Int int -> int | a -> raise (decodeError "int" a)

let float value =
  match value with `Float float -> float | a -> raise (decodeError "float" a)

let bool value =
  match value with `Bool bool -> bool | a -> raise (decodeError "bool" a)

let list decodeFn value = to_list value |> List.map decodeFn
let char value = String.get (value |> string) 0
let nullable decode = function `Null -> None | e -> Some (decode e)
let array decodeFn value = value |> list decodeFn |> Array.of_list

let pair decodeFnA decodeFnB value =
  let pairList = value |> to_list in
  let length = List.length pairList in
  if length = 2 then
    try (decodeFnA (List.nth pairList 0), decodeFnB (List.nth pairList 1))
    with DecodeError msg -> raise @@ DecodeError (msg ^ "\n\tin pair/tuple2")
  else
    raise
      (DecodeError
         (Printf.sprintf "Expected array of length 2, got array of length %s"
            (length |> Int.to_string)))

let tuple3 decodeFnA decodeFnB decodeFnC value =
  let tuple3List = value |> to_list in
  let length = List.length tuple3List in
  if length = 3 then
    try
      ( decodeFnA (List.nth tuple3List 0),
        decodeFnB (List.nth tuple3List 1),
        decodeFnC (List.nth tuple3List 2) )
    with DecodeError msg -> raise @@ DecodeError (msg ^ "\n\tin tuple3")
  else
    raise
      (DecodeError
         (Printf.sprintf "Expected array of length 3, got array of length %s"
            (length |> Int.to_string)))

let tuple4 decodeFnA decodeFnB decodeFnC decodeFnD value =
  let tuple4List = value |> to_list in
  let length = List.length tuple4List in
  if length = 4 then
    try
      ( decodeFnA (List.nth tuple4List 0),
        decodeFnB (List.nth tuple4List 1),
        decodeFnC (List.nth tuple4List 2),
        decodeFnD (List.nth tuple4List 3) )
    with DecodeError msg -> raise @@ DecodeError (msg ^ "\n\tin tuple3")
  else
    raise
      (DecodeError
         (Printf.sprintf "Expected array of length 4, got array of length %s"
            (length |> Int.to_string)))

let field fieldName decoder json =
  match json with
  | `Assoc values -> (
      match List.find_opt (fun (key, _) -> key == fieldName) values with
      | Some (key, value) -> (
          try decoder value
          with DecodeError msg ->
            raise @@ DecodeError (msg ^ "\n\tat field '" ^ key ^ "'"))
      | None ->
          raise @@ DecodeError (Printf.sprintf "Expected field '%s'" fieldName))
  | a -> raise (decodeError "object" a)

let with_default default decode json =
  try decode json with DecodeError _ -> default

let assoc decode json =
  match json with
  | `Assoc assocList ->
      List.map (fun (key, value) -> (key, decode value)) assocList
  | a -> raise (decodeError "object" a)

let rec at key_path decoder =
  match key_path with
  | [ key ] -> field key decoder
  | first :: rest -> field first (at rest decoder)
  | [] ->
      raise
      @@ Invalid_argument "Expected key_path to contain at least one element"

let one_of decoders json =
  let rec inner decoders errors =
    match decoders with
    | [] ->
        let formattedErrors = "\n- " ^ String.concat "\n- " (List.rev errors) in
        raise
        @@ DecodeError
             (Printf.sprintf
                "All decoders given to one_of failed. Here are all the errors: \
                 %s\n\
                 And the JSON being decoded: %s" formattedErrors
                (Json_Util.to_string json))
    | decode :: rest -> (
        try decode json with DecodeError e -> inner rest (e :: errors))
  in
  inner decoders []

let either a b = one_of [ a; b ]
let map f decode json = f (decode json)
let and_then b a json = b (a json) json
