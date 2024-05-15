open Genlex

exception ParserError of string

type t =
  [ `Assoc of (string * t) list
  | `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `List of t list
  | `Null ]

let lexer = make_lexer [ "true"; "false"; "null"; "{"; "}"; "["; "]"; ":"; "," ]

let rec parseJson stream =
  match Stream.peek stream with
  | Some (Kwd "{") -> parseObject stream
  | Some (Kwd "[") -> parseArray stream
  | Some (String s) ->
      Stream.junk stream;
      `String s
  | Some (Int i) ->
      Stream.junk stream;
      `Int i
  | Some (Float f) ->
      Stream.junk stream;
      `Float f
  | Some (Kwd "true") ->
      Stream.junk stream;
      `Bool true
  | Some (Kwd "false") ->
      Stream.junk stream;
      `Bool false
  | Some (Kwd "null") ->
      Stream.junk stream;
      `Null
  | _ -> raise (ParserError "Unexpected token in JSON input")

and parseObject stream : t =
  Stream.junk stream;
  let rec aux acc =
    match Stream.peek stream with
    | Some (Kwd "}") ->
        Stream.junk stream;
        `Assoc (List.rev acc)
    | _ -> (
        let key =
          match Stream.next stream with
          | String s -> s
          | _ -> raise (ParserError "Expected string key in JSON object")
        in
        (match Stream.next stream with
        | Kwd ":" -> ()
        | _ -> raise (ParserError "Expected colon in JSON object"));
        let value = parseJson stream in
        match Stream.peek stream with
        | Some (Kwd ",") ->
            Stream.junk stream;
            aux ((key, value) :: acc)
        | Some (Kwd "}") ->
            Stream.junk stream;
            `Assoc (List.rev ((key, value) :: acc))
        | _ -> raise (ParserError "Expected comma or closing brace in object"))
  in
  aux []

and parseArray stream =
  Stream.junk stream;
  let rec aux acc =
    match Stream.peek stream with
    | Some (Kwd "]") ->
        Stream.junk stream;
        `List (List.rev acc)
    | _ -> (
        let value = parseJson stream in
        match Stream.peek stream with
        | Some (Kwd ",") ->
            Stream.junk stream;
            aux (value :: acc)
        | Some (Kwd "]") ->
            Stream.junk stream;
            `List (List.rev (value :: acc))
        | _ -> raise @@ ParserError "Expected comma or closing bracket in array"
        )
  in
  aux []

let parse str : t =
  let stream = Stream.of_string str in
  let stream = lexer stream in
  try parseJson stream
  with ParserError err -> raise @@ ParserError ("JSON parsing error: " ^ err)

let rec to_string_ (value : t) offset =
  match value with
  | `String string -> Printf.sprintf "\"%s\"" string
  | `Int int -> int |> Int.to_string
  | `Float float -> float |> Float.to_string
  | `List list ->
      Printf.sprintf "[%s]"
        (List.fold_left
           (fun acc value ->
             match acc with
             | "" -> to_string_ value (offset + 4)
             | _ -> String.concat "," [ acc; to_string_ value (offset + 4) ])
           "" list)
  | `Bool bool -> bool |> Bool.to_string
  | `Assoc assoc ->
      Printf.sprintf "{%s}"
        (List.fold_left
           (fun acc (key, value) ->
             match acc with
             | "" ->
                 Printf.sprintf "\"%s\":%s" key (to_string_ value (offset + 4))
             | _ ->
                 String.concat ","
                   [
                     acc;
                     Printf.sprintf "\"%s\":%s" key
                       (to_string_ value (offset + 4));
                   ])
           "" assoc)
  | `Null -> "null"

let rec show_ (value : t) offset =
  match value with
  | `String string -> Printf.sprintf "`String (\"%s\")" string
  | `Int int -> Printf.sprintf "`Int (%s)" (int |> Int.to_string)
  | `Float float -> Printf.sprintf "`Float (%s)" (float |> Float.to_string)
  | `List list ->
      Printf.sprintf "`List ([%s])"
        (List.fold_left
           (fun acc value ->
             match acc with
             | "" ->
                 Printf.sprintf "\n%s(%s)" (String.make offset ' ')
                   (show_ value (offset + 4))
             | _ ->
                 String.concat ","
                   [
                     acc;
                     Printf.sprintf "\n%s(%s)" (String.make offset ' ')
                       (show_ value (offset + 4));
                   ])
           "" list)
  | `Bool bool -> Printf.sprintf "`Bool (%s)" (bool |> Bool.to_string)
  | `Assoc assoc ->
      Printf.sprintf "`Assoc ([%s])"
        (List.fold_left
           (fun acc (key, value) ->
             match acc with
             | "" ->
                 Printf.sprintf "\n%s(\"%s\" , (%s))" (String.make offset ' ')
                   key
                   (show_ value (offset + 4))
             | _ ->
                 String.concat ";"
                   [
                     acc;
                     Printf.sprintf "\n%s(\"%s\" , (%s))"
                       (String.make offset ' ') key
                       (show_ value (offset + 4));
                   ])
           "" assoc)
  | `Null -> "`Null"

let to_string value = to_string_ value 2
let show value = show_ value 2

let typeof : t -> string = function
  | `String _ -> "string"
  | `Int _ -> "int"
  | `Float _ -> "float"
  | `List _ -> "list"
  | `Bool _ -> "bool"
  | `Assoc _ -> "object"
  | `Null -> "null"
