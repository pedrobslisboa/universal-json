type t = Json_Util.t

let entries (dict : (string, 'a) Hashtbl.t) : (string * 'a) array =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) dict [] |> Array.of_list

let nullable encode : 'a -> t = function Some a -> encode a | _ -> `Null
let string value : t = `String value
let int value : t = `Int value
let bool value : t = `Bool value
let float value : t = `Float value

let assoc encode d : t =
  `Assoc
    (List.fold_left
       (fun acc (key, value) -> List.append acc [ (key, encode value) ])
       [] d)

let list value encode : t =
  match value with
  | [] -> `List []
  | list -> `List (List.map (fun value -> encode value) list)

let array value encode : t =
  let list = Array.to_list value in
  match list with
  | [] -> `List []
  | list -> `List (List.map (fun value -> encode value) list)

let pair value encodeA encodeB : t =
  let a, b = value in
  `List [ encodeA a; encodeB b ]

let tuple3 value encodeA encodeB encodeC : t =
  let a, b, c = value in
  `List [ encodeA a; encodeB b; encodeC c ]

let tuple4 value encodeA encodeB encodeC encodeD : t =
  let a, b, c, d = value in
  `List [ encodeA a; encodeB b; encodeC c; encodeD d ]
