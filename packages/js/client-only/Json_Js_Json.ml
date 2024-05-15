let rec to_js_json (value : Json_Util.t) : Js.Json.t =
  match value with
  | `Null -> Js.Json.null
  | `Bool bool -> Js.Json.boolean bool
  | `Int int -> Js.Json.number (float_of_int int)
  | `Float float -> Js.Json.number float
  | `String string -> Js.Json.string string
  | `List list -> Js.Json.array (Array.map to_js_json (Array.of_list list))
  | `Assoc assoc ->
      Js.Json.object_
        (Js.Dict.fromList (List.map (fun (k, v) -> (k, to_js_json v)) assoc))

let of_js_json (value : Js.Json.t) = Js.Json.stringify value |> Json_Util.parse
