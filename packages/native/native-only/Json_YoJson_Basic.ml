(* Json.Util.t and Yojson.Basic.t are the same type, so we can convert between
   them without any loss of information. *)
(* Ideally, we would map Json.Util.t to Yojson.Basic.t in the type definitions,
   in the future we will do this. *)
let to_yojson (value : Json_Util.t) : Yojson.Basic.t = value
let of_yojson (value : Yojson.Basic.t) : Json_Util.t = value
