module Decode = Json_decode
module Encode = Json_encode
module Util = Json_Util

exception ParserError of string

type t = Util.t

val stringify : Json_Util.t -> string
val parse_opt : string -> Json_Util.t option
val parse : string -> Json_Util.t

module Client : sig
  val to_js_json : t -> Js.Json.t
  val of_js_json : Js.Json.t -> t
end
