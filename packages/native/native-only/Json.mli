module Decode = Json_decode
module Encode = Json_encode
module Util = Json_Util

exception ParserError of string

type t = Util.t

val stringify : Json_Util.t -> string
val parse_opt : string -> Json_Util.t option
val parse : string -> Json_Util.t

module Native : sig
  val of_yojson : Yojson.Basic.t -> t
  val to_yojson : t -> Yojson.Basic.t
end
