module Decode = Json_decode
module Encode = Json_encode
module Util = Json_Util

exception ParserError = Util.ParserError

type t = Util.t

let stringify = Json_Util.to_string
let parse_opt s = try Some (Json_Util.parse s) with _ -> None
let parse value = Json_Util.parse value

module Native = struct
  let of_yojson = Json_YoJson_Basic.of_yojson
  let to_yojson = Json_YoJson_Basic.to_yojson
end
