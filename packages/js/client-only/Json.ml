module Decode = Json_decode
module Encode = Json_encode
module Util = Json_Util

exception ParserError = Util.ParserError

type t = Util.t

let stringify = Json_Util.to_string
let parse_opt s = try Some (Json_Util.parse s) with _ -> None
let parse = Json_Util.parse

module Client = struct
  let to_js_json = Json_Js_Json.to_js_json
  let of_js_json = Json_Js_Json.of_js_json
end
