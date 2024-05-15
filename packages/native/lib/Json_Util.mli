exception ParserError of string

type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string ]

val parse : string -> t
val show : t -> string
val typeof : t -> string
val to_string : t -> string
