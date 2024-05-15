## Universal Ocaml Json encode/decode

Universal Json for both natively Ocaml/Reason and Js apps.

This project was inspired by [melange-json](https://github.com/melange-community/melange-json) and [YoJson](https://github.com/ocaml-community/yojson)

This library can be used on both melange an native ocaml without external contract dependency.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
    - [Encode](#encode)
        - [API](#api)
        - [Examples](#examples)
    - [Decode](#decode)
        - [API](#api-1)
        - [Examples](#examples-1)
    - [Interop](#interop)
        - [Js.Json (Client only)](#jsjson-client-only)
        - [Yojson (Native only)](#yojson-native-only)

## Installation

There is no opam version yet, so you can install it by pinning the repository:
```sh
opam pin add universal-json.dev "git+https://github.com/pedrobslisboa/universal-json#main"
```

#### For client dune config add

```dune
 (libraries universal-json.js)
```

#### For native dune config add

```dune
 (libraries universal-json.native)
```


## Usage

This library doesn't use the native Json parsing and stringfy libraries, it has its own implementation, the code is strongly typed and can build the json endode without calling the Json encode function.

### Json

#### API

```ocaml
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Assoc of (string * t) list

val parse : string -> t option
(* returns None if the input is not a valid JSON string *)
val parseOrRaise : string -> t
(* raises Json.ParseError with the error message if the input is not a valid JSON string *)
val stringify : t -> string
```

### Encode

#### API

```ocaml
module Encode : sig
    val string : string -> Json.t
    val int : int -> Json.t
    val float : float -> Json.t
    val bool : bool -> Json.t
    val null : Json.t
    val list : Json.t list -> Json.t
    val assoc : (string * Json.t) list -> Json.t
    val object_ : (string * Json.t) list -> Json.t
end
```

#### Examples

You can easily build a json string by using the Json.Encode module:

```ocaml
(* val json : Json.t *)
let json = `Assoc [
    ("name", `String "John");
    ("age", `Int 30);
    ("grades", `List [`Int 90; `Int 80; `Int 70]);
    ("address", `Assoc [
        ("street", `String "123 Main St");
        ("city", `String "Sometown");
        ("state", `String "Sp");
        ("zip", `String "12345");
    ]);
]

(* is the same of *)

let json = Json.Encode.(
    assoc [
        "name", string "John";
        "age", int 30;
        "grades", list [int 90; int 80; int 70];
        "address", object_ [
            "street", string "123 Main St";
            "city", string "Sometown";
            "state", string "Sp";
            "zip", string "12345";
        ];
    ]
)
```

### Decode

#### API

```ocaml
module Decode : sig
    val string : Json.t -> string
    val int : Json.t -> int
    val float : Json.t -> float
    val bool : Json.t -> bool
    val list : ('a -> 'b) -> Json.t -> 'b list
    val assoc : ('a -> Json.t) -> (string * 'a) list -> Json.t
    val at : string list -> ('a -> Json.t) -> 'a -> 'b
    val field : string -> ('a -> Json.t) -> 'a -> 'b
    val one_of : (Json.t -> 'a) list -> Json.t -> 'a
    val nullable : ('a -> 'b) -> Json.t -> 'b option
    val either : (Json_Util.t -> 'a) -> (Json_Util.t -> 'a) -> Json_Util.t -> 'a
    val map : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val and_then : ('a -> 'b -> 'c) -> ('b -> 'a) -> 'b -> 'c
    val with_default : 'a -> ('b -> 'a) -> 'b -> 'a
'a
end
```

#### Examples

To decode a json string

```ocaml
type person = {
    name: string;
    age: int;
    grades: int list;
    address: {
        street: string;
        city: string;
        state: string;
        zip: string;
    };
}

(* val json : Json.t *)
let json = Json.parseOrRaise {|{"name": "John", "age": 30, "grades": [90, 80, 70], "address": {"street": "123 Main St", "city": "Sometown", "state": "Sp", "zip": "12345"}}|}

(* val decode_person : Json.t -> person *)
let decode_person json =
    let open Json.Decode in
    let name = field "name" string json in
    let age = field "age" int json in
    let grades = field "grades" (list int) json in
    let address = field "address" (fun json ->
        let open Json.Decode in
        let street = at ["address", "street"] string json in
        let city =  at ["address", "city"] string json in
        let state =  at ["address", "state"] string json in
        let zip =  at ["address", "zip"] string json in
        {street; city; state; zip}
    ) json in

    {name; age; grades; address}
```

You can also build your decoded hashtable by using assoc and then set it to any hashtable tool:

to Hashtbl (Universal)

```ocaml
(* val json : Json.t *)
let json = Json.parseOrRaise {|{"name": "John", "age": 30, "grades": [90, 80, 70], "address": {"street": "123 Main St", "city": "Sometown", "state": "Sp", "zip": "12345"}}|}

(* val assoc : Json.t -> (string, 'a) list *)
(* val decode_person : Json.t -> (string, string) Hashtbl.t *)
let decode_person json =
  let open Decode in
  let person = assoc string json in
  let target = Hashtbl.create @@ List.length person in
  List.fold_left
    (fun acc (key, value) ->
      Hashtbl.replace acc key value;
      target)
    target person
```

to Js.Dict (Universal with server-reason-react.js)

```ocaml
(* val json : Json.t *)
let json = Json.parseOrRaise {|{"name": "John", "age": 30, "grades": [90, 80, 70], "address": {"street": "123 Main St", "city": "Sometown", "state": "Sp", "zip": "12345"}}|}

(* val decode_person : Json.t -> string Js.Dict.t *)
let decode_person json =
  let open Decode in
  let person = assoc string json in
  Js.Dict.fromList person
```

### Interop


The code is prepared to convert safely from Js.Json to Json and vice versa, and also from Yojson to Json and vice versa:

#### Js.Json (Client only)

```ocaml
(* val json : Json.t *)
let json = Json.parseOrRaise {|{"name": "John", "age": 30}|}

(* val to_js_json : Json.t -> Js.Json.t *)
(* val js_json : Js.Json.t *)
let js_json = Json.Client.to_js_json json

(* val of_js_json : Js.Json.t -> Json.t *)
(* val json : Json.t *)
let json = Json.Client.of_js_json js_json
```

#### Yojson (Native only)

```ocaml
(* val json : Json.t *)
let json = Json.parseOrRaise {|{"name": "John", "age": 30}|}

(* val to_yojson : Json.t -> Yojson.Safe.json *)
(* val yojson : Yojson.Safe.json *)
let yojson = Json.Native.to_yojson json

(* val of_yojson : Yojson.Safe.json -> Json.t *)
(* val json : Json.t *)
let json = Json.Native.of_yojson yojson
```
