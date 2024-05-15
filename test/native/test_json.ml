let parse () =
  let json = Json.parse_opt "{\"name\":\"John Doe\",\"age\":30}" in
  match json with
  | Some (`Assoc [ ("name", `String a); ("age", `Int 30) ]) ->
      Alcotest.(check @@ string) "should be equal" "John Doe" a
  | _ -> Alcotest.fail "Ops"

let parseNone () =
  let json = Json.parse_opt "ops" in
  match json with
  | None -> Alcotest.(check @@ pass) "Should be None" () ()
  | _ -> Alcotest.fail "ops"

let parseOrRaise () =
  let json = Json.parse "{ \"name\": \"John Doe\" }" in
  match json with
  | `Assoc [ ("name", `String a) ] ->
      Alcotest.(check @@ string) "should be equal" "John Doe" a
  | _ -> Alcotest.fail "Ops"

let parseOrRaiseExn () =
  Alcotest.check_raises "should be equal"
    (Json.ParserError "JSON parsing error: Unexpected token in JSON input")
    (fun () ->
      let _ = Json.parse "" in
      ())

let stringify () =
  let jsonString =
    `Assoc [ ("name", `String "John Doe"); ("age", `Int 30) ] |> Json.stringify
  in
  Alcotest.(check @@ string)
    "should be equal" "{\"name\":\"John Doe\",\"age\":30}" jsonString

let case title fn = Alcotest.test_case title `Quick fn

let tests =
  ( "Json",
    [
      case "Json parse" parse;
      case "Json parseNone" parseNone;
      case "Json parseOrRaise" parseOrRaise;
      case "Json parseOrRaiseExn" parseOrRaiseExn;
      case "Json stringify" stringify;
    ] )
