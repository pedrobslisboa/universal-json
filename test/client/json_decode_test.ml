open Jest

let _ =
  describe "Decode" (fun () ->
      let open Expect in
      test "string" (fun () ->
          expect (Json.Decode.string (`String "Melange is awesome"))
          |> toBe "Melange is awesome");
      test "int" (fun () -> expect (Json.Decode.int (`Int 1)) |> toBe 1);
      test "float" (fun () -> expect (Json.Decode.float (`Float 1.)) |> toBe 1.);
      test "bool" (fun () ->
          expect (Json.Decode.bool (`Bool true)) |> toBe true);
      test "list" (fun () ->
          expect
            (Json.Decode.list Json.Decode.int
               (`List [ `Int 1; `Int 2; `Int 3; `Int 5; `Int 7 ]))
          |> toEqual [ 1; 2; 3; 5; 7 ]);
      test "assoc" (fun () ->
          let assocJson = `Assoc [ ("name", `String "John Doe") ] in
          expect (Json.Decode.assoc Json.Decode.string assocJson)
          |> toEqual [ ("name", "John Doe") ]);
      test "at" (fun () ->
          let strJson =
            `Assoc
              [
                ( "name",
                  `Assoc [ ("first", `String "John"); ("last", `String "Doe") ]
                );
              ]
          in
          expect
            (strJson |> Json.Decode.at [ "name"; "last" ] Json.Decode.string)
          |> toEqual "Doe");
      test "at_raise" (fun () ->
          let strJson =
            `Assoc
              [
                ( "name",
                  `Assoc [ ("first", `String "John"); ("last", `String "Doe") ]
                );
              ]
          in
          expect (fun () ->
              strJson |> Json.Decode.at [ "name"; "second" ] Json.Decode.string)
          |> toThrow);
      test "one_of" (fun () ->
          let strJson = `String "Melange is awesome" in
          expect
            (strJson
            |> Json.Decode.one_of
                 [ Json.Decode.(field "name" string); Json.Decode.string ])
          |> toBe "Melange is awesome");
      test "one_of_raise" (fun () ->
          let strJson = `Assoc [ ("last_name", `String "John Doe") ] in
          expect (fun () ->
              strJson
              |> Json.Decode.one_of
                   [ Json.Decode.(field "name" string); Json.Decode.string ])
          |> toThrow);
      test "either" (fun () ->
          let strJson = `String "Melange is Awesome" in
          expect
            (strJson
            |> Json.Decode.either
                 Json.Decode.(field "name" string)
                 Json.Decode.string)
          |> toBe "Melange is Awesome");
      test "either_raise" (fun () ->
          let strJson = `Assoc [ ("last_name", `String "John Doe") ] in
          expect (fun () ->
              strJson
              |> Json.Decode.either
                   Json.Decode.(field "name" string)
                   Json.Decode.string)
          |> toThrow);
      test "map" (fun () ->
          expect ((Json.Decode.map (( + ) 2)) Json.Decode.int (`Int 23))
          |> toBe 25);
      test "assoc" (fun () ->
          let assocJson = `Assoc [ ("name", `String "John Doe") ] in
          expect (Json.Decode.assoc Json.Decode.string assocJson)
          |> toEqual [ ("name", "John Doe") ]);
      test "and_then" (fun () ->
          let strJson =
            `Assoc
              [ ("first_name", `String "John"); ("last_name", `String "Doe") ]
          in
          let lastName =
            strJson
            |> Json.Decode.and_then
                 (function
                   | "John" -> Json.Decode.field "last_name" Json.Decode.string
                   | _ -> Json.Decode.field "first_name" Json.Decode.string)
                 (Json.Decode.field "first_name" Json.Decode.string)
          in
          expect lastName |> toBe "Doe"))
