open Jest

let expectFail expect (got : Json.t) =
  let expectFailStr = Printf.sprintf "Expect %s, got %s" expect in
  match got with
  | `List _ -> expectFailStr "`List"
  | `Int _ -> expectFailStr "`Int"
  | `String _ -> expectFailStr "`String"
  | `Null -> expectFailStr "`Null"
  | `Float _ -> expectFailStr "`Float"
  | `Bool _ -> expectFailStr "`Bool"
  | `Assoc _ -> expectFailStr "`Assoc"

let _ =
  describe "Encode" (fun () ->
      let open Expect in
      test "string" (fun () ->
          match Json.Encode.string "Melange is awesome" with
          | `String str -> expect str |> toBe "Melange is awesome"
          | a -> fail (expectFail "`String" a));
      test "int" (fun () ->
          match Json.Encode.int 1 with
          | `Int int -> expect int |> toBe 1
          | a -> fail (expectFail "`Int" a));
      test "float" (fun () ->
          match Json.Encode.float 1.61 with
          | `Float float -> expect float |> toBe 1.61
          | a -> fail (expectFail "`Float" a));
      test "bool" (fun () ->
          match Json.Encode.bool false with
          | `Bool bool -> expect bool |> toBe false
          | a -> fail (expectFail "`Bool" a));
      test "list" (fun () ->
          match Json.Encode.list [ 1; 2 ] Json.Encode.int with
          | `List [ `Int 1; `Int 2 ] -> pass
          | a -> fail (expectFail "`List" a));
      test "assoc" (fun () ->
          match Json.Encode.assoc Json.Encode.int [ ("Answer", 42) ] with
          | `Assoc [ ("Answer", `Int 42) ] -> pass
          | a -> fail (expectFail "`Assoc" a)))
