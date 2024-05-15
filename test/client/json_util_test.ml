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
  describe "Util" (fun () ->
      let open Expect in
      test "stringify" (fun () ->
          let json =
            Json.stringify
              (`Assoc
                [
                  ("name", `String "John Doe");
                  ("age", `Int 30);
                  ("is_student", `Bool false);
                  ( "address",
                    `Assoc
                      [
                        ("street", `String "1234 Main St");
                        ("city", `String "Anytown");
                        ("country", `String "USA");
                      ] );
                  ( "phone_numbers",
                    `List [ `String "123-456-7890"; `String "098-765-4321" ] );
                  ("skills", `List [ `String "programming"; `String "writing" ]);
                  ( "education",
                    `List
                      [
                        `Assoc
                          [
                            ("degree", `String "B.Sc. Computer Science");
                            ("year", `Int 2014);
                          ];
                        `Assoc
                          [
                            ("degree", `String "M.Sc. Computer Science");
                            ("year", `Int 2016);
                          ];
                      ] );
                  ("null_field", `Null);
                  ( "details",
                    `Assoc
                      [
                        ("married", `Bool true);
                        ( "hobbies",
                          `List
                            [
                              `String "reading";
                              `String "gaming";
                              `String "hiking";
                            ] );
                      ] );
                ])
          in
          expect json
          |> toBe
               "{\"name\":\"John \
                Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
                Main \
                St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
                Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
                Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}");
      test "parse" (fun () ->
          let json =
            Json.parse
              "{\"name\":\"John \
               Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
               Main \
               St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
               Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
               Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
          in
          let expected = Json.Util.show json in
          match json with
          | `Assoc
              [
                ("name", `String "John Doe");
                ("age", `Int 30);
                ("is_student", `Bool false);
                ( "address",
                  `Assoc
                    [
                      ("street", `String "1234 Main St");
                      ("city", `String "Anytown");
                      ("country", `String "USA");
                    ] );
                ( "phone_numbers",
                  `List [ `String "123-456-7890"; `String "098-765-4321" ] );
                ("skills", `List [ `String "programming"; `String "writing" ]);
                ( "education",
                  `List
                    [
                      `Assoc
                        [
                          ("degree", `String "B.Sc. Computer Science");
                          ("year", `Int 2014);
                        ];
                      `Assoc
                        [
                          ("degree", `String "M.Sc. Computer Science");
                          ("year", `Int 2016);
                        ];
                    ] );
                ("null_field", `Null);
                ( "details",
                  `Assoc
                    [
                      ("married", `Bool true);
                      ( "hobbies",
                        `List
                          [
                            `String "reading";
                            `String "gaming";
                            `String "hiking";
                          ] );
                    ] );
              ] ->
              pass
          | got ->
              fail
                (Printf.sprintf "\nExpected %s,\nGot %s" expected
                   (Json.Util.show got)));
      test "parse_raise" (fun () ->
          expect (fun () ->
              Json.parse
                "{name\":\"John \
                 Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
                 Main \
                 St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
                 Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. \
                 Computer \
                 Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}")
          |> toThrow);
      test "parse_opt" (fun () ->
          let json =
            Json.parse_opt
              "{\"name\":\"John \
               Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
               Main \
               St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
               Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
               Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
          in
          let expected = Json.Util.show (json |> Option.get) in
          match json with
          | Some
              (`Assoc
                [
                  ("name", `String "John Doe");
                  ("age", `Int 30);
                  ("is_student", `Bool false);
                  ( "address",
                    `Assoc
                      [
                        ("street", `String "1234 Main St");
                        ("city", `String "Anytown");
                        ("country", `String "USA");
                      ] );
                  ( "phone_numbers",
                    `List [ `String "123-456-7890"; `String "098-765-4321" ] );
                  ("skills", `List [ `String "programming"; `String "writing" ]);
                  ( "education",
                    `List
                      [
                        `Assoc
                          [
                            ("degree", `String "B.Sc. Computer Science");
                            ("year", `Int 2014);
                          ];
                        `Assoc
                          [
                            ("degree", `String "M.Sc. Computer Science");
                            ("year", `Int 2016);
                          ];
                      ] );
                  ("null_field", `Null);
                  ( "details",
                    `Assoc
                      [
                        ("married", `Bool true);
                        ( "hobbies",
                          `List
                            [
                              `String "reading";
                              `String "gaming";
                              `String "hiking";
                            ] );
                      ] );
                ]) ->
              pass
          | None -> fail (Printf.sprintf "\nExpected %s,\nGot None" expected)
          | Some got ->
              fail
                (Printf.sprintf "\nExpected %s,\nGot Some(%s)" expected
                   (Json.Util.show got)));
      test "parse_opt_None" (fun () ->
          let json =
            Json.parse_opt
              "{name\":\"John \
               Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
               Main \
               St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
               Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
               Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
          in
          match json with
          | None -> pass
          | Some got ->
              fail
                (Printf.sprintf "\nExpected None,\nGot Some(%s)"
                   (Json.Util.show got))))
