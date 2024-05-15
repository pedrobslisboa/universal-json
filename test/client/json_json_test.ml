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
      test "show" (fun () ->
          let json =
            Json.Util.show
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
          |> toEqual
               "`Assoc ([\n\
               \  (\"name\" , (`String (\"John Doe\")));\n\
               \  (\"age\" , (`Int (30)));\n\
               \  (\"is_student\" , (`Bool (false)));\n\
               \  (\"address\" , (`Assoc ([\n\
               \      (\"street\" , (`String (\"1234 Main St\")));\n\
               \      (\"city\" , (`String (\"Anytown\")));\n\
               \      (\"country\" , (`String (\"USA\")))])));\n\
               \  (\"phone_numbers\" , (`List ([\n\
               \      (`String (\"123-456-7890\")),\n\
               \      (`String (\"098-765-4321\"))])));\n\
               \  (\"skills\" , (`List ([\n\
               \      (`String (\"programming\")),\n\
               \      (`String (\"writing\"))])));\n\
               \  (\"education\" , (`List ([\n\
               \      (`Assoc ([\n\
               \          (\"degree\" , (`String (\"B.Sc. Computer Science\")));\n\
               \          (\"year\" , (`Int (2014)))])),\n\
               \      (`Assoc ([\n\
               \          (\"degree\" , (`String (\"M.Sc. Computer Science\")));\n\
               \          (\"year\" , (`Int (2016)))]))])));\n\
               \  (\"null_field\" , (`Null));\n\
               \  (\"details\" , (`Assoc ([\n\
               \      (\"married\" , (`Bool (true)));\n\
               \      (\"hobbies\" , (`List ([\n\
               \          (`String (\"reading\")),\n\
               \          (`String (\"gaming\")),\n\
               \          (`String (\"hiking\"))])))])))])");
      test "to_string" (fun () ->
          let json =
            Json.Util.to_string
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
            Json.Util.parse
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
                   (Json.Util.show got))))
