open Jest

let _ =
  describe "Util" (fun () ->
      let open Expect in
      test "to_js_json" (fun () ->
          let json =
            Json.Client.to_js_json
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
          expect @@ Js.Json.stringify json
          |> toBe
               "{\"name\":\"John \
                Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
                Main \
                St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
                Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
                Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}");
      test "to_js_json" (fun () ->
          let json =
            Json.Client.of_js_json
            @@ Js.Json.parseExn
                 "{\"name\":\"John \
                  Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
                  Main \
                  St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
                  Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. \
                  Computer \
                  Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
          in
          expect json
          |> toEqual
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
                 ])))
