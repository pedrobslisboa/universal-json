let to_yojson () =
  let json =
    Json.Util.parse
      "{\"name\":\"John \
       Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
       Main \
       St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
       Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
       Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
  in
  let yojson = Json.Native.to_yojson json in
  let expected = Yojson.Basic.show yojson in
  match yojson with
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
                `List [ `String "reading"; `String "gaming"; `String "hiking" ]
              );
            ] );
      ] ->
      Alcotest.(check @@ pass) "pass" () ()
  | got ->
      Alcotest.fail
        (Printf.sprintf "\nExpected %s,\nGot %s" expected (Json.Util.show got))

let of_yojson () =
  let json =
    Yojson.Basic.from_string
      "{\"name\":\"John \
       Doe\",\"age\":30,\"is_student\":false,\"address\":{\"street\":\"1234 \
       Main \
       St\",\"city\":\"Anytown\",\"country\":\"USA\"},\"phone_numbers\":[\"123-456-7890\",\"098-765-4321\"],\"skills\":[\"programming\",\"writing\"],\"education\":[{\"degree\":\"B.Sc. \
       Computer Science\",\"year\":2014},{\"degree\":\"M.Sc. Computer \
       Science\",\"year\":2016}],\"null_field\":null,\"details\":{\"married\":true,\"hobbies\":[\"reading\",\"gaming\",\"hiking\"]}}"
  in
  let universal_json = Json.Native.of_yojson json in
  let expected = Json.Util.show universal_json in
  match universal_json with
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
                `List [ `String "reading"; `String "gaming"; `String "hiking" ]
              );
            ] );
      ] ->
      Alcotest.(check @@ pass) "pass" () ()
  | got ->
      Alcotest.fail
        (Printf.sprintf "\nExpected %s,\nGot %s" expected (Json.Util.show got))

let case title fn = Alcotest.test_case title `Quick fn

let tests =
  ( "Yojson_Basic",
    [
      case "Yojson_Basic to_yojson" to_yojson;
      case "Yojson_Basic of_yojson" of_yojson;
    ] )
