let () =
  Alcotest.run "Json"
    [
      Test_json_encode.tests;
      Test_json_decode.tests;
      Test_json_util.tests;
      Test_json_yojson.tests;
      Test_json.tests;
    ]
