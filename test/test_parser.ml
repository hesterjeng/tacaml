open Alcotest

let test_successful_parse input expected_name =
  let test_name = "parse_" ^ expected_name in
  test_case test_name `Quick (fun () ->
      match Tacaml.Parser.of_string input with
      | Ok pack ->
        let actual = Tacaml.to_string pack in
        check string "parsed indicator name" expected_name actual
      | Error e -> fail ("Failed to parse: " ^ e))

let test_parse_error input expected_error_prefix =
  let test_name =
    "parse_error_" ^ String.sub input 0 (min 10 (String.length input))
  in
  test_case test_name `Quick (fun () ->
      match Tacaml.Parser.of_string input with
      | Ok pack ->
        fail ("Expected error but got success: " ^ Tacaml.to_string pack)
      | Error e ->
        check bool
          ("error message should start with: " ^ expected_error_prefix)
          true
          (String.sub e 0
             (min (String.length e) (String.length expected_error_prefix))
          = expected_error_prefix))

let successful_parse_tests =
  [
    test_successful_parse "Avgprice ()" "Avgprice";
    test_successful_parse "Accbands { timeperiod = 8 }" "Accbands";
    test_successful_parse "Adosc { fast_period = 3; slow_period = 10 }" "Adosc";
    test_successful_parse
      "Apo { fast_period = 12; slow_period = 26; ma_type = SMA }" "Apo";
    test_successful_parse
      "Bbands { timeperiod = 20; nb_dev_up = 2.0; nb_dev_dn = 2.0; ma_type = \
       EMA }"
      "Bbands";
    test_successful_parse "Ma { timeperiod = 14; ma_type = DEMA }" "Ma";
    test_successful_parse "Sar { acceleration = 0.02; maximum = 0.2 }" "Sar";
    test_successful_parse
      "Macd { fast_period = 12; slow_period = 26; signal_period = 9 }" "Macd";
    test_successful_parse "Sma { timeperiod = 20 }" "Sma";
    test_successful_parse "Rsi { timeperiod = 14 }" "Rsi";
  ]

let error_tests =
  [
    test_parse_error "InvalidIndicator ()" "Unknown or unsupported constructor";
    test_parse_error "Avgprice { timeperiod = 8 }"
      "Record must be enclosed in braces";
    test_parse_error "Sma { invalid_field = 20 }"
      "Expected: { timeperiod = int }";
    test_parse_error "Sma { timeperiod = abc }" "Invalid integer";
    test_parse_error "Apo { fast_period = 12; ma_type = INVALID }"
      "Invalid Ma_type";
    test_parse_error "Sar { acceleration = abc }" "Invalid float";
    test_parse_error "Accbands timeperiod = 8 }"
      "Record must be enclosed in braces";
  ]

let () =
  run "Parser Tests"
    [
      ("successful_parsing", successful_parse_tests);
      ("error_handling", error_tests);
    ]
