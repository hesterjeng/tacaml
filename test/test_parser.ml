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
    (* New comprehensive parser tests *)
    (* Unit constructors - candlestick patterns *)
    test_successful_parse "Cdl2crows ()" "Cdl2crows";
    test_successful_parse "Cdldoji ()" "Cdldoji";
    test_successful_parse "Cdlhammer ()" "Cdlhammer";
    test_successful_parse "Cdlengulfing ()" "Cdlengulfing";
    (* Unit constructors - Hilbert Transform *)
    test_successful_parse "Ht_dcperiod ()" "Ht_dcperiod";
    test_successful_parse "Ht_trendline ()" "Ht_trendline";
    test_successful_parse "Ht_phasor ()" "Ht_phasor";
    (* Simple timeperiod constructors *)
    test_successful_parse "Cci { timeperiod = 20 }" "Cci";
    test_successful_parse "Dema { timeperiod = 30 }" "Dema";
    test_successful_parse "Kama { timeperiod = 30 }" "Kama";
    test_successful_parse "Willr { timeperiod = 14 }" "Willr";
    test_successful_parse "Linearreg { timeperiod = 14 }" "Linearreg";
    test_successful_parse "Mfi { timeperiod = 14 }" "Mfi";
    test_successful_parse "Natr { timeperiod = 14 }" "Natr";
    test_successful_parse "Roc { timeperiod = 10 }" "Roc";
    test_successful_parse "Tema { timeperiod = 30 }" "Tema";
    test_successful_parse "Trima { timeperiod = 30 }" "Trima";
    test_successful_parse "Wma { timeperiod = 30 }" "Wma";
    (* Complex constructors *)
    test_successful_parse
      "Ppo { fast_period = 12; slow_period = 26; ma_type = SMA }" "Ppo";
    test_successful_parse "T3 { timeperiod = 5; v_factor = 0.7 }" "T3";
    test_successful_parse "Mama { fast_limit = 0.5; slow_limit = 0.05 }" "Mama";
    test_successful_parse
      "Ultosc { timeperiod1 = 7; timeperiod2 = 14; timeperiod3 = 28 }" "Ultosc";
    test_successful_parse "Stddev { timeperiod = 5; nb_dev = 1.0 }" "Stddev";
    test_successful_parse "Var { timeperiod = 5; nb_dev = 1.0 }" "Var";
    test_successful_parse
      "Stochf { fast_k_period = 5; fast_d_period = 3; fast_d_ma_type = SMA }"
      "Stochf";
    test_successful_parse
      "Stochrsi { timeperiod = 14; fast_k_period = 5; fast_d_period = 3; \
       fast_d_ma_type = SMA }"
      "Stochrsi";
    test_successful_parse "Macdfix { signal_period = 9 }" "Macdfix";
    test_successful_parse
      "Mavp { min_period = 2; max_period = 30; ma_type = SMA }" "Mavp";
    (* Candlestick patterns with penetration *)
    test_successful_parse "Cdlabandonedbaby { penetration = 0.3 }"
      "Cdlabandonedbaby";
    test_successful_parse "Cdldarkcloudcover { penetration = 0.5 }"
      "Cdldarkcloudcover";
    test_successful_parse "Cdlmorningstar { penetration = 0.3 }"
      "Cdlmorningstar";
  ]

let error_tests =
  [
    test_parse_error "InvalidIndicator ()" "Unknown or unsupported constructor";
    test_parse_error "Avgprice { timeperiod = 8 }"
      "Unknown or unsupported constructor";
    test_parse_error "Sma { invalid_field = 20 }"
      "Expected: { timeperiod = int }";
    test_parse_error "Sma { timeperiod = abc }" "Invalid integer";
    test_parse_error
      "Apo { fast_period = 12; slow_period = 26; ma_type = INVALID }"
      "Invalid Ma_type";
    test_parse_error "Sar { acceleration = abc; maximum = 0.2 }" "Invalid float";
    test_parse_error "Accbands timeperiod = 8 }"
      "Record must be enclosed in braces";
  ]

let () =
  run "Parser Tests"
    [
      ("successful_parsing", successful_parse_tests);
      ("error_handling", error_tests);
    ]
