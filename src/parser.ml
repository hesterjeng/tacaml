open Containers

(* Helper functions for parsing *)
let ( let* ) = Result.( let* )
let trim_whitespace = String.trim
let starts_with prefix s = String.prefix ~pre:prefix s
let ends_with suffix s = String.suffix ~suf:suffix s

(* Parse a field assignment like "timeperiod = 8" *)
let parse_field_assignment s =
  match String.split_on_char '=' s with
  | [ field; value ] ->
    let field = trim_whitespace field in
    let value = trim_whitespace value in
    Ok (field, value)
  | _ -> Error ("Invalid field assignment: " ^ s)

(* Parse int from string *)
let parse_int s =
  try Ok (int_of_string (trim_whitespace s)) with
  | Failure _ -> Error ("Invalid integer: " ^ s)

(* Parse float from string *)
let parse_float s =
  try Ok (float_of_string (trim_whitespace s)) with
  | Failure _ -> Error ("Invalid float: " ^ s)

(* Parse record fields from string like "{ timeperiod = 8; ma_type = SMA }" *)
let parse_record_fields s =
  let s = trim_whitespace s in
  if not (starts_with "{" s && ends_with "}" s) then
    Error "Record must be enclosed in braces"
  else
    let inner = String.sub s 1 (String.length s - 2) in
    let fields = String.split_on_char ';' inner in
    let fields = List.map trim_whitespace fields in
    let fields = List.filter (fun s -> String.length s > 0) fields in

    let rec parse_fields acc = function
      | [] -> Ok (List.rev acc)
      | field :: rest -> (
        match parse_field_assignment field with
        | Ok (name, value) -> parse_fields ((name, value) :: acc) rest
        | Error e -> Error e)
    in
    parse_fields [] fields

(* Extract constructor name and arguments from input *)
let parse_constructor_and_args s =
  let s = trim_whitespace s in
  if String.contains s ' ' then
    let parts = String.split_on_char ' ' s in
    match parts with
    | name :: rest ->
      let name = trim_whitespace name in
      let args = String.concat " " rest |> trim_whitespace in
      Ok (name, args)
    | _ -> Error ("Invalid format: " ^ s)
  else if String.contains s '(' then
    (* Handle case like "Avgprice()" *)
    let idx = String.index s '(' in
    let constructor_name = String.sub s 0 idx in
    let args = String.sub s idx (String.length s - idx) in
    Ok (constructor_name, args)
  else Error ("Invalid format: " ^ s)

(* Parser for specific constructors with timeperiod only *)
let parse_simple_timeperiod constructor fields =
  match fields with
  | [ ("timeperiod", value) ] -> parse_int value |> Result.map constructor
  | _ -> Error "Expected: { timeperiod = int }"

(* Parser for constructors with multiple parameters *)
let parse_adosc fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_period", fp); ("slow_period", sp) ] ->
    let* fast_period = parse_int fp in
    let* slow_period = parse_int sp in
    Ok (Safe.Adosc { fast_period; slow_period })
  | _ -> Error "Adosc expects: { fast_period = int; slow_period = int }"

let parse_apo fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_period", fp); ("ma_type", mt); ("slow_period", sp) ] ->
    let* fast_period = parse_int fp in
    let* slow_period = parse_int sp in
    let* ma_type = Ma_type.parse mt in
    Ok (Safe.Apo { fast_period; slow_period; ma_type })
  | _ ->
    Error
      "Apo expects: { fast_period = int; slow_period = int; ma_type = \
       Ma_type.t }"

let parse_bbands fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [
   ("ma_type", mt); ("nb_dev_dn", ndd); ("nb_dev_up", ndu); ("timeperiod", tp);
  ] ->
    let* timeperiod = parse_int tp in
    let* nb_dev_up = parse_float ndu in
    let* nb_dev_dn = parse_float ndd in
    let* ma_type = Ma_type.parse mt in
    Ok (Safe.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type })
  | _ ->
    Error
      "Bbands expects: { timeperiod = int; nb_dev_up = float; nb_dev_dn = \
       float; ma_type = Ma_type.t }"

let parse_ma fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("ma_type", mt); ("timeperiod", tp) ] ->
    let* timeperiod = parse_int tp in
    let* ma_type = Ma_type.parse mt in
    Ok (Safe.Ma { timeperiod; ma_type })
  | _ -> Error "Ma expects: { timeperiod = int; ma_type = Ma_type.t }"

let parse_sar fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("acceleration", acc); ("maximum", max) ] ->
    let* acceleration = parse_float acc in
    let* maximum = parse_float max in
    Ok (Safe.Sar { acceleration; maximum })
  | _ -> Error "Sar expects: { acceleration = float; maximum = float }"

let parse_macd fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_period", fp); ("signal_period", sgp); ("slow_period", sp) ] ->
    let* fast_period = parse_int fp in
    let* slow_period = parse_int sp in
    let* signal_period = parse_int sgp in
    Ok (Safe.Macd { fast_period; slow_period; signal_period })
  | _ ->
    Error
      "Macd expects: { fast_period = int; slow_period = int; signal_period = \
       int }"

let parse_stoch fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [
   ("fast_k_period", fkp);
   ("slow_d_ma_type", sdmt);
   ("slow_d_period", sdp);
   ("slow_k_ma_type", skmt);
   ("slow_k_period", skp);
  ] ->
    let* fast_k_period = parse_int fkp in
    let* slow_k_period = parse_int skp in
    let* slow_k_ma_type = Ma_type.parse skmt in
    let* slow_d_period = parse_int sdp in
    let* slow_d_ma_type = Ma_type.parse sdmt in
    Ok
      (Safe.Stoch
         {
           fast_k_period;
           slow_k_period;
           slow_k_ma_type;
           slow_d_period;
           slow_d_ma_type;
         })
  | _ ->
    Error
      "Stoch expects: { fast_k_period = int; slow_k_period = int; \
       slow_k_ma_type = Ma_type.t; slow_d_period = int; slow_d_ma_type = \
       Ma_type.t }"

(* Helper function to parse Stochf parameters *)
let parse_stochf fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_d_ma_type", fdmt); ("fast_d_period", fdp); ("fast_k_period", fkp) ]
    ->
    let* fast_k_period = parse_int fkp in
    let* fast_d_period = parse_int fdp in
    let* fast_d_ma_type = Ma_type.parse fdmt in
    Ok (Safe.Stochf { fast_k_period; fast_d_period; fast_d_ma_type })
  | _ ->
    Error
      "Stochf expects: { fast_k_period = int; fast_d_period = int; \
       fast_d_ma_type = Ma_type.t }"

(* Helper function to parse Stochrsi parameters *)
let parse_stochrsi fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [
   ("fast_d_ma_type", fdmt);
   ("fast_d_period", fdp);
   ("fast_k_period", fkp);
   ("timeperiod", tp);
  ] ->
    let* timeperiod = parse_int tp in
    let* fast_k_period = parse_int fkp in
    let* fast_d_period = parse_int fdp in
    let* fast_d_ma_type = Ma_type.parse fdmt in
    Ok
      (Safe.Stochrsi
         { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type })
  | _ ->
    Error
      "Stochrsi expects: { timeperiod = int; fast_k_period = int; \
       fast_d_period = int; fast_d_ma_type = Ma_type.t }"

(* Helper function to parse Macdext parameters *)
let parse_macdext fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [
   ("fast_ma_type", fmt);
   ("fast_period", fp);
   ("signal_ma_type", smt);
   ("signal_period", sp);
   ("slow_ma_type", slmt);
   ("slow_period", slp);
  ] ->
    let* fast_period = parse_int fp in
    let* fast_ma_type = Ma_type.parse fmt in
    let* slow_period = parse_int slp in
    let* slow_ma_type = Ma_type.parse slmt in
    let* signal_period = parse_int sp in
    let* signal_ma_type = Ma_type.parse smt in
    Ok
      (Safe.Macdext
         {
           fast_period;
           fast_ma_type;
           slow_period;
           slow_ma_type;
           signal_period;
           signal_ma_type;
         })
  | _ ->
    Error
      "Macdext expects: { fast_period = int; fast_ma_type = Ma_type.t; \
       slow_period = int; slow_ma_type = Ma_type.t; signal_period = int; \
       signal_ma_type = Ma_type.t }"

(* Helper function to parse Macdfix parameters *)
let parse_macdfix fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("signal_period", sp) ] ->
    let* signal_period = parse_int sp in
    Ok (Safe.Macdfix { signal_period })
  | _ -> Error "Macdfix expects: { signal_period = int }"

(* Helper function to parse Mama parameters *)
let parse_mama fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_limit", fl); ("slow_limit", sl) ] ->
    let* fast_limit = parse_float fl in
    let* slow_limit = parse_float sl in
    Ok (Safe.Mama { fast_limit; slow_limit })
  | _ -> Error "Mama expects: { fast_limit = float; slow_limit = float }"

(* Helper function to parse Mavp parameters *)
let parse_mavp fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("ma_type", mt); ("max_period", maxp); ("min_period", minp) ] ->
    let* min_period = parse_int minp in
    let* max_period = parse_int maxp in
    let* ma_type = Ma_type.parse mt in
    Ok (Safe.Mavp { min_period; max_period; ma_type })
  | _ ->
    Error
      "Mavp expects: { min_period = int; max_period = int; ma_type = Ma_type.t \
       }"

(* Helper function to parse Sarext parameters *)
let parse_sarext fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [
   ("acceleration_init_long", ail);
   ("acceleration_init_short", ais);
   ("acceleration_long", al);
   ("acceleration_max_long", aml);
   ("acceleration_max_short", ams);
   ("acceleration_short", as_);
   ("offset_on_reverse", oor);
   ("start_value", sv);
  ] ->
    let* start_value = parse_float sv in
    let* offset_on_reverse = parse_float oor in
    let* acceleration_init_long = parse_float ail in
    let* acceleration_long = parse_float al in
    let* acceleration_max_long = parse_float aml in
    let* acceleration_init_short = parse_float ais in
    let* acceleration_short = parse_float as_ in
    let* acceleration_max_short = parse_float ams in
    Ok
      (Safe.Sarext
         {
           start_value;
           offset_on_reverse;
           acceleration_init_long;
           acceleration_long;
           acceleration_max_long;
           acceleration_init_short;
           acceleration_short;
           acceleration_max_short;
         })
  | _ ->
    Error
      "Sarext expects: { start_value = float; offset_on_reverse = float; \
       acceleration_init_long = float; acceleration_long = float; \
       acceleration_max_long = float; acceleration_init_short = float; \
       acceleration_short = float; acceleration_max_short = float }"

(* Helper function to parse Ppo parameters *)
let parse_ppo fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("fast_period", fp); ("ma_type", mt); ("slow_period", sp) ] ->
    let* fast_period = parse_int fp in
    let* slow_period = parse_int sp in
    let* ma_type = Ma_type.parse mt in
    Ok (Safe.Ppo { fast_period; slow_period; ma_type })
  | _ ->
    Error
      "Ppo expects: { fast_period = int; slow_period = int; ma_type = \
       Ma_type.t }"

(* Helper function to parse T3 parameters *)
let parse_t3 fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("timeperiod", tp); ("v_factor", vf) ] ->
    let* timeperiod = parse_int tp in
    let* v_factor = parse_float vf in
    Ok (Safe.T3 { timeperiod; v_factor })
  | _ -> Error "T3 expects: { timeperiod = int; v_factor = float }"

(* Helper function to parse Stddev parameters *)
let parse_stddev fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("nb_dev", nd); ("timeperiod", tp) ] ->
    let* timeperiod = parse_int tp in
    let* nb_dev = parse_float nd in
    Ok (Safe.Stddev { timeperiod; nb_dev })
  | _ -> Error "Stddev expects: { timeperiod = int; nb_dev = float }"

(* Helper function to parse Var parameters *)
let parse_var fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("nb_dev", nd); ("timeperiod", tp) ] ->
    let* timeperiod = parse_int tp in
    let* nb_dev = parse_float nd in
    Ok (Safe.Var { timeperiod; nb_dev })
  | _ -> Error "Var expects: { timeperiod = int; nb_dev = float }"

(* Helper function to parse Ultosc parameters *)
let parse_ultosc fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("timeperiod1", tp1); ("timeperiod2", tp2); ("timeperiod3", tp3) ] ->
    let* timeperiod1 = parse_int tp1 in
    let* timeperiod2 = parse_int tp2 in
    let* timeperiod3 = parse_int tp3 in
    Ok (Safe.Ultosc { timeperiod1; timeperiod2; timeperiod3 })
  | _ ->
    Error
      "Ultosc expects: { timeperiod1 = int; timeperiod2 = int; timeperiod3 = \
       int }"

(* Helper function to parse candlestick patterns with penetration *)
let parse_penetration fields =
  let sorted_fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  match sorted_fields with
  | [ ("penetration", p) ] ->
    let* penetration = parse_float p in
    Ok penetration
  | _ -> Error "Penetration expects: { penetration = float }"

(* Main parser function *)
let parse_pack (s : string) : (Pack.t, string) result =
  match parse_constructor_and_args s with
  | Error e -> Error e
  | Ok (constructor_name, args) -> (
    match constructor_name with
    (* Unit constructors *)
    | "Avgprice" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Avgprice ()))
    | "Acos" when String.equal args "()" -> Ok (Pack.pack (Safe.Acos ()))
    | "Ad" when String.equal args "()" -> Ok (Pack.pack (Safe.Ad ()))
    | "Add" when String.equal args "()" -> Ok (Pack.pack (Safe.Add ()))
    | "Asin" when String.equal args "()" -> Ok (Pack.pack (Safe.Asin ()))
    | "Atan" when String.equal args "()" -> Ok (Pack.pack (Safe.Atan ()))
    | "Bop" when String.equal args "()" -> Ok (Pack.pack (Safe.Bop ()))
    | "Ceil" when String.equal args "()" -> Ok (Pack.pack (Safe.Ceil ()))
    | "Cos" when String.equal args "()" -> Ok (Pack.pack (Safe.Cos ()))
    | "Cosh" when String.equal args "()" -> Ok (Pack.pack (Safe.Cosh ()))
    | "Div" when String.equal args "()" -> Ok (Pack.pack (Safe.Div ()))
    | "Exp" when String.equal args "()" -> Ok (Pack.pack (Safe.Exp ()))
    | "Floor" when String.equal args "()" -> Ok (Pack.pack (Safe.Floor ()))
    | "Ln" when String.equal args "()" -> Ok (Pack.pack (Safe.Ln ()))
    | "Log10" when String.equal args "()" -> Ok (Pack.pack (Safe.Log10 ()))
    | "Medprice" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Medprice ()))
    | "Mult" when String.equal args "()" -> Ok (Pack.pack (Safe.Mult ()))
    | "Obv" when String.equal args "()" -> Ok (Pack.pack (Safe.Obv ()))
    | "Sin" when String.equal args "()" -> Ok (Pack.pack (Safe.Sin ()))
    | "Sinh" when String.equal args "()" -> Ok (Pack.pack (Safe.Sinh ()))
    | "Sqrt" when String.equal args "()" -> Ok (Pack.pack (Safe.Sqrt ()))
    | "Sub" when String.equal args "()" -> Ok (Pack.pack (Safe.Sub ()))
    | "Tan" when String.equal args "()" -> Ok (Pack.pack (Safe.Tan ()))
    | "Tanh" when String.equal args "()" -> Ok (Pack.pack (Safe.Tanh ()))
    | "Trange" when String.equal args "()" -> Ok (Pack.pack (Safe.Trange ()))
    | "Typprice" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Typprice ()))
    | "Wclprice" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Wclprice ()))
    (* Record constructors *)
    | "Accbands" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Accbands { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Adosc" ->
      let* fields = parse_record_fields args in
      let* result = parse_adosc fields in
      Ok (Pack.pack result)
    | "Adx" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Adx { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Adxr" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Adxr { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Apo" ->
      let* fields = parse_record_fields args in
      let* result = parse_apo fields in
      Ok (Pack.pack result)
    | "Aroon" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Aroon { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Aroonosc" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Aroonosc { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Atr" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Atr { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Bbands" ->
      let* fields = parse_record_fields args in
      let* result = parse_bbands fields in
      Ok (Pack.pack result)
    | "Ma" ->
      let* fields = parse_record_fields args in
      let* result = parse_ma fields in
      Ok (Pack.pack result)
    | "Macd" ->
      let* fields = parse_record_fields args in
      let* result = parse_macd fields in
      Ok (Pack.pack result)
    | "Sar" ->
      let* fields = parse_record_fields args in
      let* result = parse_sar fields in
      Ok (Pack.pack result)
    | "Sma" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Sma { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Ema" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Ema { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Rsi" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Rsi { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Stoch" ->
      let* fields = parse_record_fields args in
      let* result = parse_stoch fields in
      Ok (Pack.pack result)
    (* Additional unit constructors *)
    | "Cdl2crows" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl2crows ()))
    | "Cdl3blackcrows" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3blackcrows ()))
    | "Cdl3inside" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3inside ()))
    | "Cdl3linestrike" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3linestrike ()))
    | "Cdl3outside" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3outside ()))
    | "Cdl3starsinsouth" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3starsinsouth ()))
    | "Cdl3whitesoldiers" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdl3whitesoldiers ()))
    | "Cdladvanceblock" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdladvanceblock ()))
    | "Cdlbelthold" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlbelthold ()))
    | "Cdlbreakaway" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlbreakaway ()))
    | "Cdlclosingmarubozu" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlclosingmarubozu ()))
    | "Cdlconcealbabyswall" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlconcealbabyswall ()))
    | "Cdlcounterattack" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlcounterattack ()))
    | "Cdldoji" when String.equal args "()" -> Ok (Pack.pack (Safe.Cdldoji ()))
    | "Cdldojistar" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdldojistar ()))
    | "Cdldragonflydoji" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdldragonflydoji ()))
    | "Cdlengulfing" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlengulfing ()))
    | "Cdlgapsidesidewhite" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlgapsidesidewhite ()))
    | "Cdlgravestonedoji" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlgravestonedoji ()))
    | "Cdlhammer" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhammer ()))
    | "Cdlhangingman" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhangingman ()))
    | "Cdlharami" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlharami ()))
    | "Cdlharamicross" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlharamicross ()))
    | "Cdlhighwave" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhighwave ()))
    | "Cdlhikkake" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhikkake ()))
    | "Cdlhikkakemod" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhikkakemod ()))
    | "Cdlhomingpigeon" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlhomingpigeon ()))
    | "Cdlidentical3crows" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlidentical3crows ()))
    | "Cdlinneck" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlinneck ()))
    | "Cdlinvertedhammer" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlinvertedhammer ()))
    | "Cdlkicking" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlkicking ()))
    | "Cdlkickingbylength" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlkickingbylength ()))
    | "Cdlladderbottom" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlladderbottom ()))
    | "Cdllongleggeddoji" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdllongleggeddoji ()))
    | "Cdllongline" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdllongline ()))
    | "Cdlmarubozu" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlmarubozu ()))
    | "Cdlmatchinglow" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlmatchinglow ()))
    | "Cdlonneck" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlonneck ()))
    | "Cdlpiercing" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlpiercing ()))
    | "Cdlrickshawman" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlrickshawman ()))
    | "Cdlrisefall3methods" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlrisefall3methods ()))
    | "Cdlseparatinglines" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlseparatinglines ()))
    | "Cdlshootingstar" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlshootingstar ()))
    | "Cdlshortline" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlshortline ()))
    | "Cdlspinningtop" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlspinningtop ()))
    | "Cdlstalledpattern" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlstalledpattern ()))
    | "Cdlsticksandwich" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlsticksandwich ()))
    | "Cdltakuri" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdltakuri ()))
    | "Cdltasukigap" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdltasukigap ()))
    | "Cdlthrusting" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlthrusting ()))
    | "Cdltristar" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdltristar ()))
    | "Cdlunique3river" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlunique3river ()))
    | "Cdlupsidegap2crows" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlupsidegap2crows ()))
    | "Cdlxsidegap3methods" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Cdlxsidegap3methods ()))
    | "Ht_dcperiod" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Ht_dcperiod ()))
    | "Ht_dcphase" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Ht_dcphase ()))
    | "Ht_trendline" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Ht_trendline ()))
    | "Ht_trendmode" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Ht_trendmode ()))
    (* Additional record constructors with timeperiod *)
    | "Avgdev" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Avgdev { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Beta" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Beta { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Cci" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Cci { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Cmo" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Cmo { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Correl" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Correl { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Dema" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Dema { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Dx" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Dx { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Imi" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Imi { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Kama" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Kama { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Linearreg" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Linearreg { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Linearreg_angle" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Linearreg_angle { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Linearreg_intercept" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Linearreg_intercept { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Linearreg_slope" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Linearreg_slope { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Max" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Max { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Maxindex" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Maxindex { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Mfi" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Mfi { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Midpoint" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Midpoint { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Midprice" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Midprice { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Min" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Min { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Minindex" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Minindex { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Minmax" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Minmax { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Minmaxindex" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Minmaxindex { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Minus_di" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Minus_di { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Minus_dm" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Minus_dm { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Mom" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Mom { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Natr" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Natr { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Plus_di" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Plus_di { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Plus_dm" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Plus_dm { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Roc" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Roc { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Rocp" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Rocp { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Rocr" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Rocr { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Rocr100" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Rocr100 { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Sum" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Sum { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Tema" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Tema { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Trima" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Trima { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Trix" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Trix { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Tsf" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Tsf { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Willr" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Willr { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    | "Wma" ->
      let* fields = parse_record_fields args in
      let* result =
        parse_simple_timeperiod
          (fun timeperiod -> Safe.Wma { timeperiod })
          fields
      in
      Ok (Pack.pack result)
    (* Complex record constructors *)
    | "Ppo" ->
      let* fields = parse_record_fields args in
      let* result = parse_ppo fields in
      Ok (Pack.pack result)
    | "Stochf" ->
      let* fields = parse_record_fields args in
      let* result = parse_stochf fields in
      Ok (Pack.pack result)
    | "Stochrsi" ->
      let* fields = parse_record_fields args in
      let* result = parse_stochrsi fields in
      Ok (Pack.pack result)
    | "Macdext" ->
      let* fields = parse_record_fields args in
      let* result = parse_macdext fields in
      Ok (Pack.pack result)
    | "Macdfix" ->
      let* fields = parse_record_fields args in
      let* result = parse_macdfix fields in
      Ok (Pack.pack result)
    | "Mama" ->
      let* fields = parse_record_fields args in
      let* result = parse_mama fields in
      Ok (Pack.pack result)
    | "Mavp" ->
      let* fields = parse_record_fields args in
      let* result = parse_mavp fields in
      Ok (Pack.pack result)
    | "Sarext" ->
      let* fields = parse_record_fields args in
      let* result = parse_sarext fields in
      Ok (Pack.pack result)
    | "T3" ->
      let* fields = parse_record_fields args in
      let* result = parse_t3 fields in
      Ok (Pack.pack result)
    | "Stddev" ->
      let* fields = parse_record_fields args in
      let* result = parse_stddev fields in
      Ok (Pack.pack result)
    | "Var" ->
      let* fields = parse_record_fields args in
      let* result = parse_var fields in
      Ok (Pack.pack result)
    | "Ultosc" ->
      let* fields = parse_record_fields args in
      let* result = parse_ultosc fields in
      Ok (Pack.pack result)
    (* Candlestick patterns with penetration parameter *)
    | "Cdlabandonedbaby" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdlabandonedbaby { penetration }))
    | "Cdldarkcloudcover" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdldarkcloudcover { penetration }))
    | "Cdleveningdojistar" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdleveningdojistar { penetration }))
    | "Cdleveningstar" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdleveningstar { penetration }))
    | "Cdlmathold" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdlmathold { penetration }))
    | "Cdlmorningdojistar" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdlmorningdojistar { penetration }))
    | "Cdlmorningstar" ->
      let* fields = parse_record_fields args in
      let* penetration = parse_penetration fields in
      Ok (Pack.pack (Safe.Cdlmorningstar { penetration }))
    (* Additional two-array constructors *)
    | "Ht_phasor" when String.equal args "()" ->
      Ok (Pack.pack (Safe.Ht_phasor ()))
    | "Ht_sine" when String.equal args "()" -> Ok (Pack.pack (Safe.Ht_sine ()))
    (* Add more constructors as needed *)
    | _ -> Error ("Unknown or unsupported constructor: " ^ constructor_name))

(* Export the main function *)
let of_string = parse_pack
