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

(* Parse Ma_type from string *)
let parse_ma_type s =
  match String.uppercase_ascii (trim_whitespace s) with
  | "SMA" -> Ok Ma_type.SMA
  | "EMA" -> Ok Ma_type.EMA
  | "WMA" -> Ok Ma_type.WMA
  | "DEMA" -> Ok Ma_type.DEMA
  | "TEMA" -> Ok Ma_type.TEMA
  | "TRIMA" -> Ok Ma_type.TRIMA
  | "KAMA" -> Ok Ma_type.KAMA
  | "MAMA" -> Ok Ma_type.MAMA
  | "T3" -> Ok Ma_type.T3
  | _ -> Error ("Invalid Ma_type: " ^ s)

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
    let* ma_type = parse_ma_type mt in
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
    let* ma_type = parse_ma_type mt in
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
    let* ma_type = parse_ma_type mt in
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
    let* slow_k_ma_type = parse_ma_type skmt in
    let* slow_d_period = parse_int sdp in
    let* slow_d_ma_type = parse_ma_type sdmt in
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
    (* Add more constructors as needed *)
    | _ -> Error ("Unknown or unsupported constructor: " ^ constructor_name))

(* Export the main function *)
let of_string = parse_pack
