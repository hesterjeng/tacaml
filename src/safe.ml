module F = C.Functions
module C = Ctypes
module T = Type_description.Types
module Hash = CCHash

let ta_initialize () =
  let ret_code = F.initialize () in
  if ret_code = 0 then Ok () else Error (string_of_int ret_code)

let wrap f =
  let open Ctypes in
  let outBegIdx = C.allocate C.int 0 in
  let outNBElement = C.allocate C.int 0 in
  let res = f outBegIdx outNBElement in
  match res with
  | 0 -> Ok (!@outBegIdx, !@outNBElement)
  | err -> Error (`TALibCode err)

type float_ba =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type int_ba = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

type ('a, 'b) t =
  | Accbands : {
      timeperiod : int;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) t
  | Acos : unit -> (float_ba, float_ba) t
  | Ad : unit -> (Ohlcv.t, float_ba) t
  | Add : unit -> (float_ba * float_ba, float_ba) t
  | Adosc : { fast_period : int; slow_period : int } -> (Ohlcv.t, float_ba) t
  | Adx : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Adxr : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Apo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba) t
  | Aroon : { timeperiod : int } -> (Ohlcv.t, float_ba * float_ba) t
  | Aroonosc : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Asin : unit -> (float_ba, float_ba) t
  | Atan : unit -> (float_ba, float_ba) t
  | Atr : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Avgprice : unit -> (Ohlcv.t, float_ba) t
  | Avgdev : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Bbands : {
      timeperiod : int;
      nb_dev_up : float;
      nb_dev_dn : float;
      ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) t
  | Beta : { timeperiod : int } -> (float_ba * float_ba, float_ba) t
  | Bop : unit -> (Ohlcv.t, float_ba) t
  | Cci : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Cdl2crows : unit -> (Ohlcv.t, int_ba) t
  | Cdl3blackcrows : unit -> (Ohlcv.t, int_ba) t
  | Cdl3inside : unit -> (Ohlcv.t, int_ba) t
  | Cdl3linestrike : unit -> (Ohlcv.t, int_ba) t
  | Cdl3outside : unit -> (Ohlcv.t, int_ba) t
  | Cdl3starsinsouth : unit -> (Ohlcv.t, int_ba) t
  | Cdl3whitesoldiers : unit -> (Ohlcv.t, int_ba) t
  | Cdlabandonedbaby : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdladvanceblock : unit -> (Ohlcv.t, int_ba) t
  | Cdlbelthold : unit -> (Ohlcv.t, int_ba) t
  | Cdlbreakaway : unit -> (Ohlcv.t, int_ba) t
  | Cdlclosingmarubozu : unit -> (Ohlcv.t, int_ba) t
  | Cdlconcealbabyswall : unit -> (Ohlcv.t, int_ba) t
  | Cdlcounterattack : unit -> (Ohlcv.t, int_ba) t
  | Cdldarkcloudcover : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdldoji : unit -> (Ohlcv.t, int_ba) t
  | Cdldojistar : unit -> (Ohlcv.t, int_ba) t
  | Cdldragonflydoji : unit -> (Ohlcv.t, int_ba) t
  | Cdlengulfing : unit -> (Ohlcv.t, int_ba) t
  | Cdleveningdojistar : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdleveningstar : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdlgapsidesidewhite : unit -> (Ohlcv.t, int_ba) t
  | Cdlgravestonedoji : unit -> (Ohlcv.t, int_ba) t
  | Cdlhammer : unit -> (Ohlcv.t, int_ba) t
  | Cdlhangingman : unit -> (Ohlcv.t, int_ba) t
  | Cdlharami : unit -> (Ohlcv.t, int_ba) t
  | Cdlharamicross : unit -> (Ohlcv.t, int_ba) t
  | Cdlhighwave : unit -> (Ohlcv.t, int_ba) t
  | Cdlhikkake : unit -> (Ohlcv.t, int_ba) t
  | Cdlhikkakemod : unit -> (Ohlcv.t, int_ba) t
  | Cdlhomingpigeon : unit -> (Ohlcv.t, int_ba) t
  | Cdlidentical3crows : unit -> (Ohlcv.t, int_ba) t
  | Cdlinneck : unit -> (Ohlcv.t, int_ba) t
  | Cdlinvertedhammer : unit -> (Ohlcv.t, int_ba) t
  | Cdlkicking : unit -> (Ohlcv.t, int_ba) t
  | Cdlkickingbylength : unit -> (Ohlcv.t, int_ba) t
  | Cdlladderbottom : unit -> (Ohlcv.t, int_ba) t
  | Cdllongleggeddoji : unit -> (Ohlcv.t, int_ba) t
  | Cdllongline : unit -> (Ohlcv.t, int_ba) t
  | Cdlmarubozu : unit -> (Ohlcv.t, int_ba) t
  | Cdlmatchinglow : unit -> (Ohlcv.t, int_ba) t
  | Cdlmathold : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdlmorningdojistar : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdlmorningstar : { penetration : float } -> (Ohlcv.t, int_ba) t
  | Cdlonneck : unit -> (Ohlcv.t, int_ba) t
  | Cdlpiercing : unit -> (Ohlcv.t, int_ba) t
  | Cdlrickshawman : unit -> (Ohlcv.t, int_ba) t
  | Cdlrisefall3methods : unit -> (Ohlcv.t, int_ba) t
  | Cdlseparatinglines : unit -> (Ohlcv.t, int_ba) t
  | Cdlshootingstar : unit -> (Ohlcv.t, int_ba) t
  | Cdlshortline : unit -> (Ohlcv.t, int_ba) t
  | Cdlspinningtop : unit -> (Ohlcv.t, int_ba) t
  | Cdlstalledpattern : unit -> (Ohlcv.t, int_ba) t
  | Cdlsticksandwich : unit -> (Ohlcv.t, int_ba) t
  | Cdltakuri : unit -> (Ohlcv.t, int_ba) t
  | Cdltasukigap : unit -> (Ohlcv.t, int_ba) t
  | Cdlthrusting : unit -> (Ohlcv.t, int_ba) t
  | Cdltristar : unit -> (Ohlcv.t, int_ba) t
  | Cdlunique3river : unit -> (Ohlcv.t, int_ba) t
  | Cdlupsidegap2crows : unit -> (Ohlcv.t, int_ba) t
  | Cdlxsidegap3methods : unit -> (Ohlcv.t, int_ba) t
  | Ceil : unit -> (float_ba, float_ba) t
  | Cmo : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Correl : { timeperiod : int } -> (float_ba * float_ba, float_ba) t
  | Cos : unit -> (float_ba, float_ba) t
  | Cosh : unit -> (float_ba, float_ba) t
  | Dema : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Div : unit -> (float_ba * float_ba, float_ba) t
  | Dx : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Ema : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Exp : unit -> (float_ba, float_ba) t
  | Floor : unit -> (float_ba, float_ba) t
  | Ht_dcperiod : unit -> (Ohlcv.t, float_ba) t
  | Ht_dcphase : unit -> (Ohlcv.t, float_ba) t
  | Ht_phasor : unit -> (Ohlcv.t, float_ba * float_ba) t
  | Ht_sine : unit -> (Ohlcv.t, float_ba * float_ba) t
  | Ht_trendline : unit -> (Ohlcv.t, float_ba) t
  | Ht_trendmode : unit -> (Ohlcv.t, int_ba) t
  | Imi : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Kama : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Linearreg : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Linearreg_angle : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Linearreg_intercept : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Linearreg_slope : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Ln : unit -> (float_ba, float_ba) t
  | Log10 : unit -> (float_ba, float_ba) t
  | Ma : { timeperiod : int; ma_type : Ma_type.t } -> (Ohlcv.t, float_ba) t
  | Macd : {
      fast_period : int;
      slow_period : int;
      signal_period : int;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) t
  | Macdext : {
      fast_period : int;
      fast_ma_type : Ma_type.t;
      slow_period : int;
      slow_ma_type : Ma_type.t;
      signal_period : int;
      signal_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) t
  | Macdfix : {
      signal_period : int;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) t
  | Mama : {
      fast_limit : float;
      slow_limit : float;
    }
      -> (Ohlcv.t, float_ba * float_ba) t
  | Mavp : {
      min_period : int;
      max_period : int;
      ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba, float_ba) t
  | Max : { timeperiod : int } -> (float_ba, float_ba) t
  | Maxindex : { timeperiod : int } -> (float_ba, int_ba) t
  | Medprice : unit -> (Ohlcv.t, float_ba) t
  | Mfi : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Midpoint : { timeperiod : int } -> (float_ba, float_ba) t
  | Midprice : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Min : { timeperiod : int } -> (float_ba, float_ba) t
  | Minindex : { timeperiod : int } -> (float_ba, int_ba) t
  | Minmax : { timeperiod : int } -> (float_ba, float_ba * float_ba) t
  | Minmaxindex : { timeperiod : int } -> (float_ba, int_ba * int_ba) t
  | Minus_di : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Minus_dm : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Mom : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Mult : unit -> (float_ba * float_ba, float_ba) t
  | Natr : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Obv : unit -> (Ohlcv.t, float_ba) t
  | Plus_di : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Plus_dm : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Ppo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba) t
  | Roc : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Rocp : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Rocr : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Rocr100 : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Rsi : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Sar : { acceleration : float; maximum : float } -> (Ohlcv.t, float_ba) t
  | Sarext : {
      start_value : float;
      offset_on_reverse : float;
      acceleration_init_long : float;
      acceleration_long : float;
      acceleration_max_long : float;
      acceleration_init_short : float;
      acceleration_short : float;
      acceleration_max_short : float;
    }
      -> (Ohlcv.t, float_ba) t
  | Sin : unit -> (float_ba, float_ba) t
  | Sinh : unit -> (float_ba, float_ba) t
  | Sma : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Sqrt : unit -> (float_ba, float_ba) t
  | Stddev : { timeperiod : int; nb_dev : float } -> (float_ba, float_ba) t
  | Stoch : {
      fast_k_period : int;
      slow_k_period : int;
      slow_k_ma_type : Ma_type.t;
      slow_d_period : int;
      slow_d_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba) t
  | Stochf : {
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba) t
  | Stochrsi : {
      timeperiod : int;
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba) t
  | Sub : unit -> (float_ba * float_ba, float_ba) t
  | Sum : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | T3 : { timeperiod : int; v_factor : float } -> (Ohlcv.t, float_ba) t
  | Tan : unit -> (float_ba, float_ba) t
  | Tanh : unit -> (float_ba, float_ba) t
  | Tema : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Trange : unit -> (Ohlcv.t, float_ba) t
  | Trima : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Trix : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Tsf : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Typprice : unit -> (Ohlcv.t, float_ba) t
  | Ultosc : {
      timeperiod1 : int;
      timeperiod2 : int;
      timeperiod3 : int;
    }
      -> (Ohlcv.t, float_ba) t
  | Var : { timeperiod : int; nb_dev : float } -> (Ohlcv.t, float_ba) t
  | Wclprice : unit -> (Ohlcv.t, float_ba) t
  | Willr : { timeperiod : int } -> (Ohlcv.t, float_ba) t
  | Wma : { timeperiod : int } -> (Ohlcv.t, float_ba) t

let to_string : type a b. (a, b) t -> string = function
  | Accbands _ -> "Accbands"
  | Acos _ -> "Acos"
  | Ad _ -> "Ad"
  | Add _ -> "Add"
  | Adosc _ -> "Adosc"
  | Adx _ -> "Adx"
  | Adxr _ -> "Adxr"
  | Apo _ -> "Apo"
  | Aroon _ -> "Aroon"
  | Aroonosc _ -> "Aroonosc"
  | Asin _ -> "Asin"
  | Atan _ -> "Atan"
  | Atr _ -> "Atr"
  | Avgprice _ -> "Avgprice"
  | Avgdev _ -> "Avgdev"
  | Bbands _ -> "Bbands"
  | Beta _ -> "Beta"
  | Bop _ -> "Bop"
  | Cci _ -> "Cci"
  | Cdl2crows _ -> "Cdl2crows"
  | Cdl3blackcrows _ -> "Cdl3blackcrows"
  | Cdl3inside _ -> "Cdl3inside"
  | Cdl3linestrike _ -> "Cdl3linestrike"
  | Cdl3outside _ -> "Cdl3outside"
  | Cdl3starsinsouth _ -> "Cdl3starsinsouth"
  | Cdl3whitesoldiers _ -> "Cdl3whitesoldiers"
  | Cdlabandonedbaby _ -> "Cdlabandonedbaby"
  | Cdladvanceblock _ -> "Cdladvanceblock"
  | Cdlbelthold _ -> "Cdlbelthold"
  | Cdlbreakaway _ -> "Cdlbreakaway"
  | Cdlclosingmarubozu _ -> "Cdlclosingmarubozu"
  | Cdlconcealbabyswall _ -> "Cdlconcealbabyswall"
  | Cdlcounterattack _ -> "Cdlcounterattack"
  | Cdldarkcloudcover _ -> "Cdldarkcloudcover"
  | Cdldoji _ -> "Cdldoji"
  | Cdldojistar _ -> "Cdldojistar"
  | Cdldragonflydoji _ -> "Cdldragonflydoji"
  | Cdlengulfing _ -> "Cdlengulfing"
  | Cdleveningdojistar _ -> "Cdleveningdojistar"
  | Cdleveningstar _ -> "Cdleveningstar"
  | Cdlgapsidesidewhite _ -> "Cdlgapsidesidewhite"
  | Cdlgravestonedoji _ -> "Cdlgravestonedoji"
  | Cdlhammer _ -> "Cdlhammer"
  | Cdlhangingman _ -> "Cdlhangingman"
  | Cdlharami _ -> "Cdlharami"
  | Cdlharamicross _ -> "Cdlharamicross"
  | Cdlhighwave _ -> "Cdlhighwave"
  | Cdlhikkake _ -> "Cdlhikkake"
  | Cdlhikkakemod _ -> "Cdlhikkakemod"
  | Cdlhomingpigeon _ -> "Cdlhomingpigeon"
  | Cdlidentical3crows _ -> "Cdlidentical3crows"
  | Cdlinneck _ -> "Cdlinneck"
  | Cdlinvertedhammer _ -> "Cdlinvertedhammer"
  | Cdlkicking _ -> "Cdlkicking"
  | Cdlkickingbylength _ -> "Cdlkickingbylength"
  | Cdlladderbottom _ -> "Cdlladderbottom"
  | Cdllongleggeddoji _ -> "Cdllongleggeddoji"
  | Cdllongline _ -> "Cdllongline"
  | Cdlmarubozu _ -> "Cdlmarubozu"
  | Cdlmatchinglow _ -> "Cdlmatchinglow"
  | Cdlmathold _ -> "Cdlmathold"
  | Cdlmorningdojistar _ -> "Cdlmorningdojistar"
  | Cdlmorningstar _ -> "Cdlmorningstar"
  | Cdlonneck _ -> "Cdlonneck"
  | Cdlpiercing _ -> "Cdlpiercing"
  | Cdlrickshawman _ -> "Cdlrickshawman"
  | Cdlrisefall3methods _ -> "Cdlrisefall3methods"
  | Cdlseparatinglines _ -> "Cdlseparatinglines"
  | Cdlshootingstar _ -> "Cdlshootingstar"
  | Cdlshortline _ -> "Cdlshortline"
  | Cdlspinningtop _ -> "Cdlspinningtop"
  | Cdlstalledpattern _ -> "Cdlstalledpattern"
  | Cdlsticksandwich _ -> "Cdlsticksandwich"
  | Cdltakuri _ -> "Cdltakuri"
  | Cdltasukigap _ -> "Cdltasukigap"
  | Cdlthrusting _ -> "Cdlthrusting"
  | Cdltristar _ -> "Cdltristar"
  | Cdlunique3river _ -> "Cdlunique3river"
  | Cdlupsidegap2crows _ -> "Cdlupsidegap2crows"
  | Cdlxsidegap3methods _ -> "Cdlxsidegap3methods"
  | Ceil _ -> "Ceil"
  | Cmo _ -> "Cmo"
  | Correl _ -> "Correl"
  | Cos _ -> "Cos"
  | Cosh _ -> "Cosh"
  | Dema _ -> "Dema"
  | Div _ -> "Div"
  | Dx _ -> "Dx"
  | Ema _ -> "Ema"
  | Exp _ -> "Exp"
  | Floor _ -> "Floor"
  | Ht_dcperiod _ -> "Ht_dcperiod"
  | Ht_dcphase _ -> "Ht_dcphase"
  | Ht_phasor _ -> "Ht_phasor"
  | Ht_sine _ -> "Ht_sine"
  | Ht_trendline _ -> "Ht_trendline"
  | Ht_trendmode _ -> "Ht_trendmode"
  | Imi _ -> "Imi"
  | Kama _ -> "Kama"
  | Linearreg _ -> "Linearreg"
  | Linearreg_angle _ -> "Linearreg_angle"
  | Linearreg_intercept _ -> "Linearreg_intercept"
  | Linearreg_slope _ -> "Linearreg_slope"
  | Ln _ -> "Ln"
  | Log10 _ -> "Log10"
  | Ma _ -> "Ma"
  | Macd _ -> "Macd"
  | Macdext _ -> "Macdext"
  | Macdfix _ -> "Macdfix"
  | Mama _ -> "Mama"
  | Mavp _ -> "Mavp"
  | Max _ -> "Max"
  | Maxindex _ -> "Maxindex"
  | Medprice _ -> "Medprice"
  | Mfi _ -> "Mfi"
  | Midpoint _ -> "Midpoint"
  | Midprice _ -> "Midprice"
  | Min _ -> "Min"
  | Minindex _ -> "Minindex"
  | Minmax _ -> "Minmax"
  | Minmaxindex _ -> "Minmaxindex"
  | Minus_di _ -> "Minus_di"
  | Minus_dm _ -> "Minus_dm"
  | Mom _ -> "Mom"
  | Mult _ -> "Mult"
  | Natr _ -> "Natr"
  | Obv _ -> "Obv"
  | Plus_di _ -> "Plus_di"
  | Plus_dm _ -> "Plus_dm"
  | Ppo _ -> "Ppo"
  | Roc _ -> "Roc"
  | Rocp _ -> "Rocp"
  | Rocr _ -> "Rocr"
  | Rocr100 _ -> "Rocr100"
  | Rsi _ -> "Rsi"
  | Sar _ -> "Sar"
  | Sarext _ -> "Sarext"
  | Sin _ -> "Sin"
  | Sinh _ -> "Sinh"
  | Sma _ -> "Sma"
  | Sqrt _ -> "Sqrt"
  | Stddev _ -> "Stddev"
  | Stoch _ -> "Stoch"
  | Stochf _ -> "Stochf"
  | Stochrsi _ -> "Stochrsi"
  | Sub _ -> "Sub"
  | Sum _ -> "Sum"
  | T3 _ -> "T3"
  | Tan _ -> "Tan"
  | Tanh _ -> "Tanh"
  | Tema _ -> "Tema"
  | Trange _ -> "Trange"
  | Trima _ -> "Trima"
  | Trix _ -> "Trix"
  | Tsf _ -> "Tsf"
  | Typprice _ -> "Typprice"
  | Ultosc _ -> "Ultosc"
  | Var _ -> "Var"
  | Wclprice _ -> "Wclprice"
  | Willr _ -> "Willr"
  | Wma _ -> "Wma"

let pp : type a b. Format.formatter -> (a, b) t -> unit = fun fmt x ->
  let format_params params =
    match params with
    | [] -> ""
    | _ -> "(" ^ (String.concat ", " params) ^ ")"
  in
  let name, params = match x with
    | Accbands { timeperiod } -> "Accbands", ["timeperiod=" ^ string_of_int timeperiod]
    | Acos _ -> "Acos", []
    | Ad _ -> "Ad", []
    | Add _ -> "Add", []
    | Adosc { fast_period; slow_period } -> "Adosc", [
        "fast_period=" ^ string_of_int fast_period;
        "slow_period=" ^ string_of_int slow_period
      ]
    | Adx { timeperiod } -> "Adx", ["timeperiod=" ^ string_of_int timeperiod]
    | Adxr { timeperiod } -> "Adxr", ["timeperiod=" ^ string_of_int timeperiod]
    | Apo { fast_period; slow_period; ma_type } -> "Apo", [
        "fast_period=" ^ string_of_int fast_period;
        "slow_period=" ^ string_of_int slow_period;
        "ma_type=" ^ Ma_type.show ma_type
      ]
    | Aroon { timeperiod } -> "Aroon", ["timeperiod=" ^ string_of_int timeperiod]
    | Aroonosc { timeperiod } -> "Aroonosc", ["timeperiod=" ^ string_of_int timeperiod]
    | Asin _ -> "Asin", []
    | Atan _ -> "Atan", []
    | Atr { timeperiod } -> "Atr", ["timeperiod=" ^ string_of_int timeperiod]
    | Avgprice _ -> "Avgprice", []
    | Avgdev { timeperiod } -> "Avgdev", ["timeperiod=" ^ string_of_int timeperiod]
    | Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } -> "Bbands", [
        "timeperiod=" ^ string_of_int timeperiod;
        "nb_dev_up=" ^ string_of_float nb_dev_up;
        "nb_dev_dn=" ^ string_of_float nb_dev_dn;
        "ma_type=" ^ Ma_type.show ma_type
      ]
    | Beta { timeperiod } -> "Beta", ["timeperiod=" ^ string_of_int timeperiod]
    | Bop _ -> "Bop", []
    | Cci { timeperiod } -> "Cci", ["timeperiod=" ^ string_of_int timeperiod]
    | Cdl2crows _ -> "Cdl2crows", []
    | Cdl3blackcrows _ -> "Cdl3blackcrows", []
    | Cdl3inside _ -> "Cdl3inside", []
    | Cdl3linestrike _ -> "Cdl3linestrike", []
    | Cdl3outside _ -> "Cdl3outside", []
    | Cdl3starsinsouth _ -> "Cdl3starsinsouth", []
    | Cdl3whitesoldiers _ -> "Cdl3whitesoldiers", []
    | Cdlabandonedbaby { penetration } -> "Cdlabandonedbaby", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdladvanceblock _ -> "Cdladvanceblock", []
    | Cdlbelthold _ -> "Cdlbelthold", []
    | Cdlbreakaway _ -> "Cdlbreakaway", []
    | Cdlclosingmarubozu _ -> "Cdlclosingmarubozu", []
    | Cdlconcealbabyswall _ -> "Cdlconcealbabyswall", []
    | Cdlcounterattack _ -> "Cdlcounterattack", []
    | Cdldarkcloudcover { penetration } -> "Cdldarkcloudcover", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdldoji _ -> "Cdldoji", []
    | Cdldojistar _ -> "Cdldojistar", []
    | Cdldragonflydoji _ -> "Cdldragonflydoji", []
    | Cdlengulfing _ -> "Cdlengulfing", []
    | Cdleveningdojistar { penetration } -> "Cdleveningdojistar", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdleveningstar { penetration } -> "Cdleveningstar", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdlgapsidesidewhite _ -> "Cdlgapsidesidewhite", []
    | Cdlgravestonedoji _ -> "Cdlgravestonedoji", []
    | Cdlhammer _ -> "Cdlhammer", []
    | Cdlhangingman _ -> "Cdlhangingman", []
    | Cdlharami _ -> "Cdlharami", []
    | Cdlharamicross _ -> "Cdlharamicross", []
    | Cdlhighwave _ -> "Cdlhighwave", []
    | Cdlhikkake _ -> "Cdlhikkake", []
    | Cdlhikkakemod _ -> "Cdlhikkakemod", []
    | Cdlhomingpigeon _ -> "Cdlhomingpigeon", []
    | Cdlidentical3crows _ -> "Cdlidentical3crows", []
    | Cdlinneck _ -> "Cdlinneck", []
    | Cdlinvertedhammer _ -> "Cdlinvertedhammer", []
    | Cdlkicking _ -> "Cdlkicking", []
    | Cdlkickingbylength _ -> "Cdlkickingbylength", []
    | Cdlladderbottom _ -> "Cdlladderbottom", []
    | Cdllongleggeddoji _ -> "Cdllongleggeddoji", []
    | Cdllongline _ -> "Cdllongline", []
    | Cdlmarubozu _ -> "Cdlmarubozu", []
    | Cdlmatchinglow _ -> "Cdlmatchinglow", []
    | Cdlmathold { penetration } -> "Cdlmathold", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdlmorningdojistar { penetration } -> "Cdlmorningdojistar", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdlmorningstar { penetration } -> "Cdlmorningstar", [
        "penetration=" ^ string_of_float penetration
      ]
    | Cdlonneck _ -> "Cdlonneck", []
    | Cdlpiercing _ -> "Cdlpiercing", []
    | Cdlrickshawman _ -> "Cdlrickshawman", []
    | Cdlrisefall3methods _ -> "Cdlrisefall3methods", []
    | Cdlseparatinglines _ -> "Cdlseparatinglines", []
    | Cdlshootingstar _ -> "Cdlshootingstar", []
    | Cdlshortline _ -> "Cdlshortline", []
    | Cdlspinningtop _ -> "Cdlspinningtop", []
    | Cdlstalledpattern _ -> "Cdlstalledpattern", []
    | Cdlsticksandwich _ -> "Cdlsticksandwich", []
    | Cdltakuri _ -> "Cdltakuri", []
    | Cdltasukigap _ -> "Cdltasukigap", []
    | Cdlthrusting _ -> "Cdlthrusting", []
    | Cdltristar _ -> "Cdltristar", []
    | Cdlunique3river _ -> "Cdlunique3river", []
    | Cdlupsidegap2crows _ -> "Cdlupsidegap2crows", []
    | Cdlxsidegap3methods _ -> "Cdlxsidegap3methods", []
    | Ceil _ -> "Ceil", []
    | Cmo { timeperiod } -> "Cmo", ["timeperiod=" ^ string_of_int timeperiod]
    | Correl { timeperiod } -> "Correl", ["timeperiod=" ^ string_of_int timeperiod]
    | Cos _ -> "Cos", []
    | Cosh _ -> "Cosh", []
    | Dema { timeperiod } -> "Dema", ["timeperiod=" ^ string_of_int timeperiod]
    | Div _ -> "Div", []
    | Dx { timeperiod } -> "Dx", ["timeperiod=" ^ string_of_int timeperiod]
    | Ema { timeperiod } -> "Ema", ["timeperiod=" ^ string_of_int timeperiod]
    | Exp _ -> "Exp", []
    | Floor _ -> "Floor", []
    | Ht_dcperiod _ -> "Ht_dcperiod", []
    | Ht_dcphase _ -> "Ht_dcphase", []
    | Ht_phasor _ -> "Ht_phasor", []
    | Ht_sine _ -> "Ht_sine", []
    | Ht_trendline _ -> "Ht_trendline", []
    | Ht_trendmode _ -> "Ht_trendmode", []
    | Imi { timeperiod } -> "Imi", ["timeperiod=" ^ string_of_int timeperiod]
    | Kama { timeperiod } -> "Kama", ["timeperiod=" ^ string_of_int timeperiod]
    | Linearreg { timeperiod } -> "Linearreg", ["timeperiod=" ^ string_of_int timeperiod]
    | Linearreg_angle { timeperiod } -> "Linearreg_angle", ["timeperiod=" ^ string_of_int timeperiod]
    | Linearreg_intercept { timeperiod } -> "Linearreg_intercept", ["timeperiod=" ^ string_of_int timeperiod]
    | Linearreg_slope { timeperiod } -> "Linearreg_slope", ["timeperiod=" ^ string_of_int timeperiod]
    | Ln _ -> "Ln", []
    | Log10 _ -> "Log10", []
    | Ma { timeperiod; ma_type } -> "Ma", [
        "timeperiod=" ^ string_of_int timeperiod;
        "ma_type=" ^ Ma_type.show ma_type
      ]
    | Macd { fast_period; slow_period; signal_period } -> "Macd", [
        "fast_period=" ^ string_of_int fast_period;
        "slow_period=" ^ string_of_int slow_period;
        "signal_period=" ^ string_of_int signal_period
      ]
    | Macdext { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type } -> "Macdext", [
        "fast_period=" ^ string_of_int fast_period;
        "fast_ma_type=" ^ Ma_type.show fast_ma_type;
        "slow_period=" ^ string_of_int slow_period;
        "slow_ma_type=" ^ Ma_type.show slow_ma_type;
        "signal_period=" ^ string_of_int signal_period;
        "signal_ma_type=" ^ Ma_type.show signal_ma_type
      ]
    | Macdfix { signal_period } -> "Macdfix", [
        "signal_period=" ^ string_of_int signal_period
      ]
    | Mama { fast_limit; slow_limit } -> "Mama", [
        "fast_limit=" ^ string_of_float fast_limit;
        "slow_limit=" ^ string_of_float slow_limit
      ]
    | Mavp { min_period; max_period; ma_type } -> "Mavp", [
        "min_period=" ^ string_of_int min_period;
        "max_period=" ^ string_of_int max_period;
        "ma_type=" ^ Ma_type.show ma_type
      ]
    | Max { timeperiod } -> "Max", ["timeperiod=" ^ string_of_int timeperiod]
    | Maxindex { timeperiod } -> "Maxindex", ["timeperiod=" ^ string_of_int timeperiod]
    | Medprice _ -> "Medprice", []
    | Mfi { timeperiod } -> "Mfi", ["timeperiod=" ^ string_of_int timeperiod]
    | Midpoint { timeperiod } -> "Midpoint", ["timeperiod=" ^ string_of_int timeperiod]
    | Midprice { timeperiod } -> "Midprice", ["timeperiod=" ^ string_of_int timeperiod]
    | Min { timeperiod } -> "Min", ["timeperiod=" ^ string_of_int timeperiod]
    | Minindex { timeperiod } -> "Minindex", ["timeperiod=" ^ string_of_int timeperiod]
    | Minmax { timeperiod } -> "Minmax", ["timeperiod=" ^ string_of_int timeperiod]
    | Minmaxindex { timeperiod } -> "Minmaxindex", ["timeperiod=" ^ string_of_int timeperiod]
    | Minus_di { timeperiod } -> "Minus_di", ["timeperiod=" ^ string_of_int timeperiod]
    | Minus_dm { timeperiod } -> "Minus_dm", ["timeperiod=" ^ string_of_int timeperiod]
    | Mom { timeperiod } -> "Mom", ["timeperiod=" ^ string_of_int timeperiod]
    | Mult _ -> "Mult", []
    | Natr { timeperiod } -> "Natr", ["timeperiod=" ^ string_of_int timeperiod]
    | Obv _ -> "Obv", []
    | Plus_di { timeperiod } -> "Plus_di", ["timeperiod=" ^ string_of_int timeperiod]
    | Plus_dm { timeperiod } -> "Plus_dm", ["timeperiod=" ^ string_of_int timeperiod]
    | Ppo { fast_period; slow_period; ma_type } -> "Ppo", [
        "fast_period=" ^ string_of_int fast_period;
        "slow_period=" ^ string_of_int slow_period;
        "ma_type=" ^ Ma_type.show ma_type
      ]
    | Roc { timeperiod } -> "Roc", ["timeperiod=" ^ string_of_int timeperiod]
    | Rocp { timeperiod } -> "Rocp", ["timeperiod=" ^ string_of_int timeperiod]
    | Rocr { timeperiod } -> "Rocr", ["timeperiod=" ^ string_of_int timeperiod]
    | Rocr100 { timeperiod } -> "Rocr100", ["timeperiod=" ^ string_of_int timeperiod]
    | Rsi { timeperiod } -> "Rsi", ["timeperiod=" ^ string_of_int timeperiod]
    | Sar { acceleration; maximum } -> "Sar", [
        "acceleration=" ^ string_of_float acceleration;
        "maximum=" ^ string_of_float maximum
      ]
    | Sarext { start_value; offset_on_reverse; acceleration_init_long; acceleration_long; acceleration_max_long; acceleration_init_short; acceleration_short; acceleration_max_short } -> "Sarext", [
        "start_value=" ^ string_of_float start_value;
        "offset_on_reverse=" ^ string_of_float offset_on_reverse;
        "acceleration_init_long=" ^ string_of_float acceleration_init_long;
        "acceleration_long=" ^ string_of_float acceleration_long;
        "acceleration_max_long=" ^ string_of_float acceleration_max_long;
        "acceleration_init_short=" ^ string_of_float acceleration_init_short;
        "acceleration_short=" ^ string_of_float acceleration_short;
        "acceleration_max_short=" ^ string_of_float acceleration_max_short
      ]
    | Sin _ -> "Sin", []
    | Sinh _ -> "Sinh", []
    | Sma { timeperiod } -> "Sma", ["timeperiod=" ^ string_of_int timeperiod]
    | Sqrt _ -> "Sqrt", []
    | Stddev { timeperiod; nb_dev } -> "Stddev", [
        "timeperiod=" ^ string_of_int timeperiod;
        "nb_dev=" ^ string_of_float nb_dev
      ]
    | Stoch { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type } -> "Stoch", [
        "fast_k_period=" ^ string_of_int fast_k_period;
        "slow_k_period=" ^ string_of_int slow_k_period;
        "slow_k_ma_type=" ^ Ma_type.show slow_k_ma_type;
        "slow_d_period=" ^ string_of_int slow_d_period;
        "slow_d_ma_type=" ^ Ma_type.show slow_d_ma_type
      ]
    | Stochf { fast_k_period; fast_d_period; fast_d_ma_type } -> "Stochf", [
        "fast_k_period=" ^ string_of_int fast_k_period;
        "fast_d_period=" ^ string_of_int fast_d_period;
        "fast_d_ma_type=" ^ Ma_type.show fast_d_ma_type
      ]
    | Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } -> "Stochrsi", [
        "timeperiod=" ^ string_of_int timeperiod;
        "fast_k_period=" ^ string_of_int fast_k_period;
        "fast_d_period=" ^ string_of_int fast_d_period;
        "fast_d_ma_type=" ^ Ma_type.show fast_d_ma_type
      ]
    | Sub _ -> "Sub", []
    | Sum { timeperiod } -> "Sum", ["timeperiod=" ^ string_of_int timeperiod]
    | T3 { timeperiod; v_factor } -> "T3", [
        "timeperiod=" ^ string_of_int timeperiod;
        "v_factor=" ^ string_of_float v_factor
      ]
    | Tan _ -> "Tan", []
    | Tanh _ -> "Tanh", []
    | Tema { timeperiod } -> "Tema", ["timeperiod=" ^ string_of_int timeperiod]
    | Trange _ -> "Trange", []
    | Trima { timeperiod } -> "Trima", ["timeperiod=" ^ string_of_int timeperiod]
    | Trix { timeperiod } -> "Trix", ["timeperiod=" ^ string_of_int timeperiod]
    | Tsf { timeperiod } -> "Tsf", ["timeperiod=" ^ string_of_int timeperiod]
    | Typprice _ -> "Typprice", []
    | Ultosc { timeperiod1; timeperiod2; timeperiod3 } -> "Ultosc", [
        "timeperiod1=" ^ string_of_int timeperiod1;
        "timeperiod2=" ^ string_of_int timeperiod2;
        "timeperiod3=" ^ string_of_int timeperiod3
      ]
    | Var { timeperiod; nb_dev } -> "Var", [
        "timeperiod=" ^ string_of_int timeperiod;
        "nb_dev=" ^ string_of_float nb_dev
      ]
    | Wclprice _ -> "Wclprice", []
    | Willr { timeperiod } -> "Willr", ["timeperiod=" ^ string_of_int timeperiod]
    | Wma { timeperiod } -> "Wma", ["timeperiod=" ^ string_of_int timeperiod]
  in
  Format.fprintf fmt "@[%s%s@]" name (format_params params)

let hash : type a b. (a, b) t -> int = function
  | Accbands { timeperiod } -> Hash.combine2 (Hash.int 0) (Hash.int timeperiod)
  | Acos () -> Hash.int 1
  | Ad () -> Hash.int 2
  | Add () -> Hash.int 3
  | Adosc { fast_period; slow_period } ->
    Hash.combine3 (Hash.int 4) (Hash.int fast_period) (Hash.int slow_period)
  | Adx { timeperiod } -> Hash.combine2 (Hash.int 5) (Hash.int timeperiod)
  | Adxr { timeperiod } -> Hash.combine2 (Hash.int 6) (Hash.int timeperiod)
  | Apo { fast_period; slow_period; ma_type } ->
    Hash.combine4 (Hash.int 7) (Hash.int fast_period) (Hash.int slow_period)
      (Hash.int (Ma_type.to_int ma_type))
  | Aroon { timeperiod } -> Hash.combine2 (Hash.int 8) (Hash.int timeperiod)
  | Aroonosc { timeperiod } -> Hash.combine2 (Hash.int 9) (Hash.int timeperiod)
  | Asin () -> Hash.int 10
  | Atan () -> Hash.int 11
  | Atr { timeperiod } -> Hash.combine2 (Hash.int 12) (Hash.int timeperiod)
  | Avgprice () -> Hash.int 13
  | Avgdev { timeperiod } -> Hash.combine2 (Hash.int 14) (Hash.int timeperiod)
  | Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
    Hash.combine5 (Hash.int 15) (Hash.int timeperiod) (Hash.poly nb_dev_up)
      (Hash.poly nb_dev_dn)
      (Hash.int (Ma_type.to_int ma_type))
  | Beta { timeperiod } -> Hash.combine2 (Hash.int 16) (Hash.int timeperiod)
  | Bop () -> Hash.int 17
  | Cci { timeperiod } -> Hash.combine2 (Hash.int 18) (Hash.int timeperiod)
  | Cdl2crows () -> Hash.int 19
  | Cdl3blackcrows () -> Hash.int 20
  | Cdl3inside () -> Hash.int 21
  | Cdl3linestrike () -> Hash.int 22
  | Cdl3outside () -> Hash.int 23
  | Cdl3starsinsouth () -> Hash.int 24
  | Cdl3whitesoldiers () -> Hash.int 25
  | Cdlabandonedbaby { penetration } ->
    Hash.combine2 (Hash.int 26) (Hash.poly penetration)
  | Cdladvanceblock () -> Hash.int 27
  | Cdlbelthold () -> Hash.int 28
  | Cdlbreakaway () -> Hash.int 29
  | Cdlclosingmarubozu () -> Hash.int 30
  | Cdlconcealbabyswall () -> Hash.int 31
  | Cdlcounterattack () -> Hash.int 32
  | Cdldarkcloudcover { penetration } ->
    Hash.combine2 (Hash.int 33) (Hash.poly penetration)
  | Cdldoji () -> Hash.int 34
  | Cdldojistar () -> Hash.int 35
  | Cdldragonflydoji () -> Hash.int 36
  | Cdlengulfing () -> Hash.int 37
  | Cdleveningdojistar { penetration } ->
    Hash.combine2 (Hash.int 38) (Hash.poly penetration)
  | Cdleveningstar { penetration } ->
    Hash.combine2 (Hash.int 39) (Hash.poly penetration)
  | Cdlgapsidesidewhite () -> Hash.int 40
  | Cdlgravestonedoji () -> Hash.int 41
  | Cdlhammer () -> Hash.int 42
  | Cdlhangingman () -> Hash.int 43
  | Cdlharami () -> Hash.int 44
  | Cdlharamicross () -> Hash.int 45
  | Cdlhighwave () -> Hash.int 46
  | Cdlhikkake () -> Hash.int 47
  | Cdlhikkakemod () -> Hash.int 48
  | Cdlhomingpigeon () -> Hash.int 49
  | Cdlidentical3crows () -> Hash.int 50
  | Cdlinneck () -> Hash.int 51
  | Cdlinvertedhammer () -> Hash.int 52
  | Cdlkicking () -> Hash.int 53
  | Cdlkickingbylength () -> Hash.int 54
  | Cdlladderbottom () -> Hash.int 55
  | Cdllongleggeddoji () -> Hash.int 56
  | Cdllongline () -> Hash.int 57
  | Cdlmarubozu () -> Hash.int 58
  | Cdlmatchinglow () -> Hash.int 59
  | Cdlmathold { penetration } ->
    Hash.combine2 (Hash.int 60) (Hash.poly penetration)
  | Cdlmorningdojistar { penetration } ->
    Hash.combine2 (Hash.int 61) (Hash.poly penetration)
  | Cdlmorningstar { penetration } ->
    Hash.combine2 (Hash.int 62) (Hash.poly penetration)
  | Cdlonneck () -> Hash.int 63
  | Cdlpiercing () -> Hash.int 64
  | Cdlrickshawman () -> Hash.int 65
  | Cdlrisefall3methods () -> Hash.int 66
  | Cdlseparatinglines () -> Hash.int 67
  | Cdlshootingstar () -> Hash.int 68
  | Cdlshortline () -> Hash.int 69
  | Cdlspinningtop () -> Hash.int 70
  | Cdlstalledpattern () -> Hash.int 71
  | Cdlsticksandwich () -> Hash.int 72
  | Cdltakuri () -> Hash.int 73
  | Cdltasukigap () -> Hash.int 74
  | Cdlthrusting () -> Hash.int 75
  | Cdltristar () -> Hash.int 76
  | Cdlunique3river () -> Hash.int 77
  | Cdlupsidegap2crows () -> Hash.int 78
  | Cdlxsidegap3methods () -> Hash.int 79
  | Ceil () -> Hash.int 80
  | Cmo { timeperiod } -> Hash.combine2 (Hash.int 81) (Hash.int timeperiod)
  | Correl { timeperiod } -> Hash.combine2 (Hash.int 82) (Hash.int timeperiod)
  | Cos () -> Hash.int 83
  | Cosh () -> Hash.int 84
  | Dema { timeperiod } -> Hash.combine2 (Hash.int 85) (Hash.int timeperiod)
  | Div () -> Hash.int 86
  | Dx { timeperiod } -> Hash.combine2 (Hash.int 87) (Hash.int timeperiod)
  | Ema { timeperiod } -> Hash.combine2 (Hash.int 88) (Hash.int timeperiod)
  | Exp () -> Hash.int 89
  | Floor () -> Hash.int 90
  | Ht_dcperiod () -> Hash.int 91
  | Ht_dcphase () -> Hash.int 92
  | Ht_phasor () -> Hash.int 93
  | Ht_sine () -> Hash.int 94
  | Ht_trendline () -> Hash.int 95
  | Ht_trendmode () -> Hash.int 96
  | Imi { timeperiod } -> Hash.combine2 (Hash.int 97) (Hash.int timeperiod)
  | Kama { timeperiod } -> Hash.combine2 (Hash.int 98) (Hash.int timeperiod)
  | Linearreg { timeperiod } ->
    Hash.combine2 (Hash.int 99) (Hash.int timeperiod)
  | Linearreg_angle { timeperiod } ->
    Hash.combine2 (Hash.int 100) (Hash.int timeperiod)
  | Linearreg_intercept { timeperiod } ->
    Hash.combine2 (Hash.int 101) (Hash.int timeperiod)
  | Linearreg_slope { timeperiod } ->
    Hash.combine2 (Hash.int 102) (Hash.int timeperiod)
  | Ln () -> Hash.int 103
  | Log10 () -> Hash.int 104
  | Ma { timeperiod; ma_type } ->
    Hash.combine3 (Hash.int 105) (Hash.int timeperiod)
      (Hash.int (Ma_type.to_int ma_type))
  | Macd { fast_period; slow_period; signal_period } ->
    Hash.combine4 (Hash.int 106) (Hash.int fast_period) (Hash.int slow_period)
      (Hash.int signal_period)
  | Macdext
      {
        fast_period;
        fast_ma_type;
        slow_period;
        slow_ma_type;
        signal_period;
        signal_ma_type;
      } ->
    Hash.combine6 (Hash.int 107) (Hash.int fast_period)
      (Hash.int (Ma_type.to_int fast_ma_type))
      (Hash.int slow_period)
      (Hash.int (Ma_type.to_int slow_ma_type))
      (Hash.combine2 (Hash.int signal_period)
         (Hash.int (Ma_type.to_int signal_ma_type)))
  | Macdfix { signal_period } ->
    Hash.combine2 (Hash.int 108) (Hash.int signal_period)
  | Mama { fast_limit; slow_limit } ->
    Hash.combine3 (Hash.int 109) (Hash.poly fast_limit) (Hash.poly slow_limit)
  | Mavp { min_period; max_period; ma_type } ->
    Hash.combine4 (Hash.int 110) (Hash.int min_period) (Hash.int max_period)
      (Hash.int (Ma_type.to_int ma_type))
  | Max { timeperiod } -> Hash.combine2 (Hash.int 111) (Hash.int timeperiod)
  | Maxindex { timeperiod } ->
    Hash.combine2 (Hash.int 112) (Hash.int timeperiod)
  | Medprice () -> Hash.int 113
  | Mfi { timeperiod } -> Hash.combine2 (Hash.int 114) (Hash.int timeperiod)
  | Midpoint { timeperiod } ->
    Hash.combine2 (Hash.int 115) (Hash.int timeperiod)
  | Midprice { timeperiod } ->
    Hash.combine2 (Hash.int 116) (Hash.int timeperiod)
  | Min { timeperiod } -> Hash.combine2 (Hash.int 117) (Hash.int timeperiod)
  | Minindex { timeperiod } ->
    Hash.combine2 (Hash.int 118) (Hash.int timeperiod)
  | Minmax { timeperiod } -> Hash.combine2 (Hash.int 119) (Hash.int timeperiod)
  | Minmaxindex { timeperiod } ->
    Hash.combine2 (Hash.int 120) (Hash.int timeperiod)
  | Minus_di { timeperiod } ->
    Hash.combine2 (Hash.int 121) (Hash.int timeperiod)
  | Minus_dm { timeperiod } ->
    Hash.combine2 (Hash.int 122) (Hash.int timeperiod)
  | Mom { timeperiod } -> Hash.combine2 (Hash.int 123) (Hash.int timeperiod)
  | Mult () -> Hash.int 124
  | Natr { timeperiod } -> Hash.combine2 (Hash.int 125) (Hash.int timeperiod)
  | Obv () -> Hash.int 126
  | Plus_di { timeperiod } -> Hash.combine2 (Hash.int 127) (Hash.int timeperiod)
  | Plus_dm { timeperiod } -> Hash.combine2 (Hash.int 128) (Hash.int timeperiod)
  | Ppo { fast_period; slow_period; ma_type } ->
    Hash.combine4 (Hash.int 129) (Hash.int fast_period) (Hash.int slow_period)
      (Hash.int (Ma_type.to_int ma_type))
  | Roc { timeperiod } -> Hash.combine2 (Hash.int 130) (Hash.int timeperiod)
  | Rocp { timeperiod } -> Hash.combine2 (Hash.int 131) (Hash.int timeperiod)
  | Rocr { timeperiod } -> Hash.combine2 (Hash.int 132) (Hash.int timeperiod)
  | Rocr100 { timeperiod } -> Hash.combine2 (Hash.int 133) (Hash.int timeperiod)
  | Rsi { timeperiod } -> Hash.combine2 (Hash.int 134) (Hash.int timeperiod)
  | Sar { acceleration; maximum } ->
    Hash.combine3 (Hash.int 135) (Hash.poly acceleration) (Hash.poly maximum)
  | Sarext
      {
        start_value;
        offset_on_reverse;
        acceleration_init_long;
        acceleration_long;
        acceleration_max_long;
        acceleration_init_short;
        acceleration_short;
        acceleration_max_short;
      } ->
    Hash.combine2 (Hash.int 136)
      (Hash.combine6 (Hash.poly start_value)
         (Hash.poly offset_on_reverse)
         (Hash.poly acceleration_init_long)
         (Hash.poly acceleration_long)
         (Hash.poly acceleration_max_long)
         (Hash.combine3
            (Hash.poly acceleration_init_short)
            (Hash.poly acceleration_short)
            (Hash.poly acceleration_max_short)))
  | Sin () -> Hash.int 137
  | Sinh () -> Hash.int 138
  | Sma { timeperiod } -> Hash.combine2 (Hash.int 139) (Hash.int timeperiod)
  | Sqrt () -> Hash.int 140
  | Stddev { timeperiod; nb_dev } ->
    Hash.combine3 (Hash.int 141) (Hash.int timeperiod) (Hash.poly nb_dev)
  | Stoch
      {
        fast_k_period;
        slow_k_period;
        slow_k_ma_type;
        slow_d_period;
        slow_d_ma_type;
      } ->
    Hash.combine6 (Hash.int 142) (Hash.int fast_k_period)
      (Hash.int slow_k_period)
      (Hash.int (Ma_type.to_int slow_k_ma_type))
      (Hash.int slow_d_period)
      (Hash.int (Ma_type.to_int slow_d_ma_type))
  | Stochf { fast_k_period; fast_d_period; fast_d_ma_type } ->
    Hash.combine4 (Hash.int 143) (Hash.int fast_k_period)
      (Hash.int fast_d_period)
      (Hash.int (Ma_type.to_int fast_d_ma_type))
  | Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
    Hash.combine5 (Hash.int 144) (Hash.int timeperiod) (Hash.int fast_k_period)
      (Hash.int fast_d_period)
      (Hash.int (Ma_type.to_int fast_d_ma_type))
  | Sub () -> Hash.int 145
  | Sum { timeperiod } -> Hash.combine2 (Hash.int 146) (Hash.int timeperiod)
  | T3 { timeperiod; v_factor } ->
    Hash.combine3 (Hash.int 147) (Hash.int timeperiod) (Hash.poly v_factor)
  | Tan () -> Hash.int 148
  | Tanh () -> Hash.int 149
  | Tema { timeperiod } -> Hash.combine2 (Hash.int 150) (Hash.int timeperiod)
  | Trange () -> Hash.int 151
  | Trima { timeperiod } -> Hash.combine2 (Hash.int 152) (Hash.int timeperiod)
  | Trix { timeperiod } -> Hash.combine2 (Hash.int 153) (Hash.int timeperiod)
  | Tsf { timeperiod } -> Hash.combine2 (Hash.int 154) (Hash.int timeperiod)
  | Typprice () -> Hash.int 155
  | Ultosc { timeperiod1; timeperiod2; timeperiod3 } ->
    Hash.combine4 (Hash.int 156) (Hash.int timeperiod1) (Hash.int timeperiod2)
      (Hash.int timeperiod3)
  | Var { timeperiod; nb_dev } ->
    Hash.combine3 (Hash.int 157) (Hash.int timeperiod) (Hash.poly nb_dev)
  | Wclprice () -> Hash.int 158
  | Willr { timeperiod } -> Hash.combine2 (Hash.int 159) (Hash.int timeperiod)
  | Wma { timeperiod } -> Hash.combine2 (Hash.int 160) (Hash.int timeperiod)

let equal : type a b c d. (a, b) t -> (c, d) t -> bool =
 fun x y ->
  match (x, y) with
  | Accbands { timeperiod = t1 }, Accbands { timeperiod = t2 } -> t1 = t2
  | Acos (), Acos () -> true
  | Ad (), Ad () -> true
  | Add (), Add () -> true
  | ( Adosc { fast_period = fp1; slow_period = sp1 },
      Adosc { fast_period = fp2; slow_period = sp2 } ) ->
    fp1 = fp2 && sp1 = sp2
  | Adx { timeperiod = t1 }, Adx { timeperiod = t2 } -> t1 = t2
  | Adxr { timeperiod = t1 }, Adxr { timeperiod = t2 } -> t1 = t2
  | ( Apo { fast_period = fp1; slow_period = sp1; ma_type = mt1 },
      Apo { fast_period = fp2; slow_period = sp2; ma_type = mt2 } ) ->
    fp1 = fp2 && sp1 = sp2 && Stdlib.( = ) mt1 mt2
  | Aroon { timeperiod = t1 }, Aroon { timeperiod = t2 } -> t1 = t2
  | Aroonosc { timeperiod = t1 }, Aroonosc { timeperiod = t2 } -> t1 = t2
  | Asin (), Asin () -> true
  | Atan (), Atan () -> true
  | Atr { timeperiod = t1 }, Atr { timeperiod = t2 } -> t1 = t2
  | Avgprice (), Avgprice () -> true
  | Avgdev { timeperiod = t1 }, Avgdev { timeperiod = t2 } -> t1 = t2
  | ( Bbands
        { timeperiod = t1; nb_dev_up = ndu1; nb_dev_dn = ndd1; ma_type = mt1 },
      Bbands
        { timeperiod = t2; nb_dev_up = ndu2; nb_dev_dn = ndd2; ma_type = mt2 } )
    ->
    t1 = t2 && Float.equal ndu1 ndu2 && Float.equal ndd1 ndd2
    && Stdlib.( = ) mt1 mt2
  | Beta { timeperiod = t1 }, Beta { timeperiod = t2 } -> t1 = t2
  | Bop (), Bop () -> true
  | Cci { timeperiod = t1 }, Cci { timeperiod = t2 } -> t1 = t2
  | Cdl2crows (), Cdl2crows () -> true
  | Cdl3blackcrows (), Cdl3blackcrows () -> true
  | Cdl3inside (), Cdl3inside () -> true
  | Cdl3linestrike (), Cdl3linestrike () -> true
  | Cdl3outside (), Cdl3outside () -> true
  | Cdl3starsinsouth (), Cdl3starsinsouth () -> true
  | Cdl3whitesoldiers (), Cdl3whitesoldiers () -> true
  | Cdlabandonedbaby { penetration = p1 }, Cdlabandonedbaby { penetration = p2 }
    ->
    Float.equal p1 p2
  | Cdladvanceblock (), Cdladvanceblock () -> true
  | Cdlbelthold (), Cdlbelthold () -> true
  | Cdlbreakaway (), Cdlbreakaway () -> true
  | Cdlclosingmarubozu (), Cdlclosingmarubozu () -> true
  | Cdlconcealbabyswall (), Cdlconcealbabyswall () -> true
  | Cdlcounterattack (), Cdlcounterattack () -> true
  | ( Cdldarkcloudcover { penetration = p1 },
      Cdldarkcloudcover { penetration = p2 } ) ->
    Float.equal p1 p2
  | Cdldoji (), Cdldoji () -> true
  | Cdldojistar (), Cdldojistar () -> true
  | Cdldragonflydoji (), Cdldragonflydoji () -> true
  | Cdlengulfing (), Cdlengulfing () -> true
  | ( Cdleveningdojistar { penetration = p1 },
      Cdleveningdojistar { penetration = p2 } ) ->
    Float.equal p1 p2
  | Cdleveningstar { penetration = p1 }, Cdleveningstar { penetration = p2 } ->
    Float.equal p1 p2
  | Cdlgapsidesidewhite (), Cdlgapsidesidewhite () -> true
  | Cdlgravestonedoji (), Cdlgravestonedoji () -> true
  | Cdlhammer (), Cdlhammer () -> true
  | Cdlhangingman (), Cdlhangingman () -> true
  | Cdlharami (), Cdlharami () -> true
  | Cdlharamicross (), Cdlharamicross () -> true
  | Cdlhighwave (), Cdlhighwave () -> true
  | Cdlhikkake (), Cdlhikkake () -> true
  | Cdlhikkakemod (), Cdlhikkakemod () -> true
  | Cdlhomingpigeon (), Cdlhomingpigeon () -> true
  | Cdlidentical3crows (), Cdlidentical3crows () -> true
  | Cdlinneck (), Cdlinneck () -> true
  | Cdlinvertedhammer (), Cdlinvertedhammer () -> true
  | Cdlkicking (), Cdlkicking () -> true
  | Cdlkickingbylength (), Cdlkickingbylength () -> true
  | Cdlladderbottom (), Cdlladderbottom () -> true
  | Cdllongleggeddoji (), Cdllongleggeddoji () -> true
  | Cdllongline (), Cdllongline () -> true
  | Cdlmarubozu (), Cdlmarubozu () -> true
  | Cdlmatchinglow (), Cdlmatchinglow () -> true
  | Cdlmathold { penetration = p1 }, Cdlmathold { penetration = p2 } ->
    Float.equal p1 p2
  | ( Cdlmorningdojistar { penetration = p1 },
      Cdlmorningdojistar { penetration = p2 } ) ->
    Float.equal p1 p2
  | Cdlmorningstar { penetration = p1 }, Cdlmorningstar { penetration = p2 } ->
    Float.equal p1 p2
  | Cdlonneck (), Cdlonneck () -> true
  | Cdlpiercing (), Cdlpiercing () -> true
  | Cdlrickshawman (), Cdlrickshawman () -> true
  | Cdlrisefall3methods (), Cdlrisefall3methods () -> true
  | Cdlseparatinglines (), Cdlseparatinglines () -> true
  | Cdlshootingstar (), Cdlshootingstar () -> true
  | Cdlshortline (), Cdlshortline () -> true
  | Cdlspinningtop (), Cdlspinningtop () -> true
  | Cdlstalledpattern (), Cdlstalledpattern () -> true
  | Cdlsticksandwich (), Cdlsticksandwich () -> true
  | Cdltakuri (), Cdltakuri () -> true
  | Cdltasukigap (), Cdltasukigap () -> true
  | Cdlthrusting (), Cdlthrusting () -> true
  | Cdltristar (), Cdltristar () -> true
  | Cdlunique3river (), Cdlunique3river () -> true
  | Cdlupsidegap2crows (), Cdlupsidegap2crows () -> true
  | Cdlxsidegap3methods (), Cdlxsidegap3methods () -> true
  | Ceil (), Ceil () -> true
  | Cmo { timeperiod = t1 }, Cmo { timeperiod = t2 } -> t1 = t2
  | Correl { timeperiod = t1 }, Correl { timeperiod = t2 } -> t1 = t2
  | Cos (), Cos () -> true
  | Cosh (), Cosh () -> true
  | Dema { timeperiod = t1 }, Dema { timeperiod = t2 } -> t1 = t2
  | Div (), Div () -> true
  | Dx { timeperiod = t1 }, Dx { timeperiod = t2 } -> t1 = t2
  | Ema { timeperiod = t1 }, Ema { timeperiod = t2 } -> t1 = t2
  | Exp (), Exp () -> true
  | Floor (), Floor () -> true
  | Ht_dcperiod (), Ht_dcperiod () -> true
  | Ht_dcphase (), Ht_dcphase () -> true
  | Ht_phasor (), Ht_phasor () -> true
  | Ht_sine (), Ht_sine () -> true
  | Ht_trendline (), Ht_trendline () -> true
  | Ht_trendmode (), Ht_trendmode () -> true
  | Kama { timeperiod = t1 }, Kama { timeperiod = t2 } -> t1 = t2
  | Linearreg { timeperiod = t1 }, Linearreg { timeperiod = t2 } -> t1 = t2
  | Linearreg_angle { timeperiod = t1 }, Linearreg_angle { timeperiod = t2 } ->
    t1 = t2
  | ( Linearreg_intercept { timeperiod = t1 },
      Linearreg_intercept { timeperiod = t2 } ) ->
    t1 = t2
  | Linearreg_slope { timeperiod = t1 }, Linearreg_slope { timeperiod = t2 } ->
    t1 = t2
  | Ln (), Ln () -> true
  | Log10 (), Log10 () -> true
  | Ma { timeperiod = t1; ma_type = mt1 }, Ma { timeperiod = t2; ma_type = mt2 }
    ->
    t1 = t2 && Stdlib.( = ) mt1 mt2
  | ( Macd { fast_period = fp1; slow_period = sp1; signal_period = sigp1 },
      Macd { fast_period = fp2; slow_period = sp2; signal_period = sigp2 } ) ->
    fp1 = fp2 && sp1 = sp2 && sigp1 = sigp2
  | ( Macdext
        {
          fast_period = fp1;
          fast_ma_type = fmt1;
          slow_period = sp1;
          slow_ma_type = smt1;
          signal_period = sigp1;
          signal_ma_type = sigmt1;
        },
      Macdext
        {
          fast_period = fp2;
          fast_ma_type = fmt2;
          slow_period = sp2;
          slow_ma_type = smt2;
          signal_period = sigp2;
          signal_ma_type = sigmt2;
        } ) ->
    fp1 = fp2 && Stdlib.( = ) fmt1 fmt2 && sp1 = sp2 && Stdlib.( = ) smt1 smt2
    && sigp1 = sigp2 && Stdlib.( = ) sigmt1 sigmt2
  | Macdfix { signal_period = sp1 }, Macdfix { signal_period = sp2 } ->
    sp1 = sp2
  | ( Mama { fast_limit = fl1; slow_limit = sl1 },
      Mama { fast_limit = fl2; slow_limit = sl2 } ) ->
    Float.equal fl1 fl2 && Float.equal sl1 sl2
  | ( Mavp { min_period = minp1; max_period = maxp1; ma_type = mt1 },
      Mavp { min_period = minp2; max_period = maxp2; ma_type = mt2 } ) ->
    minp1 = minp2 && maxp1 = maxp2 && Stdlib.( = ) mt1 mt2
  | Max { timeperiod = t1 }, Max { timeperiod = t2 } -> t1 = t2
  | Maxindex { timeperiod = t1 }, Maxindex { timeperiod = t2 } -> t1 = t2
  | Medprice (), Medprice () -> true
  | Mfi { timeperiod = t1 }, Mfi { timeperiod = t2 } -> t1 = t2
  | Midpoint { timeperiod = t1 }, Midpoint { timeperiod = t2 } -> t1 = t2
  | Midprice { timeperiod = t1 }, Midprice { timeperiod = t2 } -> t1 = t2
  | Min { timeperiod = t1 }, Min { timeperiod = t2 } -> t1 = t2
  | Minindex { timeperiod = t1 }, Minindex { timeperiod = t2 } -> t1 = t2
  | Minmax { timeperiod = t1 }, Minmax { timeperiod = t2 } -> t1 = t2
  | Minmaxindex { timeperiod = t1 }, Minmaxindex { timeperiod = t2 } -> t1 = t2
  | Minus_di { timeperiod = t1 }, Minus_di { timeperiod = t2 } -> t1 = t2
  | Minus_dm { timeperiod = t1 }, Minus_dm { timeperiod = t2 } -> t1 = t2
  | Mom { timeperiod = t1 }, Mom { timeperiod = t2 } -> t1 = t2
  | Mult (), Mult () -> true
  | Natr { timeperiod = t1 }, Natr { timeperiod = t2 } -> t1 = t2
  | Obv (), Obv () -> true
  | Plus_di { timeperiod = t1 }, Plus_di { timeperiod = t2 } -> t1 = t2
  | Plus_dm { timeperiod = t1 }, Plus_dm { timeperiod = t2 } -> t1 = t2
  | ( Ppo { fast_period = fp1; slow_period = sp1; ma_type = mt1 },
      Ppo { fast_period = fp2; slow_period = sp2; ma_type = mt2 } ) ->
    fp1 = fp2 && sp1 = sp2 && Stdlib.( = ) mt1 mt2
  | Roc { timeperiod = t1 }, Roc { timeperiod = t2 } -> t1 = t2
  | Rocp { timeperiod = t1 }, Rocp { timeperiod = t2 } -> t1 = t2
  | Rocr { timeperiod = t1 }, Rocr { timeperiod = t2 } -> t1 = t2
  | Rocr100 { timeperiod = t1 }, Rocr100 { timeperiod = t2 } -> t1 = t2
  | Rsi { timeperiod = t1 }, Rsi { timeperiod = t2 } -> t1 = t2
  | ( Sar { acceleration = a1; maximum = m1 },
      Sar { acceleration = a2; maximum = m2 } ) ->
    Float.equal a1 a2 && Float.equal m1 m2
  | ( Sarext
        {
          start_value = sv1;
          offset_on_reverse = oor1;
          acceleration_init_long = ail1;
          acceleration_long = al1;
          acceleration_max_long = aml1;
          acceleration_init_short = ais1;
          acceleration_short = as1;
          acceleration_max_short = ams1;
        },
      Sarext
        {
          start_value = sv2;
          offset_on_reverse = oor2;
          acceleration_init_long = ail2;
          acceleration_long = al2;
          acceleration_max_long = aml2;
          acceleration_init_short = ais2;
          acceleration_short = as2;
          acceleration_max_short = ams2;
        } ) ->
    Float.equal sv1 sv2 && Float.equal oor1 oor2 && Float.equal ail1 ail2
    && Float.equal al1 al2 && Float.equal aml1 aml2 && Float.equal ais1 ais2
    && Float.equal as1 as2 && Float.equal ams1 ams2
  | Sin (), Sin () -> true
  | Sinh (), Sinh () -> true
  | Sma { timeperiod = t1 }, Sma { timeperiod = t2 } -> t1 = t2
  | Sqrt (), Sqrt () -> true
  | ( Stddev { timeperiod = t1; nb_dev = nd1 },
      Stddev { timeperiod = t2; nb_dev = nd2 } ) ->
    t1 = t2 && Float.equal nd1 nd2
  | ( Stoch
        {
          fast_k_period = fkp1;
          slow_k_period = skp1;
          slow_k_ma_type = skmt1;
          slow_d_period = sdp1;
          slow_d_ma_type = sdmt1;
        },
      Stoch
        {
          fast_k_period = fkp2;
          slow_k_period = skp2;
          slow_k_ma_type = skmt2;
          slow_d_period = sdp2;
          slow_d_ma_type = sdmt2;
        } ) ->
    fkp1 = fkp2 && skp1 = skp2 && Stdlib.( = ) skmt1 skmt2 && sdp1 = sdp2
    && Stdlib.( = ) sdmt1 sdmt2
  | ( Stochf
        { fast_k_period = fkp1; fast_d_period = fdp1; fast_d_ma_type = fdmt1 },
      Stochf
        { fast_k_period = fkp2; fast_d_period = fdp2; fast_d_ma_type = fdmt2 } )
    ->
    fkp1 = fkp2 && fdp1 = fdp2 && Stdlib.( = ) fdmt1 fdmt2
  | ( Stochrsi
        {
          timeperiod = t1;
          fast_k_period = fkp1;
          fast_d_period = fdp1;
          fast_d_ma_type = fdmt1;
        },
      Stochrsi
        {
          timeperiod = t2;
          fast_k_period = fkp2;
          fast_d_period = fdp2;
          fast_d_ma_type = fdmt2;
        } ) ->
    t1 = t2 && fkp1 = fkp2 && fdp1 = fdp2 && Stdlib.( = ) fdmt1 fdmt2
  | Sub (), Sub () -> true
  | Sum { timeperiod = t1 }, Sum { timeperiod = t2 } -> t1 = t2
  | ( T3 { timeperiod = t1; v_factor = vf1 },
      T3 { timeperiod = t2; v_factor = vf2 } ) ->
    t1 = t2 && Float.equal vf1 vf2
  | Tan (), Tan () -> true
  | Tanh (), Tanh () -> true
  | Tema { timeperiod = t1 }, Tema { timeperiod = t2 } -> t1 = t2
  | Trange (), Trange () -> true
  | Trima { timeperiod = t1 }, Trima { timeperiod = t2 } -> t1 = t2
  | Trix { timeperiod = t1 }, Trix { timeperiod = t2 } -> t1 = t2
  | Tsf { timeperiod = t1 }, Tsf { timeperiod = t2 } -> t1 = t2
  | Typprice (), Typprice () -> true
  | ( Ultosc { timeperiod1 = t1_1; timeperiod2 = t2_1; timeperiod3 = t3_1 },
      Ultosc { timeperiod1 = t1_2; timeperiod2 = t2_2; timeperiod3 = t3_2 } ) ->
    t1_1 = t1_2 && t2_1 = t2_2 && t3_1 = t3_2
  | Var { timeperiod = t1; nb_dev = nd1 }, Var { timeperiod = t2; nb_dev = nd2 }
    ->
    t1 = t2 && Float.equal nd1 nd2
  | Wclprice (), Wclprice () -> true
  | Willr { timeperiod = t1 }, Willr { timeperiod = t2 } -> t1 = t2
  | Wma { timeperiod = t1 }, Wma { timeperiod = t2 } -> t1 = t2
  | _ -> false

(* Specialization functions for immediate type resolution *)
let accbands ~timeperiod : (Ohlcv.t, float_ba * float_ba * float_ba) t =
  Accbands { timeperiod }

let acos () : (float_ba, float_ba) t = Acos ()
let ad () : (Ohlcv.t, float_ba) t = Ad ()
let add () : (float_ba * float_ba, float_ba) t = Add ()

let adosc ~fast_period ~slow_period : (Ohlcv.t, float_ba) t =
  Adosc { fast_period; slow_period }

let adx ~timeperiod : (Ohlcv.t, float_ba) t = Adx { timeperiod }
let adxr ~timeperiod : (Ohlcv.t, float_ba) t = Adxr { timeperiod }

let apo ~fast_period ~slow_period ~ma_type : (Ohlcv.t, float_ba) t =
  Apo { fast_period; slow_period; ma_type }

let aroon ~timeperiod : (Ohlcv.t, float_ba * float_ba) t = Aroon { timeperiod }
let aroonosc ~timeperiod : (Ohlcv.t, float_ba) t = Aroonosc { timeperiod }
let asin () : (float_ba, float_ba) t = Asin ()
let atan () : (float_ba, float_ba) t = Atan ()
let atr ~timeperiod : (Ohlcv.t, float_ba) t = Atr { timeperiod }
let avgprice () : (Ohlcv.t, float_ba) t = Avgprice ()
let avgdev ~timeperiod : (Ohlcv.t, float_ba) t = Avgdev { timeperiod }

let bbands ~timeperiod ~nb_dev_up ~nb_dev_dn ~ma_type :
    (Ohlcv.t, float_ba * float_ba * float_ba) t =
  Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type }

let beta ~timeperiod : (float_ba * float_ba, float_ba) t = Beta { timeperiod }
let bop () : (Ohlcv.t, float_ba) t = Bop ()
let cci ~timeperiod : (Ohlcv.t, float_ba) t = Cci { timeperiod }
let cdl2crows () : (Ohlcv.t, int_ba) t = Cdl2crows ()
let cdl3blackcrows () : (Ohlcv.t, int_ba) t = Cdl3blackcrows ()
let cdl3inside () : (Ohlcv.t, int_ba) t = Cdl3inside ()
let cdl3linestrike () : (Ohlcv.t, int_ba) t = Cdl3linestrike ()
let cdl3outside () : (Ohlcv.t, int_ba) t = Cdl3outside ()
let cdl3starsinsouth () : (Ohlcv.t, int_ba) t = Cdl3starsinsouth ()
let cdl3whitesoldiers () : (Ohlcv.t, int_ba) t = Cdl3whitesoldiers ()

let cdlabandonedbaby ~penetration : (Ohlcv.t, int_ba) t =
  Cdlabandonedbaby { penetration }

let cdladvanceblock () : (Ohlcv.t, int_ba) t = Cdladvanceblock ()
let cdlbelthold () : (Ohlcv.t, int_ba) t = Cdlbelthold ()
let cdlbreakaway () : (Ohlcv.t, int_ba) t = Cdlbreakaway ()
let cdlclosingmarubozu () : (Ohlcv.t, int_ba) t = Cdlclosingmarubozu ()
let cdlconcealbabyswall () : (Ohlcv.t, int_ba) t = Cdlconcealbabyswall ()
let cdlcounterattack () : (Ohlcv.t, int_ba) t = Cdlcounterattack ()

let cdldarkcloudcover ~penetration : (Ohlcv.t, int_ba) t =
  Cdldarkcloudcover { penetration }

let cdldoji () : (Ohlcv.t, int_ba) t = Cdldoji ()
let cdldojistar () : (Ohlcv.t, int_ba) t = Cdldojistar ()
let cdldragonflydoji () : (Ohlcv.t, int_ba) t = Cdldragonflydoji ()
let cdlengulfing () : (Ohlcv.t, int_ba) t = Cdlengulfing ()

let cdleveningdojistar ~penetration : (Ohlcv.t, int_ba) t =
  Cdleveningdojistar { penetration }

let cdleveningstar ~penetration : (Ohlcv.t, int_ba) t =
  Cdleveningstar { penetration }

let cdlgapsidesidewhite () : (Ohlcv.t, int_ba) t = Cdlgapsidesidewhite ()
let cdlgravestonedoji () : (Ohlcv.t, int_ba) t = Cdlgravestonedoji ()
let cdlhammer () : (Ohlcv.t, int_ba) t = Cdlhammer ()
let cdlhangingman () : (Ohlcv.t, int_ba) t = Cdlhangingman ()
let cdlharami () : (Ohlcv.t, int_ba) t = Cdlharami ()
let cdlharamicross () : (Ohlcv.t, int_ba) t = Cdlharamicross ()
let cdlhighwave () : (Ohlcv.t, int_ba) t = Cdlhighwave ()
let cdlhikkake () : (Ohlcv.t, int_ba) t = Cdlhikkake ()
let cdlhikkakemod () : (Ohlcv.t, int_ba) t = Cdlhikkakemod ()
let cdlhomingpigeon () : (Ohlcv.t, int_ba) t = Cdlhomingpigeon ()
let cdlidentical3crows () : (Ohlcv.t, int_ba) t = Cdlidentical3crows ()
let cdlinneck () : (Ohlcv.t, int_ba) t = Cdlinneck ()
let cdlinvertedhammer () : (Ohlcv.t, int_ba) t = Cdlinvertedhammer ()
let cdlkicking () : (Ohlcv.t, int_ba) t = Cdlkicking ()
let cdlkickingbylength () : (Ohlcv.t, int_ba) t = Cdlkickingbylength ()
let cdlladderbottom () : (Ohlcv.t, int_ba) t = Cdlladderbottom ()
let cdllongleggeddoji () : (Ohlcv.t, int_ba) t = Cdllongleggeddoji ()
let cdllongline () : (Ohlcv.t, int_ba) t = Cdllongline ()
let cdlmarubozu () : (Ohlcv.t, int_ba) t = Cdlmarubozu ()
let cdlmatchinglow () : (Ohlcv.t, int_ba) t = Cdlmatchinglow ()
let cdlmathold ~penetration : (Ohlcv.t, int_ba) t = Cdlmathold { penetration }

let cdlmorningdojistar ~penetration : (Ohlcv.t, int_ba) t =
  Cdlmorningdojistar { penetration }

let cdlmorningstar ~penetration : (Ohlcv.t, int_ba) t =
  Cdlmorningstar { penetration }

let cdlonneck () : (Ohlcv.t, int_ba) t = Cdlonneck ()
let cdlpiercing () : (Ohlcv.t, int_ba) t = Cdlpiercing ()
let cdlrickshawman () : (Ohlcv.t, int_ba) t = Cdlrickshawman ()
let cdlrisefall3methods () : (Ohlcv.t, int_ba) t = Cdlrisefall3methods ()
let cdlseparatinglines () : (Ohlcv.t, int_ba) t = Cdlseparatinglines ()
let cdlshootingstar () : (Ohlcv.t, int_ba) t = Cdlshootingstar ()
let cdlshortline () : (Ohlcv.t, int_ba) t = Cdlshortline ()
let cdlspinningtop () : (Ohlcv.t, int_ba) t = Cdlspinningtop ()
let cdlstalledpattern () : (Ohlcv.t, int_ba) t = Cdlstalledpattern ()
let cdlsticksandwich () : (Ohlcv.t, int_ba) t = Cdlsticksandwich ()
let cdltakuri () : (Ohlcv.t, int_ba) t = Cdltakuri ()
let cdltasukigap () : (Ohlcv.t, int_ba) t = Cdltasukigap ()
let cdlthrusting () : (Ohlcv.t, int_ba) t = Cdlthrusting ()
let cdltristar () : (Ohlcv.t, int_ba) t = Cdltristar ()
let cdlunique3river () : (Ohlcv.t, int_ba) t = Cdlunique3river ()
let cdlupsidegap2crows () : (Ohlcv.t, int_ba) t = Cdlupsidegap2crows ()
let cdlxsidegap3methods () : (Ohlcv.t, int_ba) t = Cdlxsidegap3methods ()
let ceil () : (float_ba, float_ba) t = Ceil ()
let cmo ~timeperiod : (Ohlcv.t, float_ba) t = Cmo { timeperiod }

let correl ~timeperiod : (float_ba * float_ba, float_ba) t =
  Correl { timeperiod }

let cos () : (float_ba, float_ba) t = Cos ()
let cosh () : (float_ba, float_ba) t = Cosh ()
let dema ~timeperiod : (Ohlcv.t, float_ba) t = Dema { timeperiod }
let div () : (float_ba * float_ba, float_ba) t = Div ()
let dx ~timeperiod : (Ohlcv.t, float_ba) t = Dx { timeperiod }
let ema ~timeperiod : (Ohlcv.t, float_ba) t = Ema { timeperiod }
let exp () : (float_ba, float_ba) t = Exp ()
let floor () : (float_ba, float_ba) t = Floor ()
let ht_dcperiod () : (Ohlcv.t, float_ba) t = Ht_dcperiod ()
let ht_dcphase () : (Ohlcv.t, float_ba) t = Ht_dcphase ()
let ht_phasor () : (Ohlcv.t, float_ba * float_ba) t = Ht_phasor ()
let ht_sine () : (Ohlcv.t, float_ba * float_ba) t = Ht_sine ()
let ht_trendline () : (Ohlcv.t, float_ba) t = Ht_trendline ()
let ht_trendmode () : (Ohlcv.t, int_ba) t = Ht_trendmode ()
let imi ~timeperiod : (Ohlcv.t, float_ba) t = Imi { timeperiod }
let kama ~timeperiod : (Ohlcv.t, float_ba) t = Kama { timeperiod }
let linearreg ~timeperiod : (Ohlcv.t, float_ba) t = Linearreg { timeperiod }

let linearreg_angle ~timeperiod : (Ohlcv.t, float_ba) t =
  Linearreg_angle { timeperiod }

let linearreg_intercept ~timeperiod : (Ohlcv.t, float_ba) t =
  Linearreg_intercept { timeperiod }

let linearreg_slope ~timeperiod : (Ohlcv.t, float_ba) t =
  Linearreg_slope { timeperiod }

let ln () : (float_ba, float_ba) t = Ln ()
let log10 () : (float_ba, float_ba) t = Log10 ()
let ma ~timeperiod ~ma_type : (Ohlcv.t, float_ba) t = Ma { timeperiod; ma_type }

let macd ~fast_period ~slow_period ~signal_period :
    (Ohlcv.t, float_ba * float_ba * float_ba) t =
  Macd { fast_period; slow_period; signal_period }

let macdext ~fast_period ~fast_ma_type ~slow_period ~slow_ma_type ~signal_period
    ~signal_ma_type : (Ohlcv.t, float_ba * float_ba * float_ba) t =
  Macdext
    {
      fast_period;
      fast_ma_type;
      slow_period;
      slow_ma_type;
      signal_period;
      signal_ma_type;
    }

let macdfix ~signal_period : (Ohlcv.t, float_ba * float_ba * float_ba) t =
  Macdfix { signal_period }

let mama ~fast_limit ~slow_limit : (Ohlcv.t, float_ba * float_ba) t =
  Mama { fast_limit; slow_limit }

let mavp ~min_period ~max_period ~ma_type : (float_ba * float_ba, float_ba) t =
  Mavp { min_period; max_period; ma_type }

let max ~timeperiod : (float_ba, float_ba) t = Max { timeperiod }
let maxindex ~timeperiod : (float_ba, int_ba) t = Maxindex { timeperiod }
let medprice () : (Ohlcv.t, float_ba) t = Medprice ()
let mfi ~timeperiod : (Ohlcv.t, float_ba) t = Mfi { timeperiod }
let midpoint ~timeperiod : (float_ba, float_ba) t = Midpoint { timeperiod }
let midprice ~timeperiod : (Ohlcv.t, float_ba) t = Midprice { timeperiod }
let min ~timeperiod : (float_ba, float_ba) t = Min { timeperiod }
let minindex ~timeperiod : (float_ba, int_ba) t = Minindex { timeperiod }

let minmax ~timeperiod : (float_ba, float_ba * float_ba) t =
  Minmax { timeperiod }

let minmaxindex ~timeperiod : (float_ba, int_ba * int_ba) t =
  Minmaxindex { timeperiod }

let minus_di ~timeperiod : (Ohlcv.t, float_ba) t = Minus_di { timeperiod }
let minus_dm ~timeperiod : (Ohlcv.t, float_ba) t = Minus_dm { timeperiod }
let mom ~timeperiod : (Ohlcv.t, float_ba) t = Mom { timeperiod }
let mult () : (float_ba * float_ba, float_ba) t = Mult ()
let natr ~timeperiod : (Ohlcv.t, float_ba) t = Natr { timeperiod }
let obv () : (Ohlcv.t, float_ba) t = Obv ()
let plus_di ~timeperiod : (Ohlcv.t, float_ba) t = Plus_di { timeperiod }
let plus_dm ~timeperiod : (Ohlcv.t, float_ba) t = Plus_dm { timeperiod }

let ppo ~fast_period ~slow_period ~ma_type : (Ohlcv.t, float_ba) t =
  Ppo { fast_period; slow_period; ma_type }

let roc ~timeperiod : (Ohlcv.t, float_ba) t = Roc { timeperiod }
let rocp ~timeperiod : (Ohlcv.t, float_ba) t = Rocp { timeperiod }
let rocr ~timeperiod : (Ohlcv.t, float_ba) t = Rocr { timeperiod }
let rocr100 ~timeperiod : (Ohlcv.t, float_ba) t = Rocr100 { timeperiod }
let rsi ~timeperiod : (Ohlcv.t, float_ba) t = Rsi { timeperiod }

let sar ~acceleration ~maximum : (Ohlcv.t, float_ba) t =
  Sar { acceleration; maximum }

let sarext ~start_value ~offset_on_reverse ~acceleration_init_long
    ~acceleration_long ~acceleration_max_long ~acceleration_init_short
    ~acceleration_short ~acceleration_max_short : (Ohlcv.t, float_ba) t =
  Sarext
    {
      start_value;
      offset_on_reverse;
      acceleration_init_long;
      acceleration_long;
      acceleration_max_long;
      acceleration_init_short;
      acceleration_short;
      acceleration_max_short;
    }

let sin () : (float_ba, float_ba) t = Sin ()
let sinh () : (float_ba, float_ba) t = Sinh ()
let sma ~timeperiod : (Ohlcv.t, float_ba) t = Sma { timeperiod }
let sqrt () : (float_ba, float_ba) t = Sqrt ()

let stddev ~timeperiod ~nb_dev : (float_ba, float_ba) t =
  Stddev { timeperiod; nb_dev }

let stoch ~fast_k_period ~slow_k_period ~slow_k_ma_type ~slow_d_period
    ~slow_d_ma_type : (Ohlcv.t, float_ba * float_ba) t =
  Stoch
    {
      fast_k_period;
      slow_k_period;
      slow_k_ma_type;
      slow_d_period;
      slow_d_ma_type;
    }

let stochf ~fast_k_period ~fast_d_period ~fast_d_ma_type :
    (Ohlcv.t, float_ba * float_ba) t =
  Stochf { fast_k_period; fast_d_period; fast_d_ma_type }

let stochrsi ~timeperiod ~fast_k_period ~fast_d_period ~fast_d_ma_type :
    (Ohlcv.t, float_ba * float_ba) t =
  Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }

let sub () : (float_ba * float_ba, float_ba) t = Sub ()
let sum ~timeperiod : (Ohlcv.t, float_ba) t = Sum { timeperiod }

let t3 ~timeperiod ~v_factor : (Ohlcv.t, float_ba) t =
  T3 { timeperiod; v_factor }

let tan () : (float_ba, float_ba) t = Tan ()
let tanh () : (float_ba, float_ba) t = Tanh ()
let tema ~timeperiod : (Ohlcv.t, float_ba) t = Tema { timeperiod }
let trange () : (Ohlcv.t, float_ba) t = Trange ()
let trima ~timeperiod : (Ohlcv.t, float_ba) t = Trima { timeperiod }
let trix ~timeperiod : (Ohlcv.t, float_ba) t = Trix { timeperiod }
let tsf ~timeperiod : (Ohlcv.t, float_ba) t = Tsf { timeperiod }
let typprice () : (Ohlcv.t, float_ba) t = Typprice ()

let ultosc ~timeperiod1 ~timeperiod2 ~timeperiod3 : (Ohlcv.t, float_ba) t =
  Ultosc { timeperiod1; timeperiod2; timeperiod3 }

let var ~timeperiod ~nb_dev : (Ohlcv.t, float_ba) t = Var { timeperiod; nb_dev }
let wclprice () : (Ohlcv.t, float_ba) t = Wclprice ()
let willr ~timeperiod : (Ohlcv.t, float_ba) t = Willr { timeperiod }
let wma ~timeperiod : (Ohlcv.t, float_ba) t = Wma { timeperiod }

let lookback : type a b. (a, b) t -> int =
 fun params ->
  match params with
  | Accbands { timeperiod } -> F.Lookback.accbands timeperiod
  | Acos () -> F.Lookback.acos ()
  | Ad () -> F.Lookback.ad ()
  | Add () -> F.Lookback.add ()
  | Adosc { fast_period; slow_period } ->
    F.Lookback.adosc fast_period slow_period
  | Adx { timeperiod } -> F.Lookback.adx timeperiod
  | Adxr { timeperiod } -> F.Lookback.adxr timeperiod
  | Apo { fast_period; slow_period; ma_type } ->
    F.Lookback.apo fast_period slow_period (Ma_type.to_int ma_type)
  | Aroon { timeperiod } -> F.Lookback.aroon timeperiod
  | Aroonosc { timeperiod } -> F.Lookback.aroonosc timeperiod
  | Asin () -> F.Lookback.asin ()
  | Atan () -> F.Lookback.atan ()
  | Atr { timeperiod } -> F.Lookback.atr timeperiod
  | Avgprice () -> F.Lookback.avgprice ()
  | Avgdev { timeperiod } -> F.Lookback.avgdev timeperiod
  | Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
    F.Lookback.bbands timeperiod nb_dev_up nb_dev_dn (Ma_type.to_int ma_type)
  | Beta { timeperiod } -> F.Lookback.beta timeperiod
  | Bop () -> F.Lookback.bop ()
  | Cci { timeperiod } -> F.Lookback.cci timeperiod
  | Cdl2crows () -> F.Lookback.cdl2crows ()
  | Cdl3blackcrows () -> F.Lookback.cdl3blackcrows ()
  | Cdl3inside () -> F.Lookback.cdl3inside ()
  | Cdl3linestrike () -> F.Lookback.cdl3linestrike ()
  | Cdl3outside () -> F.Lookback.cdl3outside ()
  | Cdl3starsinsouth () -> F.Lookback.cdl3starsinsouth ()
  | Cdl3whitesoldiers () -> F.Lookback.cdl3whitesoldiers ()
  | Cdlabandonedbaby { penetration } -> F.Lookback.cdlabandonedbaby penetration
  | Cdladvanceblock () -> F.Lookback.cdladvanceblock ()
  | Cdlbelthold () -> F.Lookback.cdlbelthold ()
  | Cdlbreakaway () -> F.Lookback.cdlbreakaway ()
  | Cdlclosingmarubozu () -> F.Lookback.cdlclosingmarubozu ()
  | Cdlconcealbabyswall () -> F.Lookback.cdlconcealbabyswall ()
  | Cdlcounterattack () -> F.Lookback.cdlcounterattack ()
  | Cdldarkcloudcover { penetration } ->
    F.Lookback.cdldarkcloudcover penetration
  | Cdldoji () -> F.Lookback.cdldoji ()
  | Cdldojistar () -> F.Lookback.cdldojistar ()
  | Cdldragonflydoji () -> F.Lookback.cdldragonflydoji ()
  | Cdlengulfing () -> F.Lookback.cdlengulfing ()
  | Cdleveningdojistar { penetration } ->
    F.Lookback.cdleveningdojistar penetration
  | Cdleveningstar { penetration } -> F.Lookback.cdleveningstar penetration
  | Cdlgapsidesidewhite () -> F.Lookback.cdlgapsidesidewhite ()
  | Cdlgravestonedoji () -> F.Lookback.cdlgravestonedoji ()
  | Cdlhammer () -> F.Lookback.cdlhammer ()
  | Cdlhangingman () -> F.Lookback.cdlhangingman ()
  | Cdlharami () -> F.Lookback.cdlharami ()
  | Cdlharamicross () -> F.Lookback.cdlharamicross ()
  | Cdlhighwave () -> F.Lookback.cdlhighwave ()
  | Cdlhikkake () -> F.Lookback.cdlhikkake ()
  | Cdlhikkakemod () -> F.Lookback.cdlhikkakemod ()
  | Cdlhomingpigeon () -> F.Lookback.cdlhomingpigeon ()
  | Cdlidentical3crows () -> F.Lookback.cdlidentical3crows ()
  | Cdlinneck () -> F.Lookback.cdlinneck ()
  | Cdlinvertedhammer () -> F.Lookback.cdlinvertedhammer ()
  | Cdlkicking () -> F.Lookback.cdlkicking ()
  | Cdlkickingbylength () -> F.Lookback.cdlkickingbylength ()
  | Cdlladderbottom () -> F.Lookback.cdlladderbottom ()
  | Cdllongleggeddoji () -> F.Lookback.cdllongleggeddoji ()
  | Cdllongline () -> F.Lookback.cdllongline ()
  | Cdlmarubozu () -> F.Lookback.cdlmarubozu ()
  | Cdlmatchinglow () -> F.Lookback.cdlmatchinglow ()
  | Cdlmathold { penetration } -> F.Lookback.cdlmathold penetration
  | Cdlmorningdojistar { penetration } ->
    F.Lookback.cdlmorningdojistar penetration
  | Cdlmorningstar { penetration } -> F.Lookback.cdlmorningstar penetration
  | Cdlonneck () -> F.Lookback.cdlonneck ()
  | Cdlpiercing () -> F.Lookback.cdlpiercing ()
  | Cdlrickshawman () -> F.Lookback.cdlrickshawman ()
  | Cdlrisefall3methods () -> F.Lookback.cdlrisefall3methods ()
  | Cdlseparatinglines () -> F.Lookback.cdlseparatinglines ()
  | Cdlshootingstar () -> F.Lookback.cdlshootingstar ()
  | Cdlshortline () -> F.Lookback.cdlshortline ()
  | Cdlspinningtop () -> F.Lookback.cdlspinningtop ()
  | Cdlstalledpattern () -> F.Lookback.cdlstalledpattern ()
  | Cdlsticksandwich () -> F.Lookback.cdlsticksandwich ()
  | Cdltakuri () -> F.Lookback.cdltakuri ()
  | Cdltasukigap () -> F.Lookback.cdltasukigap ()
  | Cdlthrusting () -> F.Lookback.cdlthrusting ()
  | Cdltristar () -> F.Lookback.cdltristar ()
  | Cdlunique3river () -> F.Lookback.cdlunique3river ()
  | Cdlupsidegap2crows () -> F.Lookback.cdlupsidegap2crows ()
  | Cdlxsidegap3methods () -> F.Lookback.cdlxsidegap3methods ()
  | Ceil () -> F.Lookback.ceil ()
  | Cmo { timeperiod } -> F.Lookback.cmo timeperiod
  | Correl { timeperiod } -> F.Lookback.correl timeperiod
  | Cos () -> F.Lookback.cos ()
  | Cosh () -> F.Lookback.cosh ()
  | Dema { timeperiod } -> F.Lookback.dema timeperiod
  | Div () -> F.Lookback.div ()
  | Dx { timeperiod } -> F.Lookback.dx timeperiod
  | Ema { timeperiod } -> F.Lookback.ema timeperiod
  | Exp () -> F.Lookback.exp ()
  | Floor () -> F.Lookback.floor ()
  | Ht_dcperiod () -> F.Lookback.ht_dcperiod ()
  | Ht_dcphase () -> F.Lookback.ht_dcphase ()
  | Ht_phasor () -> F.Lookback.ht_phasor ()
  | Ht_sine () -> F.Lookback.ht_sine ()
  | Ht_trendline () -> F.Lookback.ht_trendline ()
  | Ht_trendmode () -> F.Lookback.ht_trendmode ()
  | Imi { timeperiod } -> F.Lookback.imi timeperiod
  | Kama { timeperiod } -> F.Lookback.kama timeperiod
  | Linearreg { timeperiod } -> F.Lookback.linearreg timeperiod
  | Linearreg_angle { timeperiod } -> F.Lookback.linearreg_angle timeperiod
  | Linearreg_intercept { timeperiod } ->
    F.Lookback.linearreg_intercept timeperiod
  | Linearreg_slope { timeperiod } -> F.Lookback.linearreg_slope timeperiod
  | Ln () -> F.Lookback.ln ()
  | Log10 () -> F.Lookback.log10 ()
  | Ma { timeperiod; ma_type } ->
    F.Lookback.ma timeperiod (Ma_type.to_int ma_type)
  | Macd { fast_period; slow_period; signal_period } ->
    F.Lookback.macd fast_period slow_period signal_period
  | Macdext
      {
        fast_period;
        fast_ma_type;
        slow_period;
        slow_ma_type;
        signal_period;
        signal_ma_type;
      } ->
    F.Lookback.macdext fast_period
      (Ma_type.to_int fast_ma_type)
      slow_period
      (Ma_type.to_int slow_ma_type)
      signal_period
      (Ma_type.to_int signal_ma_type)
  | Macdfix { signal_period } -> F.Lookback.macdfix signal_period
  | Mama { fast_limit; slow_limit } -> F.Lookback.mama fast_limit slow_limit
  | Mavp { min_period; max_period; ma_type } ->
    F.Lookback.mavp min_period max_period (Ma_type.to_int ma_type)
  | Max { timeperiod } -> F.Lookback.max timeperiod
  | Maxindex { timeperiod } -> F.Lookback.maxindex timeperiod
  | Medprice () -> F.Lookback.medprice ()
  | Mfi { timeperiod } -> F.Lookback.mfi timeperiod
  | Midpoint { timeperiod } -> F.Lookback.midpoint timeperiod
  | Midprice { timeperiod } -> F.Lookback.midprice timeperiod
  | Min { timeperiod } -> F.Lookback.min timeperiod
  | Minindex { timeperiod } -> F.Lookback.minindex timeperiod
  | Minmax { timeperiod } -> F.Lookback.minmax timeperiod
  | Minmaxindex { timeperiod } -> F.Lookback.minmaxindex timeperiod
  | Minus_di { timeperiod } -> F.Lookback.minus_di timeperiod
  | Minus_dm { timeperiod } -> F.Lookback.minus_dm timeperiod
  | Mom { timeperiod } -> F.Lookback.mom timeperiod
  | Mult () -> F.Lookback.mult ()
  | Natr { timeperiod } -> F.Lookback.natr timeperiod
  | Obv () -> F.Lookback.obv ()
  | Plus_di { timeperiod } -> F.Lookback.plus_di timeperiod
  | Plus_dm { timeperiod } -> F.Lookback.plus_dm timeperiod
  | Ppo { fast_period; slow_period; ma_type } ->
    F.Lookback.ppo fast_period slow_period (Ma_type.to_int ma_type)
  | Roc { timeperiod } -> F.Lookback.roc timeperiod
  | Rocp { timeperiod } -> F.Lookback.rocp timeperiod
  | Rocr { timeperiod } -> F.Lookback.rocr timeperiod
  | Rocr100 { timeperiod } -> F.Lookback.rocr100 timeperiod
  | Rsi { timeperiod } -> F.Lookback.rsi timeperiod
  | Sar { acceleration; maximum } -> F.Lookback.sar acceleration maximum
  | Sarext
      {
        start_value;
        offset_on_reverse;
        acceleration_init_long;
        acceleration_long;
        acceleration_max_long;
        acceleration_init_short;
        acceleration_short;
        acceleration_max_short;
      } ->
    F.Lookback.sarext start_value offset_on_reverse acceleration_init_long
      acceleration_long acceleration_max_long acceleration_init_short
      acceleration_short acceleration_max_short
  | Sin () -> F.Lookback.sin ()
  | Sinh () -> F.Lookback.sinh ()
  | Sma { timeperiod } -> F.Lookback.sma timeperiod
  | Sqrt () -> F.Lookback.sqrt ()
  | Stddev { timeperiod; nb_dev } -> F.Lookback.stddev timeperiod nb_dev
  | Stoch
      {
        fast_k_period;
        slow_k_period;
        slow_k_ma_type;
        slow_d_period;
        slow_d_ma_type;
      } ->
    F.Lookback.stoch fast_k_period slow_k_period
      (Ma_type.to_int slow_k_ma_type)
      slow_d_period
      (Ma_type.to_int slow_d_ma_type)
  | Stochf { fast_k_period; fast_d_period; fast_d_ma_type } ->
    F.Lookback.stochf fast_k_period fast_d_period
      (Ma_type.to_int fast_d_ma_type)
  | Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
    F.Lookback.stochrsi timeperiod fast_k_period fast_d_period
      (Ma_type.to_int fast_d_ma_type)
  | Sub () -> F.Lookback.sub ()
  | Sum { timeperiod } -> F.Lookback.sum timeperiod
  | T3 { timeperiod; v_factor } -> F.Lookback.t3 timeperiod v_factor
  | Tan () -> F.Lookback.tan ()
  | Tanh () -> F.Lookback.tanh ()
  | Tema { timeperiod } -> F.Lookback.tema timeperiod
  | Trange () -> F.Lookback.trange ()
  | Trima { timeperiod } -> F.Lookback.trima timeperiod
  | Trix { timeperiod } -> F.Lookback.trix timeperiod
  | Tsf { timeperiod } -> F.Lookback.tsf timeperiod
  | Typprice () -> F.Lookback.typprice ()
  | Ultosc { timeperiod1; timeperiod2; timeperiod3 } ->
    F.Lookback.ultosc timeperiod1 timeperiod2 timeperiod3
  | Var { timeperiod; nb_dev } -> F.Lookback.var timeperiod nb_dev
  | Wclprice () -> F.Lookback.wclprice ()
  | Willr { timeperiod } -> F.Lookback.willr timeperiod
  | Wma { timeperiod } -> F.Lookback.wma timeperiod
