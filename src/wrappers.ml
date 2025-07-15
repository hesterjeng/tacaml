module F = C.Functions
module C = Ctypes
module T = Type_description.Types

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

let pp = fun fmt x -> Format.fprintf fmt "@[%s@]" (to_string x)

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
