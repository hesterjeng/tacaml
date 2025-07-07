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

let ba = C.bigarray_start C.array1

let iba :
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int Ctypes.ptr =
  C.bigarray_start C.array1

type float_ba =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type int_ba = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

type ('a, 'b) indicator_params =
  | Accbands : {
      timeperiod : int;
    }
      -> (Ohlcv.t, float_ba * float_ba * float_ba) indicator_params
  | Acos : unit -> (float_ba, float_ba) indicator_params
  | Ad : unit -> (Ohlcv.t, float_ba) indicator_params
  | Add : unit -> (float_ba * float_ba, float_ba) indicator_params
  | Adosc : {
      fast_period : int;
      slow_period : int;
    }
      -> (Ohlcv.t, float_ba) indicator_params
  | Adx : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Adxr : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Apo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba) indicator_params
  | Aroon : {
      timeperiod : int;
    }
      -> (Ohlcv.t, float_ba * float_ba) indicator_params
  | Aroonosc : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Asin : unit -> (float_ba, float_ba) indicator_params
  | Atan : unit -> (float_ba, float_ba) indicator_params
  | Atr : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Avgprice : unit -> (Ohlcv.t, float_ba) indicator_params
  | Avgdev : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Bbands : {
      timeperiod : int;
      nb_dev_up : float;
      nb_dev_dn : float;
      ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba * float_ba * float_ba) indicator_params
  | Beta : {
      timeperiod : int;
    }
      -> (float_ba * float_ba, float_ba) indicator_params
  | Bop : unit -> (Ohlcv.t, float_ba) indicator_params
  | Cci : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Cdl2crows : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3blackcrows : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3inside : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3linestrike : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3outside : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3starsinsouth : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdl3whitesoldiers : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlabandonedbaby : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdladvanceblock : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlbelthold : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlbreakaway : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlclosingmarubozu : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlconcealbabyswall : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlcounterattack : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdldarkcloudcover : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdldoji : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdldojistar : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdldragonflydoji : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlengulfing : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdleveningdojistar : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdleveningstar : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdlgapsidesidewhite : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlgravestonedoji : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhammer : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhangingman : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlharami : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlharamicross : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhighwave : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhikkake : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhikkakemod : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlhomingpigeon : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlidentical3crows : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlinneck : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlinvertedhammer : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlkicking : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlkickingbylength : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlladderbottom : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdllongleggeddoji : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdllongline : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlmarubozu : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlmatchinglow : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlmathold : { penetration : float } -> (Ohlcv.t, int_ba) indicator_params
  | Cdlmorningdojistar : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdlmorningstar : {
      penetration : float;
    }
      -> (Ohlcv.t, int_ba) indicator_params
  | Cdlonneck : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlpiercing : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlrickshawman : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlrisefall3methods : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlseparatinglines : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlshootingstar : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlshortline : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlspinningtop : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlstalledpattern : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlsticksandwich : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdltakuri : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdltasukigap : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlthrusting : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdltristar : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlunique3river : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlupsidegap2crows : unit -> (Ohlcv.t, int_ba) indicator_params
  | Cdlxsidegap3methods : unit -> (Ohlcv.t, int_ba) indicator_params
  | Ceil : unit -> (float_ba, float_ba) indicator_params
  | Cmo : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Correl : {
      timeperiod : int;
    }
      -> (float_ba * float_ba, float_ba) indicator_params
  | Cos : unit -> (float_ba, float_ba) indicator_params
  | Cosh : unit -> (float_ba, float_ba) indicator_params
  | Dema : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Div : unit -> (float_ba * float_ba, float_ba) indicator_params
  | Dx : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Ema : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Exp : unit -> (float_ba, float_ba) indicator_params
  | Floor : unit -> (float_ba, float_ba) indicator_params
  | Ht_dcperiod : unit -> (float_ba, float_ba) indicator_params
  | Ht_dcphase : unit -> (float_ba, float_ba) indicator_params
  | Ht_phasor : unit -> (float_ba, float_ba * float_ba) indicator_params
  | Ht_sine : unit -> (float_ba, float_ba * float_ba) indicator_params
  | Ht_trendline : unit -> (float_ba, float_ba) indicator_params
  | Ht_trendmode : unit -> (float_ba, int_ba) indicator_params
  | Imi : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Kama : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Linearreg : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Linearreg_angle : {
      timeperiod : int;
    }
      -> (float_ba, float_ba) indicator_params
  | Linearreg_intercept : {
      timeperiod : int;
    }
      -> (float_ba, float_ba) indicator_params
  | Linearreg_slope : {
      timeperiod : int;
    }
      -> (float_ba, float_ba) indicator_params
  | Ln : unit -> (float_ba, float_ba) indicator_params
  | Log10 : unit -> (float_ba, float_ba) indicator_params
  | Ma : {
      timeperiod : int;
      ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba) indicator_params
  | Macd : {
      fast_period : int;
      slow_period : int;
      signal_period : int;
    }
      -> (float_ba, float_ba * float_ba * float_ba) indicator_params
  | Macdext : {
      fast_period : int;
      fast_ma_type : Ma_type.t;
      slow_period : int;
      slow_ma_type : Ma_type.t;
      signal_period : int;
      signal_ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba * float_ba * float_ba) indicator_params
  | Macdfix : {
      signal_period : int;
    }
      -> (float_ba, float_ba * float_ba * float_ba) indicator_params
  | Mama : {
      fast_limit : float;
      slow_limit : float;
    }
      -> (float_ba, float_ba * float_ba) indicator_params
  | Mavp : {
      min_period : int;
      max_period : int;
      ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba, float_ba) indicator_params
  | Max : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Maxindex : { timeperiod : int } -> (float_ba, int_ba) indicator_params
  | Medprice : unit -> (Ohlcv.t, float_ba) indicator_params
  | Mfi : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Midpoint : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Midprice : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Min : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Minindex : { timeperiod : int } -> (float_ba, int_ba) indicator_params
  | Minmax : {
      timeperiod : int;
    }
      -> (float_ba, float_ba * float_ba) indicator_params
  | Minmaxindex : {
      timeperiod : int;
    }
      -> (float_ba, int_ba * int_ba) indicator_params
  | Minus_di : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Minus_dm : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Mom : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Mult : unit -> (float_ba * float_ba, float_ba) indicator_params
  | Natr : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Obv : unit -> (Ohlcv.t, float_ba) indicator_params
  | Plus_di : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Plus_dm : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Ppo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba) indicator_params
  | Roc : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Rocp : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Rocr : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Rocr100 : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Rsi : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Sar : {
      acceleration : float;
      maximum : float;
    }
      -> (Ohlcv.t, float_ba) indicator_params
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
      -> (Ohlcv.t, float_ba) indicator_params
  | Sin : unit -> (float_ba, float_ba) indicator_params
  | Sinh : unit -> (float_ba, float_ba) indicator_params
  | Sma : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Sqrt : unit -> (float_ba, float_ba) indicator_params
  | Stddev : {
      timeperiod : int;
      nb_dev : float;
    }
      -> (float_ba, float_ba) indicator_params
  | Stoch : {
      fast_k_period : int;
      slow_k_period : int;
      slow_k_ma_type : Ma_type.t;
      slow_d_period : int;
      slow_d_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba) indicator_params
  | Stochf : {
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (Ohlcv.t, float_ba * float_ba) indicator_params
  | Stochrsi : {
      timeperiod : int;
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (float_ba, float_ba * float_ba) indicator_params
  | Sub : unit -> (float_ba * float_ba, float_ba) indicator_params
  | Sum : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | T3 : {
      timeperiod : int;
      v_factor : float;
    }
      -> (float_ba, float_ba) indicator_params
  | Tan : unit -> (float_ba, float_ba) indicator_params
  | Tanh : unit -> (float_ba, float_ba) indicator_params
  | Tema : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Trange : unit -> (Ohlcv.t, float_ba) indicator_params
  | Trima : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Trix : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Tsf : { timeperiod : int } -> (float_ba, float_ba) indicator_params
  | Typprice : unit -> (Ohlcv.t, float_ba) indicator_params
  | Ultosc : {
      timeperiod1 : int;
      timeperiod2 : int;
      timeperiod3 : int;
    }
      -> (Ohlcv.t, float_ba) indicator_params
  | Var : {
      timeperiod : int;
      nb_dev : float;
    }
      -> (float_ba, float_ba) indicator_params
  | Wclprice : unit -> (Ohlcv.t, float_ba) indicator_params
  | Willr : { timeperiod : int } -> (Ohlcv.t, float_ba) indicator_params
  | Wma : { timeperiod : int } -> (float_ba, float_ba) indicator_params

let lookback : type a b. (a, b) indicator_params -> int =
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

let range ?i x =
  match i with
  | Some i -> i
  | None -> Bigarray.Array1.dim x - 1

exception BadInput

let calculate : type a b.
    (a, b) indicator_params ->
    ?i:int ->
    a ->
    b ->
    (int * int, [> `TALibCode of int ]) result =
 fun params ?i source output ->
  try
    let startIdx = Option.get_or ~default:0 i in
    let offset = lookback params in
    let slice arr =
      match i with
      | Some i -> Bigarray.Array1.sub arr i 1
      | None ->
        let len = Bigarray.Array1.dim arr in
        if offset >= len then raise BadInput
        else Bigarray.Array1.sub arr offset (len - offset)
    in
    match (params, source, output) with
    | Accbands { timeperiod }, ohlcv, (out1, out2, out3) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_accbands startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Acos (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_acos startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ad (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ad startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) outBegIdx outNBElement
            (ba (slice out)))
    | Add (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_add startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Adosc { fast_period; slow_period }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) fast_period slow_period outBegIdx
            outNBElement
            (ba (slice out)))
    | Adx { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Adxr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adxr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Apo { fast_period; slow_period; ma_type }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_apo startIdx endIdx (ba f) fast_period slow_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Aroon { timeperiod }, ohlcv, (out1, out2) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_aroon startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Aroonosc { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_aroonosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Asin (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_asin startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Atan (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_atan startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Atr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_atr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Avgprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_avgprice startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Avgdev { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_avgdev startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type }, f, (out1, out2, out3)
      ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_bbands startIdx endIdx (ba f) timeperiod nb_dev_up nb_dev_dn
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Beta { timeperiod }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_beta startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Bop (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_bop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Cci { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cci startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Cdl2crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3blackcrows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3blackcrows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3inside (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3inside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3linestrike (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3linestrike startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3outside (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3outside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3starsinsouth (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3starsinsouth startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3whitesoldiers (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3whitesoldiers startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlabandonedbaby { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlabandonedbaby startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdladvanceblock (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdladvanceblock startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlbelthold (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlbelthold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlbreakaway (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlbreakaway startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlclosingmarubozu (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlclosingmarubozu startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlconcealbabyswall (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlconcealbabyswall startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlcounterattack (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlcounterattack startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldarkcloudcover { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldarkcloudcover startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdldoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldojistar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldragonflydoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldragonflydoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlengulfing (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlengulfing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdleveningdojistar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdleveningdojistar startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdleveningstar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdleveningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlgapsidesidewhite (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlgapsidesidewhite startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlgravestonedoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlgravestonedoji startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlhammer (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhangingman (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhangingman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlharami (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlharami startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlharamicross (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlharamicross startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhighwave (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhighwave startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhikkake (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhikkake startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhikkakemod (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhikkakemod startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhomingpigeon (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhomingpigeon startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlidentical3crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlidentical3crows startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlinneck (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlinneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlinvertedhammer (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlinvertedhammer startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlkicking (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlkicking startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlkickingbylength (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlkickingbylength startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlladderbottom (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlladderbottom startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdllongleggeddoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdllongleggeddoji startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdllongline (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdllongline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmarubozu (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmatchinglow (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmatchinglow startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmathold { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmathold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmorningdojistar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmorningdojistar startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmorningstar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmorningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlonneck (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlonneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlpiercing (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlpiercing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlrickshawman (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlrickshawman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlrisefall3methods (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlrisefall3methods startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlseparatinglines (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlseparatinglines startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlshootingstar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlshootingstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlshortline (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlshortline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlspinningtop (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlspinningtop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlstalledpattern (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlstalledpattern startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlsticksandwich (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlsticksandwich startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltakuri (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltakuri startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltasukigap (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltasukigap startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlthrusting (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlthrusting startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltristar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltristar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlunique3river (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlunique3river startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlupsidegap2crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlupsidegap2crows startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlxsidegap3methods (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlxsidegap3methods startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Ceil (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ceil startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Cmo { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cmo startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Correl { timeperiod }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_correl startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Cos (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cos startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Cosh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cosh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Dema { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_dema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Div (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_div startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Dx { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_dx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Ema { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Exp (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_exp startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Floor (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_floor startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_dcperiod (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_dcperiod startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_dcphase (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_dcphase startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_phasor (), f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_phasor startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Ht_sine (), f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_sine startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Ht_trendline (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_trendline startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_trendmode (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_trendmode startIdx endIdx (ba f) outBegIdx outNBElement
            (iba (slice out)))
    | Imi { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_imi startIdx endIdx (ba ohlcv.open_) (ba ohlcv.close) timeperiod
            outBegIdx outNBElement
            (ba (slice out)))
    | Kama { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_kama startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Linearreg { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_angle { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_angle startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_intercept { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_intercept startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_slope { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_slope startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Ln (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ln startIdx endIdx (ba f) outBegIdx outNBElement (ba (slice out)))
    | Log10 (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_log10 startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ma { timeperiod; ma_type }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ma startIdx endIdx (ba f) timeperiod (Ma_type.to_int ma_type)
            outBegIdx outNBElement
            (ba (slice out)))
    | Macd { fast_period; slow_period; signal_period }, f, (out1, out2, out3) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macd startIdx endIdx (ba f) fast_period slow_period signal_period
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | ( Macdext
          {
            fast_period;
            fast_ma_type;
            slow_period;
            slow_ma_type;
            signal_period;
            signal_ma_type;
          },
        f,
        (out1, out2, out3) ) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macdext startIdx endIdx (ba f) fast_period
            (Ma_type.to_int fast_ma_type)
            slow_period
            (Ma_type.to_int slow_ma_type)
            signal_period
            (Ma_type.to_int signal_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Macdfix { signal_period }, f, (out1, out2, out3) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macdfix startIdx endIdx (ba f) signal_period outBegIdx
            outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Mama { fast_limit; slow_limit }, f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mama startIdx endIdx (ba f) fast_limit slow_limit outBegIdx
            outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Mavp { min_period; max_period; ma_type }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mavp startIdx endIdx (ba f1) (ba f2) min_period max_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Max { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_max startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Maxindex { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_maxindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (iba (slice out)))
    | Medprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_medprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) outBegIdx
            outNBElement
            (ba (slice out)))
    | Mfi { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mfi startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Midpoint { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_midpoint startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Midprice { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_midprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Min { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_min startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Minindex { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (iba (slice out)))
    | Minmax { timeperiod }, f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minmax startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Minmaxindex { timeperiod }, f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minmaxindex startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (iba (slice out1))
            (iba (slice out2)))
    | Minus_di { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Minus_dm { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Mom { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mom startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Mult (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mult startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Natr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_natr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Obv (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_obv startIdx endIdx (ba ohlcv.close) (ba ohlcv.volume) outBegIdx
            outNBElement
            (ba (slice out)))
    | Plus_di { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_plus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Plus_dm { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_plus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
            outBegIdx outNBElement
            (ba (slice out)))
    | Ppo { fast_period; slow_period; ma_type }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ppo startIdx endIdx (ba f) fast_period slow_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Roc { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_roc startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Rocp { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocp startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Rocr { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocr startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Rocr100 { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocr100 startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Rsi { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rsi startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Sar { acceleration; maximum }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sar startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) acceleration
            maximum outBegIdx outNBElement
            (ba (slice out)))
    | ( Sarext
          {
            start_value;
            offset_on_reverse;
            acceleration_init_long;
            acceleration_long;
            acceleration_max_long;
            acceleration_init_short;
            acceleration_short;
            acceleration_max_short;
          },
        ohlcv,
        out ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sarext startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) start_value
            offset_on_reverse acceleration_init_long acceleration_long
            acceleration_max_long acceleration_init_short acceleration_short
            acceleration_max_short outBegIdx outNBElement
            (ba (slice out)))
    | Sin (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sin startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Sinh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sinh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Sma { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sma startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Sqrt (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sqrt startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Stddev { timeperiod; nb_dev }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stddev startIdx endIdx (ba f) timeperiod nb_dev outBegIdx
            outNBElement
            (ba (slice out)))
    | ( Stoch
          {
            fast_k_period;
            slow_k_period;
            slow_k_ma_type;
            slow_d_period;
            slow_d_ma_type;
          },
        ohlcv,
        (out1, out2) ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stoch startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) fast_k_period slow_k_period
            (Ma_type.to_int slow_k_ma_type)
            slow_d_period
            (Ma_type.to_int slow_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | ( Stochf { fast_k_period; fast_d_period; fast_d_ma_type },
        ohlcv,
        (out1, out2) ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stochf startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) fast_k_period fast_d_period
            (Ma_type.to_int fast_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | ( Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type },
        f,
        (out1, out2) ) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stochrsi startIdx endIdx (ba f) timeperiod fast_k_period
            fast_d_period
            (Ma_type.to_int fast_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Sub (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sub startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Sum { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sum startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | T3 { timeperiod; v_factor }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_t3 startIdx endIdx (ba f) timeperiod v_factor outBegIdx
            outNBElement
            (ba (slice out)))
    | Tan (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tan startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Tanh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tanh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Tema { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Trange (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trange startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Trima { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trima startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Trix { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trix startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Tsf { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tsf startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Typprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_typprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Ultosc { timeperiod1; timeperiod2; timeperiod3 }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ultosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod1 timeperiod2 timeperiod3 outBegIdx
            outNBElement
            (ba (slice out)))
    | Var { timeperiod; nb_dev }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_var startIdx endIdx (ba f) timeperiod nb_dev outBegIdx
            outNBElement
            (ba (slice out)))
    | Wclprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_wclprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Willr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_willr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Wma { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_wma startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
  with
  | BadInput -> Result.fail @@ `TALibCode (-2)
