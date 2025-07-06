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

type 'a indicator_params =
  | Accbands : {
      timeperiod : int;
    }
      -> (float_ba * float_ba * float_ba) indicator_params
  | Acos : unit -> float_ba indicator_params
  | Ad : unit -> float_ba indicator_params
  | Add : unit -> float_ba indicator_params
  | Adosc : {
      fast_period : int;
      slow_period : int;
    }
      -> float_ba indicator_params
  | Adx : { timeperiod : int } -> float_ba indicator_params
  | Adxr : { timeperiod : int } -> float_ba indicator_params
  | Apo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> float_ba indicator_params
  | Aroon : { timeperiod : int } -> (float_ba * float_ba) indicator_params
  | Aroonosc : { timeperiod : int } -> float_ba indicator_params
  | Asin : unit -> float_ba indicator_params
  | Atan : unit -> float_ba indicator_params
  | Atr : { timeperiod : int } -> float_ba indicator_params
  | Avgprice : unit -> float_ba indicator_params
  | Avgdev : { timeperiod : int } -> float_ba indicator_params
  | Bbands : {
      timeperiod : int;
      nb_dev_up : float;
      nb_dev_dn : float;
      ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba * float_ba) indicator_params
  | Beta : { timeperiod : int } -> float_ba indicator_params
  | Bop : unit -> float_ba indicator_params
  | Cci : { timeperiod : int } -> float_ba indicator_params
  | Cdl2crows : unit -> int_ba indicator_params
  | Cdl3blackcrows : unit -> int_ba indicator_params
  | Cdl3inside : unit -> int_ba indicator_params
  | Cdl3linestrike : unit -> int_ba indicator_params
  | Cdl3outside : unit -> int_ba indicator_params
  | Cdl3starsinsouth : unit -> int_ba indicator_params
  | Cdl3whitesoldiers : unit -> int_ba indicator_params
  | Cdlabandonedbaby : { penetration : float } -> int_ba indicator_params
  | Cdladvanceblock : unit -> int_ba indicator_params
  | Cdlbelthold : unit -> int_ba indicator_params
  | Cdlbreakaway : unit -> int_ba indicator_params
  | Cdlclosingmarubozu : unit -> int_ba indicator_params
  | Cdlconcealbabyswall : unit -> int_ba indicator_params
  | Cdlcounterattack : unit -> int_ba indicator_params
  | Cdldarkcloudcover : { penetration : float } -> int_ba indicator_params
  | Cdldoji : unit -> int_ba indicator_params
  | Cdldojistar : unit -> int_ba indicator_params
  | Cdldragonflydoji : unit -> int_ba indicator_params
  | Cdlengulfing : unit -> int_ba indicator_params
  | Cdleveningdojistar : { penetration : float } -> int_ba indicator_params
  | Cdleveningstar : { penetration : float } -> int_ba indicator_params
  | Cdlgapsidesidewhite : unit -> int_ba indicator_params
  | Cdlgravestonedoji : unit -> int_ba indicator_params
  | Cdlhammer : unit -> int_ba indicator_params
  | Cdlhangingman : unit -> int_ba indicator_params
  | Cdlharami : unit -> int_ba indicator_params
  | Cdlharamicross : unit -> int_ba indicator_params
  | Cdlhighwave : unit -> int_ba indicator_params
  | Cdlhikkake : unit -> int_ba indicator_params
  | Cdlhikkakemod : unit -> int_ba indicator_params
  | Cdlhomingpigeon : unit -> int_ba indicator_params
  | Cdlidentical3crows : unit -> int_ba indicator_params
  | Cdlinneck : unit -> int_ba indicator_params
  | Cdlinvertedhammer : unit -> int_ba indicator_params
  | Cdlkicking : unit -> int_ba indicator_params
  | Cdlkickingbylength : unit -> int_ba indicator_params
  | Cdlladderbottom : unit -> int_ba indicator_params
  | Cdllongleggeddoji : unit -> int_ba indicator_params
  | Cdllongline : unit -> int_ba indicator_params
  | Cdlmarubozu : unit -> int_ba indicator_params
  | Cdlmatchinglow : unit -> int_ba indicator_params
  | Cdlmathold : { penetration : float } -> int_ba indicator_params
  | Cdlmorningdojistar : { penetration : float } -> int_ba indicator_params
  | Cdlmorningstar : { penetration : float } -> int_ba indicator_params
  | Cdlonneck : unit -> int_ba indicator_params
  | Cdlpiercing : unit -> int_ba indicator_params
  | Cdlrickshawman : unit -> int_ba indicator_params
  | Cdlrisefall3methods : unit -> int_ba indicator_params
  | Cdlseparatinglines : unit -> int_ba indicator_params
  | Cdlshootingstar : unit -> int_ba indicator_params
  | Cdlshortline : unit -> int_ba indicator_params
  | Cdlspinningtop : unit -> int_ba indicator_params
  | Cdlstalledpattern : unit -> int_ba indicator_params
  | Cdlsticksandwich : unit -> int_ba indicator_params
  | Cdltakuri : unit -> int_ba indicator_params
  | Cdltasukigap : unit -> int_ba indicator_params
  | Cdlthrusting : unit -> int_ba indicator_params
  | Cdltristar : unit -> int_ba indicator_params
  | Cdlunique3river : unit -> int_ba indicator_params
  | Cdlupsidegap2crows : unit -> int_ba indicator_params
  | Cdlxsidegap3methods : unit -> int_ba indicator_params
  | Ceil : unit -> float_ba indicator_params
  | Cmo : { timeperiod : int } -> float_ba indicator_params
  | Correl : { timeperiod : int } -> float_ba indicator_params
  | Cos : unit -> float_ba indicator_params
  | Cosh : unit -> float_ba indicator_params
  | Dema : { timeperiod : int } -> float_ba indicator_params
  | Div : unit -> float_ba indicator_params
  | Dx : { timeperiod : int } -> float_ba indicator_params
  | Ema : { timeperiod : int } -> float_ba indicator_params
  | Exp : unit -> float_ba indicator_params
  | Floor : unit -> float_ba indicator_params
  | Ht_dcperiod : unit -> float_ba indicator_params
  | Ht_dcphase : unit -> float_ba indicator_params
  | Ht_phasor : unit -> (float_ba * float_ba) indicator_params
  | Ht_sine : unit -> (float_ba * float_ba) indicator_params
  | Ht_trendline : unit -> float_ba indicator_params
  | Ht_trendmode : unit -> int_ba indicator_params
  | Imi : { timeperiod : int } -> float_ba indicator_params
  | Kama : { timeperiod : int } -> float_ba indicator_params
  | Linearreg : { timeperiod : int } -> float_ba indicator_params
  | Linearreg_angle : { timeperiod : int } -> float_ba indicator_params
  | Linearreg_intercept : { timeperiod : int } -> float_ba indicator_params
  | Linearreg_slope : { timeperiod : int } -> float_ba indicator_params
  | Ln : unit -> float_ba indicator_params
  | Log10 : unit -> float_ba indicator_params
  | Ma : { timeperiod : int; ma_type : Ma_type.t } -> float_ba indicator_params
  | Macd : {
      fast_period : int;
      slow_period : int;
      signal_period : int;
    }
      -> (float_ba * float_ba * float_ba) indicator_params
  | Macdext : {
      fast_period : int;
      fast_ma_type : Ma_type.t;
      slow_period : int;
      slow_ma_type : Ma_type.t;
      signal_period : int;
      signal_ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba * float_ba) indicator_params
  | Macdfix : {
      signal_period : int;
    }
      -> (float_ba * float_ba * float_ba) indicator_params
  | Mama : {
      fast_limit : float;
      slow_limit : float;
    }
      -> (float_ba * float_ba) indicator_params
  | Mavp : {
      min_period : int;
      max_period : int;
      ma_type : Ma_type.t;
    }
      -> float_ba indicator_params
  | Max : { timeperiod : int } -> float_ba indicator_params
  | Maxindex : { timeperiod : int } -> int_ba indicator_params
  | Medprice : unit -> float_ba indicator_params
  | Mfi : { timeperiod : int } -> float_ba indicator_params
  | Midpoint : { timeperiod : int } -> float_ba indicator_params
  | Midprice : { timeperiod : int } -> float_ba indicator_params
  | Min : { timeperiod : int } -> float_ba indicator_params
  | Minindex : { timeperiod : int } -> int_ba indicator_params
  | Minmax : { timeperiod : int } -> (float_ba * float_ba) indicator_params
  | Minmaxindex : { timeperiod : int } -> (int_ba * int_ba) indicator_params
  | Minus_di : { timeperiod : int } -> float_ba indicator_params
  | Minus_dm : { timeperiod : int } -> float_ba indicator_params
  | Mom : { timeperiod : int } -> float_ba indicator_params
  | Mult : unit -> float_ba indicator_params
  | Natr : { timeperiod : int } -> float_ba indicator_params
  | Obv : unit -> float_ba indicator_params
  | Plus_di : { timeperiod : int } -> float_ba indicator_params
  | Plus_dm : { timeperiod : int } -> float_ba indicator_params
  | Ppo : {
      fast_period : int;
      slow_period : int;
      ma_type : Ma_type.t;
    }
      -> float_ba indicator_params
  | Roc : { timeperiod : int } -> float_ba indicator_params
  | Rocp : { timeperiod : int } -> float_ba indicator_params
  | Rocr : { timeperiod : int } -> float_ba indicator_params
  | Rocr100 : { timeperiod : int } -> float_ba indicator_params
  | Rsi : { timeperiod : int } -> float_ba indicator_params
  | Sar : { acceleration : float; maximum : float } -> float_ba indicator_params
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
      -> float_ba indicator_params
  | Sin : unit -> float_ba indicator_params
  | Sinh : unit -> float_ba indicator_params
  | Sma : { timeperiod : int } -> float_ba indicator_params
  | Sqrt : unit -> float_ba indicator_params
  | Stddev : { timeperiod : int; nb_dev : float } -> float_ba indicator_params
  | Stoch : {
      fast_k_period : int;
      slow_k_period : int;
      slow_k_ma_type : Ma_type.t;
      slow_d_period : int;
      slow_d_ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba) indicator_params
  | Stochf : {
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba) indicator_params
  | Stochrsi : {
      timeperiod : int;
      fast_k_period : int;
      fast_d_period : int;
      fast_d_ma_type : Ma_type.t;
    }
      -> (float_ba * float_ba) indicator_params
  | Sub : unit -> float_ba indicator_params
  | Sum : { timeperiod : int } -> float_ba indicator_params
  | T3 : { timeperiod : int; v_factor : float } -> float_ba indicator_params
  | Tan : unit -> float_ba indicator_params
  | Tanh : unit -> float_ba indicator_params
  | Tema : { timeperiod : int } -> float_ba indicator_params
  | Trange : unit -> float_ba indicator_params
  | Trima : { timeperiod : int } -> float_ba indicator_params
  | Trix : { timeperiod : int } -> float_ba indicator_params
  | Tsf : { timeperiod : int } -> float_ba indicator_params
  | Typprice : unit -> float_ba indicator_params
  | Ultosc : {
      timeperiod1 : int;
      timeperiod2 : int;
      timeperiod3 : int;
    }
      -> float_ba indicator_params
  | Var : { timeperiod : int; nb_dev : float } -> float_ba indicator_params
  | Wclprice : unit -> float_ba indicator_params
  | Willr : { timeperiod : int } -> float_ba indicator_params
  | Wma : { timeperiod : int } -> float_ba indicator_params

let lookback : type a. a indicator_params -> int =
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

type input_source =
  | Ohlcv of Ohlcv.t
  | Float of float_ba
  | Float_float of float_ba * float_ba
  | Float_ohlcv of float_ba * Ohlcv.t

let calculate : type a.
    int * int ->
    input_source ->
    a indicator_params ->
    a ->
    (int * int, [> `TALibCode of int ]) result =
 fun (startIdx, endIdx) source params output ->
  match (params, source, output) with
  | Accbands { timeperiod }, Ohlcv ohlcv, (out1, out2, out3) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_accbands startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out1) (ba out2)
          (ba out3))
  | Acos (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_acos startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ad (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ad startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          (ba ohlcv.volume) outBegIdx outNBElement (ba out))
  | Add (), Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_add startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement (ba out))
  | Adosc { fast_period; slow_period }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_adosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) (ba ohlcv.volume) fast_period slow_period outBegIdx
          outNBElement (ba out))
  | Adx { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_adx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          timeperiod outBegIdx outNBElement (ba out))
  | Adxr { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_adxr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out))
  | Apo { fast_period; slow_period; ma_type }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_apo startIdx endIdx (ba f) fast_period slow_period
          (Ma_type.to_int ma_type) outBegIdx outNBElement (ba out))
  | Aroon { timeperiod }, Ohlcv ohlcv, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_aroon startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
          outBegIdx outNBElement (ba out1) (ba out2))
  | Aroonosc { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_aroonosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
          outBegIdx outNBElement (ba out))
  | Asin (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_asin startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Atan (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_atan startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Atr { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_atr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          timeperiod outBegIdx outNBElement (ba out))
  | Avgprice (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_avgprice startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (ba out))
  | Avgdev { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_avgdev startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | ( Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type },
      Float f,
      (out1, out2, out3) ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_bbands startIdx endIdx (ba f) timeperiod nb_dev_up nb_dev_dn
          (Ma_type.to_int ma_type) outBegIdx outNBElement (ba out1) (ba out2)
          (ba out3))
  | Beta { timeperiod }, Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_beta startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
          outNBElement (ba out))
  | Bop (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_bop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) outBegIdx outNBElement (ba out))
  | Cci { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cci startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          timeperiod outBegIdx outNBElement (ba out))
  | Cdl2crows (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3blackcrows (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3blackcrows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3inside (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3inside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3linestrike (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3linestrike startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3outside (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3outside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3starsinsouth (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3starsinsouth startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdl3whitesoldiers (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdl3whitesoldiers startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlabandonedbaby { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlabandonedbaby startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdladvanceblock (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdladvanceblock startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlbelthold (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlbelthold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlbreakaway (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlbreakaway startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlclosingmarubozu (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlclosingmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlconcealbabyswall (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlconcealbabyswall startIdx endIdx (ba ohlcv.open_)
          (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
          (iba out))
  | Cdlcounterattack (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlcounterattack startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdldarkcloudcover { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdldarkcloudcover startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdldoji (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdldoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdldojistar (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdldojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdldragonflydoji (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdldragonflydoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlengulfing (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlengulfing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdleveningdojistar { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdleveningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdleveningstar { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdleveningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdlgapsidesidewhite (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlgapsidesidewhite startIdx endIdx (ba ohlcv.open_)
          (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
          (iba out))
  | Cdlgravestonedoji (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlgravestonedoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhammer (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhangingman (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhangingman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlharami (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlharami startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlharamicross (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlharamicross startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhighwave (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhighwave startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhikkake (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhikkake startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhikkakemod (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhikkakemod startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlhomingpigeon (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlhomingpigeon startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlidentical3crows (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlidentical3crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlinneck (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlinneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlinvertedhammer (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlinvertedhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlkicking (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlkicking startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlkickingbylength (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlkickingbylength startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlladderbottom (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlladderbottom startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdllongleggeddoji (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdllongleggeddoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdllongline (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdllongline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlmarubozu (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlmatchinglow (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlmatchinglow startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlmathold { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlmathold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdlmorningdojistar { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlmorningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdlmorningstar { penetration }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlmorningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
          (iba out))
  | Cdlonneck (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlonneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlpiercing (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlpiercing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlrickshawman (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlrickshawman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlrisefall3methods (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlrisefall3methods startIdx endIdx (ba ohlcv.open_)
          (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
          (iba out))
  | Cdlseparatinglines (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlseparatinglines startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlshootingstar (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlshootingstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlshortline (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlshortline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlspinningtop (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlspinningtop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlstalledpattern (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlstalledpattern startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlsticksandwich (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlsticksandwich startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdltakuri (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdltakuri startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdltasukigap (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdltasukigap startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlthrusting (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlthrusting startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdltristar (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdltristar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlunique3river (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlunique3river startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlupsidegap2crows (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlupsidegap2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
          (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba out))
  | Cdlxsidegap3methods (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cdlxsidegap3methods startIdx endIdx (ba ohlcv.open_)
          (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
          (iba out))
  | Ceil (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ceil startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Cmo { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cmo startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Correl { timeperiod }, Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_correl startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
          outNBElement (ba out))
  | Cos (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cos startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Cosh (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_cosh startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Dema { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_dema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Div (), Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_div startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement (ba out))
  | Dx { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_dx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          timeperiod outBegIdx outNBElement (ba out))
  | Ema { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Exp (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_exp startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Floor (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_floor startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ht_dcperiod (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_dcperiod startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ht_dcphase (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_dcphase startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ht_phasor (), Float f, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_phasor startIdx endIdx (ba f) outBegIdx outNBElement (ba out1)
          (ba out2))
  | Ht_sine (), Float f, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_sine startIdx endIdx (ba f) outBegIdx outNBElement (ba out1)
          (ba out2))
  | Ht_trendline (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_trendline startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ht_trendmode (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ht_trendmode startIdx endIdx (ba f) outBegIdx outNBElement
          (iba out))
  | Imi { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_imi startIdx endIdx (ba ohlcv.open_) (ba ohlcv.close) timeperiod
          outBegIdx outNBElement (ba out))
  | Kama { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_kama startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Linearreg { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_linearreg startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Linearreg_angle { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_linearreg_angle startIdx endIdx (ba f) timeperiod outBegIdx
          outNBElement (ba out))
  | Linearreg_intercept { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_linearreg_intercept startIdx endIdx (ba f) timeperiod outBegIdx
          outNBElement (ba out))
  | Linearreg_slope { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_linearreg_slope startIdx endIdx (ba f) timeperiod outBegIdx
          outNBElement (ba out))
  | Ln (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ln startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Log10 (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_log10 startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Ma { timeperiod; ma_type }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ma startIdx endIdx (ba f) timeperiod (Ma_type.to_int ma_type)
          outBegIdx outNBElement (ba out))
  | Macd { fast_period; slow_period; signal_period }, Float f, (out1, out2, out3)
    ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_macd startIdx endIdx (ba f) fast_period slow_period signal_period
          outBegIdx outNBElement (ba out1) (ba out2) (ba out3))
  | ( Macdext
        {
          fast_period;
          fast_ma_type;
          slow_period;
          slow_ma_type;
          signal_period;
          signal_ma_type;
        },
      Float f,
      (out1, out2, out3) ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_macdext startIdx endIdx (ba f) fast_period
          (Ma_type.to_int fast_ma_type)
          slow_period
          (Ma_type.to_int slow_ma_type)
          signal_period
          (Ma_type.to_int signal_ma_type)
          outBegIdx outNBElement (ba out1) (ba out2) (ba out3))
  | Macdfix { signal_period }, Float f, (out1, out2, out3) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_macdfix startIdx endIdx (ba f) signal_period outBegIdx outNBElement
          (ba out1) (ba out2) (ba out3))
  | Mama { fast_limit; slow_limit }, Float f, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_mama startIdx endIdx (ba f) fast_limit slow_limit outBegIdx
          outNBElement (ba out1) (ba out2))
  | Mavp { min_period; max_period; ma_type }, Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_mavp startIdx endIdx (ba f1) (ba f2) min_period max_period
          (Ma_type.to_int ma_type) outBegIdx outNBElement (ba out))
  | Max { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_max startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Maxindex { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_maxindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (iba out))
  | Medprice (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_medprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) outBegIdx
          outNBElement (ba out))
  | Mfi { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_mfi startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
          (ba ohlcv.volume) timeperiod outBegIdx outNBElement (ba out))
  | Midpoint { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_midpoint startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Midprice { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_midprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
          outBegIdx outNBElement (ba out))
  | Min { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_min startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Minindex { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_minindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (iba out))
  | Minmax { timeperiod }, Float f, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_minmax startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out1) (ba out2))
  | Minmaxindex { timeperiod }, Float f, (out1, out2) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_minmaxindex startIdx endIdx (ba f) timeperiod outBegIdx
          outNBElement (iba out1) (iba out2))
  | Minus_di { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_minus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out))
  | Minus_dm { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_minus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
          outBegIdx outNBElement (ba out))
  | Mom { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_mom startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Mult (), Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_mult startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
          (ba out))
  | Natr { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_natr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out))
  | Obv (), Float_ohlcv (f, ohlcv), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_obv startIdx endIdx (ba f) (ba ohlcv.volume) outBegIdx outNBElement
          (ba out))
  | Plus_di { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_plus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out))
  | Plus_dm { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_plus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
          outBegIdx outNBElement (ba out))
  | Ppo { fast_period; slow_period; ma_type }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ppo startIdx endIdx (ba f) fast_period slow_period
          (Ma_type.to_int ma_type) outBegIdx outNBElement (ba out))
  | Roc { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_roc startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Rocp { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_rocp startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Rocr { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_rocr startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Rocr100 { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_rocr100 startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Rsi { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_rsi startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Sar { acceleration; maximum }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sar startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) acceleration
          maximum outBegIdx outNBElement (ba out))
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
      Ohlcv ohlcv,
      out ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sarext startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) start_value
          offset_on_reverse acceleration_init_long acceleration_long
          acceleration_max_long acceleration_init_short acceleration_short
          acceleration_max_short outBegIdx outNBElement (ba out))
  | Sin (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sin startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Sinh (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sinh startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Sma { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sma startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Sqrt (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sqrt startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Stddev { timeperiod; nb_dev }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_stddev startIdx endIdx (ba f) timeperiod nb_dev outBegIdx
          outNBElement (ba out))
  | ( Stoch
        {
          fast_k_period;
          slow_k_period;
          slow_k_ma_type;
          slow_d_period;
          slow_d_ma_type;
        },
      Ohlcv ohlcv,
      (out1, out2) ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_stoch startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) fast_k_period slow_k_period
          (Ma_type.to_int slow_k_ma_type)
          slow_d_period
          (Ma_type.to_int slow_d_ma_type)
          outBegIdx outNBElement (ba out1) (ba out2))
  | ( Stochf { fast_k_period; fast_d_period; fast_d_ma_type },
      Ohlcv ohlcv,
      (out1, out2) ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_stochf startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) fast_k_period fast_d_period
          (Ma_type.to_int fast_d_ma_type)
          outBegIdx outNBElement (ba out1) (ba out2))
  | ( Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type },
      Float f,
      (out1, out2) ) ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_stochrsi startIdx endIdx (ba f) timeperiod fast_k_period
          fast_d_period
          (Ma_type.to_int fast_d_ma_type)
          outBegIdx outNBElement (ba out1) (ba out2))
  | Sub (), Float_float (f1, f2), out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sub startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement (ba out))
  | Sum { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_sum startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | T3 { timeperiod; v_factor }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_t3 startIdx endIdx (ba f) timeperiod v_factor outBegIdx
          outNBElement (ba out))
  | Tan (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_tan startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Tanh (), Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_tanh startIdx endIdx (ba f) outBegIdx outNBElement (ba out))
  | Tema { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_tema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Trange (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_trange startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) outBegIdx outNBElement (ba out))
  | Trima { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_trima startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Trix { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_trix startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Tsf { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_tsf startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | Typprice (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_typprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) outBegIdx outNBElement (ba out))
  | Ultosc { timeperiod1; timeperiod2; timeperiod3 }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_ultosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod1 timeperiod2 timeperiod3 outBegIdx
          outNBElement (ba out))
  | Var { timeperiod; nb_dev }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_var startIdx endIdx (ba f) timeperiod nb_dev outBegIdx outNBElement
          (ba out))
  | Wclprice (), Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_wclprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) outBegIdx outNBElement (ba out))
  | Willr { timeperiod }, Ohlcv ohlcv, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_willr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
          (ba ohlcv.close) timeperiod outBegIdx outNBElement (ba out))
  | Wma { timeperiod }, Float f, out ->
    wrap (fun outBegIdx outNBElement ->
        F.ta_wma startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
          (ba out))
  | _, _, _ -> Error `InvalidInput
