module S = Safe

type t = Pack : ('a, 'b) S.t -> t

let pack x = Pack x
let calculate = Calculate.calculate

let calculate ?i (Pack params) (source : Input.t) (output : Output.t) =
  match (params, source, output) with
  | S.Accbands _, Input.Ohlcv source, Output.FloatBA3 (x, y, z) ->
    calculate ?i params source (x, y, z)
  | S.Acos _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ad _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Add _, Input.FloatBA2 (s1, s2), Output.FloatBA output ->
    calculate ?i params (s1, s2) output
  | S.Adosc _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Adx _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Adxr _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Apo _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Aroon _, Input.Ohlcv source, Output.FloatBA2 (o1, o2) ->
    calculate ?i params source (o1, o2)
  | S.Aroonosc _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Asin _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Atan _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Atr _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Avgprice _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Avgdev _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Bbands _, Input.Ohlcv source, Output.FloatBA3 (o1, o2, o3) ->
    calculate ?i params source (o1, o2, o3)
  | S.Beta _, Input.FloatBA2 (s1, s2), Output.FloatBA output ->
    calculate ?i params (s1, s2) output
  | S.Bop _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Cci _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Cdl2crows _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3blackcrows _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3inside _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3linestrike _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3outside _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3starsinsouth _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdl3whitesoldiers _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlabandonedbaby _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdladvanceblock _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlbelthold _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlbreakaway _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlclosingmarubozu _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlconcealbabyswall _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlcounterattack _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdldarkcloudcover _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdldoji _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdldojistar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdldragonflydoji _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlengulfing _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdleveningdojistar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdleveningstar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlgapsidesidewhite _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlgravestonedoji _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhammer _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhangingman _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlharami _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlharamicross _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhighwave _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhikkake _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhikkakemod _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlhomingpigeon _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlidentical3crows _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlinneck _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlinvertedhammer _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlkicking _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlkickingbylength _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlladderbottom _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdllongleggeddoji _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdllongline _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlmarubozu _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlmatchinglow _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlmathold _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlmorningdojistar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlmorningstar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlonneck _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlpiercing _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlrickshawman _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlrisefall3methods _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlseparatinglines _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlshootingstar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlshortline _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlspinningtop _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlstalledpattern _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlsticksandwich _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdltakuri _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdltasukigap _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlthrusting _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdltristar _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlunique3river _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlupsidegap2crows _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Cdlxsidegap3methods _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Ceil _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Cmo _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Correl _, FloatBA2 (s0, s1), Output.FloatBA output ->
    calculate ?i params (s0, s1) output
  | S.Cos _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Cosh _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Dema _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Div _, FloatBA2 (s0, s1), Output.FloatBA output ->
    calculate ?i params (s0, s1) output
  | S.Dx _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ema _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Exp _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Floor _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ht_dcperiod _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ht_dcphase _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ht_phasor _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Ht_sine _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Ht_trendline _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ht_trendmode _, Input.Ohlcv source, Output.IntBA output ->
    calculate ?i params source output
  | S.Imi _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Kama _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Linearreg _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Linearreg_angle _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Linearreg_intercept _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Linearreg_slope _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ln _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Log10 _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ma _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Macd _, Input.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    calculate ?i params source (o0, o1, o2)
  | S.Macdext _, Input.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    calculate ?i params source (o0, o1, o2)
  | S.Macdfix _, Input.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    calculate ?i params source (o0, o1, o2)
  | S.Mama _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Mavp _, FloatBA2 (s0, s1), Output.FloatBA output ->
    calculate ?i params (s0, s1) output
  | S.Max _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Maxindex _, Input.FloatBA source, Output.IntBA output ->
    calculate ?i params source output
  | S.Medprice _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Mfi _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Midpoint _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Midprice _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Min _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Minindex _, Input.FloatBA source, Output.IntBA output ->
    calculate ?i params source output
  | S.Minmax _, Input.FloatBA source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Minmaxindex _, Input.FloatBA source, Output.IntBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Minus_di _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Minus_dm _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Mom _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Mult _, FloatBA2 (s0, s1), Output.FloatBA output ->
    calculate ?i params (s0, s1) output
  | S.Natr _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Obv _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Plus_di _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Plus_dm _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ppo _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Roc _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Rocp _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Rocr _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Rocr100 _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Rsi _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sar _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sarext _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sin _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sinh _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sma _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Sqrt _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Stddev _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Stoch _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Stochf _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Stochrsi _, Input.Ohlcv source, FloatBA2 (o0, o1) ->
    calculate ?i params source (o0, o1)
  | S.Sub _, FloatBA2 (s0, s1), Output.FloatBA output ->
    calculate ?i params (s0, s1) output
  | S.Sum _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.T3 _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Tan _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Tanh _, Input.FloatBA source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Tema _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Trange _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Trima _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Trix _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Tsf _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Typprice _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Ultosc _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Var _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Wclprice _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Willr _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | S.Wma _, Input.Ohlcv source, Output.FloatBA output ->
    calculate ?i params source output
  | wrapper, i, o ->
    let str = S.to_string wrapper in
    let inp = Input.to_string i in
    let out = Output.to_string o in
    let msg =
      Format.asprintf
        "%s %s %s: Invalid input or output source/destinations chosen in TA-Lib"
        str inp out
    in
    Error (`FatalError msg)

let input_flag_from_wrapper : t -> Input.Flag.t =
 fun (Pack wrapper_params) ->
  match wrapper_params with
  | S.Medprice _
  | S.Dema _
  | S.Accbands _
  | S.Ad _
  | S.Adosc _
  | S.Adx _
  | S.Adxr _
  | S.Apo _
  | S.Aroon _
  | S.Aroonosc _
  | S.Atr _
  | S.Avgprice _
  | S.Avgdev _
  | S.Bbands _
  | S.Bop _
  | S.Cci _
  | S.Cdl2crows _
  | S.Cdl3blackcrows _
  | S.Cdl3inside _
  | S.Cdl3linestrike _
  | S.Cdl3outside _
  | S.Cdl3starsinsouth _
  | S.Cdl3whitesoldiers _
  | S.Cdlabandonedbaby _
  | S.Cdladvanceblock _
  | S.Cdlbelthold _
  | S.Cdlbreakaway _
  | S.Cdlclosingmarubozu _
  | S.Cdlconcealbabyswall _
  | S.Cdlcounterattack _
  | S.Cdldarkcloudcover _
  | S.Cdldoji _
  | S.Cdldojistar _
  | S.Cdldragonflydoji _
  | S.Cdlengulfing _
  | S.Cdleveningdojistar _
  | S.Cdleveningstar _
  | S.Cdlgapsidesidewhite _
  | S.Cdlgravestonedoji _
  | S.Cdlhammer _
  | S.Cdlhangingman _
  | S.Cdlharami _
  | S.Cdlharamicross _
  | S.Cdlhighwave _
  | S.Cdlhikkake _
  | S.Cdlhikkakemod _
  | S.Cdlhomingpigeon _
  | S.Cdlidentical3crows _
  | S.Cdlinneck _
  | S.Cdlinvertedhammer _
  | S.Cdlkicking _
  | S.Cdlkickingbylength _
  | S.Cdlladderbottom _
  | S.Cdllongleggeddoji _
  | S.Cdllongline _
  | S.Cdlmarubozu _
  | S.Cdlmatchinglow _
  | S.Cdlmathold _
  | S.Cdlmorningdojistar _
  | S.Cdlmorningstar _
  | S.Cdlonneck _
  | S.Cdlpiercing _
  | S.Cdlrickshawman _
  | S.Cdlrisefall3methods _
  | S.Cdlseparatinglines _
  | S.Cdlshootingstar _
  | S.Cdlshortline _
  | S.Cdlspinningtop _
  | S.Cdlstalledpattern _
  | S.Cdlsticksandwich _
  | S.Cdltakuri _
  | S.Cdltasukigap _
  | S.Cdlthrusting _
  | S.Cdltristar _
  | S.Cdlunique3river _
  | S.Cdlupsidegap2crows _
  | S.Cdlxsidegap3methods _
  | S.Cmo _
  | S.Dx _
  | S.Ema _
  | S.Ht_dcperiod _
  | S.Ht_dcphase _
  | S.Ht_phasor _
  | S.Ht_sine _
  | S.Ht_trendline _
  | S.Ht_trendmode _
  | S.Imi _
  | S.Kama _
  | S.Linearreg _
  | S.Linearreg_angle _
  | S.Linearreg_intercept _
  | S.Linearreg_slope _
  | S.Ma _
  | S.Macd _
  | S.Macdext _
  | S.Macdfix _
  | S.Mama _
  | S.Mfi _
  | S.Midprice _
  | S.Minus_di _
  | S.Minus_dm _
  | S.Mom _
  | S.Natr _
  | S.Obv _
  | S.Plus_di _
  | S.Plus_dm _
  | S.Ppo _
  | S.Roc _
  | S.Rocp _
  | S.Rocr _
  | S.Rocr100 _
  | S.Rsi _
  | S.Sar _
  | S.Sarext _
  | S.Sma _
  | S.Stoch _
  | S.Stochf _
  | S.Stochrsi _
  | S.Sum _
  | S.T3 _
  | S.Tema _
  | S.Trange _
  | S.Trima _
  | S.Trix _
  | S.Tsf _
  | S.Typprice _
  | S.Ultosc _
  | S.Var _
  | S.Wclprice _
  | S.Willr _
  | S.Wma _ ->
    OhlcvFlag
  | S.Sin _
  | S.Sinh _
  | S.Acos _
  | S.Asin _
  | S.Atan _
  | S.Ceil _
  | S.Cos _
  | S.Cosh _
  | S.Exp _
  | S.Floor _
  | S.Ln _
  | S.Log10 _
  | S.Max _
  | S.Maxindex _
  | S.Midpoint _
  | S.Min _
  | S.Minindex _
  | S.Minmax _
  | S.Minmaxindex _
  | S.Sqrt _
  | S.Stddev _
  | S.Tan _
  | S.Tanh _ ->
    FloatBAFlag
  | S.Add _
  | S.Beta _
  | S.Correl _
  | S.Div _
  | S.Mavp _
  | S.Mult _
  | S.Sub _ ->
    FloatBA2Flag

let output_flag_from_wrapper : t -> Output.Flag.t =
 fun (Pack wrapper_params) ->
  match wrapper_params with
  | S.Accbands { timeperiod } ->
    Output.Flag.FloatBA3Flag
      ( F
          (Indicator.Float.UpperBBand
             {
               timeperiod;
               nb_dev_up = 2.0;
               nb_dev_dn = 2.0;
               ma_type = Ma_type.Sma;
             }),
        F
          (Indicator.Float.MiddleBBand
             {
               timeperiod;
               nb_dev_up = 2.0;
               nb_dev_dn = 2.0;
               ma_type = Ma_type.Sma;
             }),
        F
          (Indicator.Float.LowerBBand
             {
               timeperiod;
               nb_dev_up = 2.0;
               nb_dev_dn = 2.0;
               ma_type = Ma_type.Sma;
             }) )
  | S.Acos _ -> FloatBAFlag (F Indicator.Float.Acos)
  | S.Ad _ -> FloatBAFlag (F Indicator.Float.Ad)
  | S.Add _ -> FloatBAFlag (F Indicator.Float.Add)
  | S.Adosc { fast_period; slow_period } ->
    FloatBAFlag (F (Indicator.Float.Adosc { fast_period; slow_period }))
  | S.Adx { timeperiod } -> FloatBAFlag (F (Indicator.Float.Adx { timeperiod }))
  | S.Adxr { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Adxr { timeperiod }))
  | S.Apo { fast_period; slow_period; ma_type } ->
    FloatBAFlag (F (Indicator.Float.Apo { fast_period; slow_period; ma_type }))
  | S.Aroon { timeperiod } ->
    Output.Flag.FloatBA2Flag
      ( F (Indicator.Float.Aroon_Down { timeperiod }),
        F (Indicator.Float.Aroon_Up { timeperiod }) )
  | S.Aroonosc { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.AroonOsc { timeperiod }))
  | S.Asin _ -> FloatBAFlag (F Indicator.Float.Asin)
  | S.Atan _ -> FloatBAFlag (F Indicator.Float.Atan)
  | S.Atr { timeperiod } -> FloatBAFlag (F (Indicator.Float.Atr { timeperiod }))
  | S.Avgprice _ -> FloatBAFlag (F Indicator.Float.AvgPrice)
  | S.Avgdev { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Avgdev { timeperiod }))
  | S.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
    Output.Flag.FloatBA3Flag
      ( F
          (Indicator.Float.UpperBBand
             { timeperiod; nb_dev_up; nb_dev_dn; ma_type }),
        F
          (Indicator.Float.MiddleBBand
             { timeperiod; nb_dev_up; nb_dev_dn; ma_type }),
        F
          (Indicator.Float.LowerBBand
             { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) )
  | S.Beta { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Beta { timeperiod }))
  | S.Bop _ -> FloatBAFlag (F Indicator.Float.Bop)
  | S.Cci { timeperiod } -> FloatBAFlag (F (Indicator.Float.Cci { timeperiod }))
  | S.Cdl2crows _ -> IntBAFlag (I Indicator.Int.Cdl2Crows)
  | S.Cdl3blackcrows _ -> IntBAFlag (I Indicator.Int.Cdl3BlackCrows)
  | S.Cdl3inside _ -> IntBAFlag (I Indicator.Int.Cdl3Inside)
  | S.Cdl3linestrike _ -> IntBAFlag (I Indicator.Int.Cdl3LineStrike)
  | S.Cdl3outside _ -> IntBAFlag (I Indicator.Int.Cdl3Outside)
  | S.Cdl3starsinsouth _ -> IntBAFlag (I Indicator.Int.Cdl3StarsInSouth)
  | S.Cdl3whitesoldiers _ -> IntBAFlag (I Indicator.Int.Cdl3WhiteSoldiers)
  | S.Cdlabandonedbaby { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlAbandonedBaby { penetration }))
  | S.Cdladvanceblock _ -> IntBAFlag (I Indicator.Int.CdlAdvanceBlock)
  | S.Cdlbelthold _ -> IntBAFlag (I Indicator.Int.CdlBeltHold)
  | S.Cdlbreakaway _ -> IntBAFlag (I Indicator.Int.CdlBreakaway)
  | S.Cdlclosingmarubozu _ -> IntBAFlag (I Indicator.Int.CdlClosingMarubozu)
  | S.Cdlconcealbabyswall _ -> IntBAFlag (I Indicator.Int.CdlConcealBabySwall)
  | S.Cdlcounterattack _ -> IntBAFlag (I Indicator.Int.CdlCounterAttack)
  | S.Cdldarkcloudcover { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlDarkCloudCover { penetration }))
  | S.Cdldoji _ -> IntBAFlag (I Indicator.Int.CdlDoji)
  | S.Cdldojistar _ -> IntBAFlag (I Indicator.Int.CdlDojiStar)
  | S.Cdldragonflydoji _ -> IntBAFlag (I Indicator.Int.CdlDragonflyDoji)
  | S.Cdlengulfing _ -> IntBAFlag (I Indicator.Int.CdlEngulfing)
  | S.Cdleveningdojistar { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlEveningDojiStar { penetration }))
  | S.Cdleveningstar { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlEveningStar { penetration }))
  | S.Cdlgapsidesidewhite _ -> IntBAFlag (I Indicator.Int.CdlGapSideSideWhite)
  | S.Cdlgravestonedoji _ -> IntBAFlag (I Indicator.Int.CdlGravestoneDoji)
  | S.Cdlhammer _ -> IntBAFlag (I Indicator.Int.CdlHammer)
  | S.Cdlhangingman _ -> IntBAFlag (I Indicator.Int.CdlHangingMan)
  | S.Cdlharami _ -> IntBAFlag (I Indicator.Int.CdlHarami)
  | S.Cdlharamicross _ -> IntBAFlag (I Indicator.Int.CdlHaramiCross)
  | S.Cdlhighwave _ -> IntBAFlag (I Indicator.Int.CdlHighWave)
  | S.Cdlhikkake _ -> IntBAFlag (I Indicator.Int.CdlHikkake)
  | S.Cdlhikkakemod _ -> IntBAFlag (I Indicator.Int.CdlHikkakeMod)
  | S.Cdlhomingpigeon _ -> IntBAFlag (I Indicator.Int.CdlHomingPigeon)
  | S.Cdlidentical3crows _ -> IntBAFlag (I Indicator.Int.CdlIdentical3Crows)
  | S.Cdlinneck _ -> IntBAFlag (I Indicator.Int.CdlInNeck)
  | S.Cdlinvertedhammer _ -> IntBAFlag (I Indicator.Int.CdlInvertedHammer)
  | S.Cdlkicking _ -> IntBAFlag (I Indicator.Int.CdlKicking)
  | S.Cdlkickingbylength _ -> IntBAFlag (I Indicator.Int.CdlKickingByLength)
  | S.Cdlladderbottom _ -> IntBAFlag (I Indicator.Int.CdlLadderBottom)
  | S.Cdllongleggeddoji _ -> IntBAFlag (I Indicator.Int.CdlLongLeggedDoji)
  | S.Cdllongline _ -> IntBAFlag (I Indicator.Int.CdlLongLine)
  | S.Cdlmarubozu _ -> IntBAFlag (I Indicator.Int.CdlMarubozu)
  | S.Cdlmatchinglow _ -> IntBAFlag (I Indicator.Int.CdlMatchingLow)
  | S.Cdlmathold { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlMatHold { penetration }))
  | S.Cdlmorningdojistar { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlMorningDojiStar { penetration }))
  | S.Cdlmorningstar { penetration } ->
    IntBAFlag (I (Indicator.Int.CdlMorningStar { penetration }))
  | S.Cdlonneck _ -> IntBAFlag (I Indicator.Int.CdlOnNeck)
  | S.Cdlpiercing _ -> IntBAFlag (I Indicator.Int.CdlPiercing)
  | S.Cdlrickshawman _ -> IntBAFlag (I Indicator.Int.CdlRickshawMan)
  | S.Cdlrisefall3methods _ -> IntBAFlag (I Indicator.Int.CdlRiseFall3Methods)
  | S.Cdlseparatinglines _ -> IntBAFlag (I Indicator.Int.CdlSeparatingLines)
  | S.Cdlshootingstar _ -> IntBAFlag (I Indicator.Int.CdlShootingStar)
  | S.Cdlshortline _ -> IntBAFlag (I Indicator.Int.CdlShortLine)
  | S.Cdlspinningtop _ -> IntBAFlag (I Indicator.Int.CdlSpinningTop)
  | S.Cdlstalledpattern _ -> IntBAFlag (I Indicator.Int.CdlStalledPattern)
  | S.Cdlsticksandwich _ -> IntBAFlag (I Indicator.Int.CdlStickSandwich)
  | S.Cdltakuri _ -> IntBAFlag (I Indicator.Int.CdlTakuri)
  | S.Cdltasukigap _ -> IntBAFlag (I Indicator.Int.CdlTasukiGap)
  | S.Cdlthrusting _ -> IntBAFlag (I Indicator.Int.CdlThrusting)
  | S.Cdltristar _ -> IntBAFlag (I Indicator.Int.CdlTristar)
  | S.Cdlunique3river _ -> IntBAFlag (I Indicator.Int.CdlUnique3River)
  | S.Cdlupsidegap2crows _ -> IntBAFlag (I Indicator.Int.CdlUpsideGap2Crows)
  | S.Cdlxsidegap3methods _ -> IntBAFlag (I Indicator.Int.CdlXSideGap3Methods)
  | S.Ceil _ -> FloatBAFlag (F Indicator.Float.Ceil)
  | S.Cmo { timeperiod } -> FloatBAFlag (F (Indicator.Float.Cmo { timeperiod }))
  | S.Correl { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Correl { timeperiod }))
  | S.Cos _ -> FloatBAFlag (F Indicator.Float.Cos)
  | S.Cosh _ -> FloatBAFlag (F Indicator.Float.Cosh)
  | S.Dema { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Dema { timeperiod }))
  | S.Div _ -> FloatBAFlag (F Indicator.Float.Div)
  | S.Dx { timeperiod } -> FloatBAFlag (F (Indicator.Float.Dx { timeperiod }))
  | S.Ema { timeperiod } -> FloatBAFlag (F (Indicator.Float.Ema { timeperiod }))
  | S.Exp _ -> FloatBAFlag (F Indicator.Float.Exp)
  | S.Floor _ -> FloatBAFlag (F Indicator.Float.Floor)
  | S.Ht_dcperiod _ -> FloatBAFlag (F Indicator.Float.HtDcPeriod)
  | S.Ht_dcphase _ -> FloatBAFlag (F Indicator.Float.HtDcPhase)
  | S.Ht_phasor _ ->
    Output.Flag.FloatBA2Flag
      (F Indicator.Float.HtPhasor_InPhase, F Indicator.Float.HtPhasor_Quadrature)
  | S.Ht_sine _ ->
    Output.Flag.FloatBA2Flag
      (F Indicator.Float.HtSine_Sine, F Indicator.Float.HtSine_LeadSine)
  | S.Ht_trendline _ -> FloatBAFlag (F Indicator.Float.HtTrendline)
  | S.Ht_trendmode _ -> IntBAFlag (I Indicator.Int.HtTrendMode)
  | S.Imi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Imi { timeperiod }))
  | S.Kama { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Kama { timeperiod }))
  | S.Linearreg { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Linearreg { timeperiod }))
  | S.Linearreg_angle { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.LinearregAngle { timeperiod }))
  | S.Linearreg_intercept { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.LinearregIntercept { timeperiod }))
  | S.Linearreg_slope { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.LinearregSlope { timeperiod }))
  | S.Ln _ -> FloatBAFlag (F Indicator.Float.Ln)
  | S.Log10 _ -> FloatBAFlag (F Indicator.Float.Log10)
  | S.Ma { timeperiod; ma_type } ->
    FloatBAFlag (F (Indicator.Float.Ma { timeperiod; ma_type }))
  | S.Macd { fast_period; slow_period; signal_period } ->
    Output.Flag.FloatBA3Flag
      ( F (Indicator.Float.Macd_MACD { fast_period; slow_period; signal_period }),
        F
          (Indicator.Float.Macd_MACDSignal
             { fast_period; slow_period; signal_period }),
        F
          (Indicator.Float.Macd_MACDHist
             { fast_period; slow_period; signal_period }) )
  | S.Macdext
      {
        fast_period;
        fast_ma_type;
        slow_period;
        slow_ma_type;
        signal_period;
        signal_ma_type;
      } ->
    Output.Flag.FloatBA3Flag
      ( F
          (Indicator.Float.MacdExt_MACD
             {
               fast_period;
               fast_ma_type;
               slow_period;
               slow_ma_type;
               signal_period;
               signal_ma_type;
             }),
        F
          (Indicator.Float.MacdExt_MACDSignal
             {
               fast_period;
               fast_ma_type;
               slow_period;
               slow_ma_type;
               signal_period;
               signal_ma_type;
             }),
        F
          (Indicator.Float.MacdExt_MACDHist
             {
               fast_period;
               fast_ma_type;
               slow_period;
               slow_ma_type;
               signal_period;
               signal_ma_type;
             }) )
  | S.Macdfix { signal_period } ->
    Output.Flag.FloatBA3Flag
      ( F (Indicator.Float.MacdFix_MACD { signal_period }),
        F (Indicator.Float.MacdFix_MACDSignal { signal_period }),
        F (Indicator.Float.MacdFix_MACDHist { signal_period }) )
  | S.Mama { fast_limit; slow_limit } ->
    Output.Flag.FloatBA2Flag
      ( F (Indicator.Float.Mama_MAMA { fast_limit; slow_limit }),
        F (Indicator.Float.Mama_FAMA { fast_limit; slow_limit }) )
  | S.Mavp { min_period; max_period; ma_type } ->
    FloatBAFlag (F (Indicator.Float.Mavp { min_period; max_period; ma_type }))
  | S.Max { timeperiod } -> FloatBAFlag (F (Indicator.Float.Max { timeperiod }))
  | S.Maxindex { timeperiod } ->
    IntBAFlag (I (Indicator.Int.MaxIndex { timeperiod }))
  | S.Medprice _ -> FloatBAFlag (F Indicator.Float.MedPrice)
  | S.Mfi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Mfi { timeperiod }))
  | S.Midpoint { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Midpoint { timeperiod }))
  | S.Midprice { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Midprice { timeperiod }))
  | S.Min { timeperiod } -> FloatBAFlag (F (Indicator.Float.Min { timeperiod }))
  | S.Minindex { timeperiod } ->
    IntBAFlag (I (Indicator.Int.MinIndex { timeperiod }))
  | S.Minmax { timeperiod } ->
    Output.Flag.FloatBA2Flag
      ( F (Indicator.Float.MinMax_Min { timeperiod }),
        F (Indicator.Float.MinMax_Max { timeperiod }) )
  | S.Minmaxindex { timeperiod } ->
    Output.Flag.IntBA2Flag
      ( I (Indicator.Int.MinMaxIndex_Min { timeperiod }),
        I (Indicator.Int.MinMaxIndex_Max { timeperiod }) )
  | S.Minus_di { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.MinusDI { timeperiod }))
  | S.Minus_dm { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.MinusDM { timeperiod }))
  | S.Mom { timeperiod } -> FloatBAFlag (F (Indicator.Float.Mom { timeperiod }))
  | S.Mult _ -> FloatBAFlag (F Indicator.Float.Mult)
  | S.Natr { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Natr { timeperiod }))
  | S.Obv _ -> FloatBAFlag (F Indicator.Float.Obv)
  | S.Plus_di { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.PlusDI { timeperiod }))
  | S.Plus_dm { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.PlusDM { timeperiod }))
  | S.Ppo { fast_period; slow_period; ma_type } ->
    FloatBAFlag (F (Indicator.Float.Ppo { fast_period; slow_period; ma_type }))
  | S.Roc { timeperiod } -> FloatBAFlag (F (Indicator.Float.Roc { timeperiod }))
  | S.Rocp { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Rocp { timeperiod }))
  | S.Rocr { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Rocr { timeperiod }))
  | S.Rocr100 { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Rocr100 { timeperiod }))
  | S.Rsi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Rsi { timeperiod }))
  | S.Sar { acceleration; maximum } ->
    FloatBAFlag (F (Indicator.Float.Sar { acceleration; maximum }))
  | S.Sarext
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
    FloatBAFlag
      (F
         (Indicator.Float.Sarext
            {
              start_value;
              offset_on_reverse;
              acceleration_init_long;
              acceleration_long;
              acceleration_max_long;
              acceleration_init_short;
              acceleration_short;
              acceleration_max_short;
            }))
  | S.Sin _ -> FloatBAFlag (F Indicator.Float.Sin)
  | S.Sinh _ -> FloatBAFlag (F Indicator.Float.Sinh)
  | S.Sma { timeperiod } -> FloatBAFlag (F (Indicator.Float.Sma { timeperiod }))
  | S.Sqrt _ -> FloatBAFlag (F Indicator.Float.Sqrt)
  | S.Stddev { timeperiod; nb_dev } ->
    FloatBAFlag (F (Indicator.Float.Stddev { timeperiod; nb_dev }))
  | S.Stoch
      {
        fast_k_period;
        slow_k_period;
        slow_k_ma_type;
        slow_d_period;
        slow_d_ma_type;
      } ->
    Output.Flag.FloatBA2Flag
      ( F
          (Indicator.Float.Stoch_SlowK
             {
               fast_k_period;
               slow_k_period;
               slow_k_ma_type;
               slow_d_period;
               slow_d_ma_type;
             }),
        F
          (Indicator.Float.Stoch_SlowD
             {
               fast_k_period;
               slow_k_period;
               slow_k_ma_type;
               slow_d_period;
               slow_d_ma_type;
             }) )
  | S.Stochf { fast_k_period; fast_d_period; fast_d_ma_type } ->
    Output.Flag.FloatBA2Flag
      ( F
          (Indicator.Float.StochF_FastK
             { fast_k_period; fast_d_period; fast_d_ma_type }),
        F
          (Indicator.Float.StochF_FastD
             { fast_k_period; fast_d_period; fast_d_ma_type }) )
  | S.Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
    Output.Flag.FloatBA2Flag
      ( F
          (Indicator.Float.StochRsi_FastK
             { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }),
        F
          (Indicator.Float.StochRsi_FastD
             { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }) )
  | S.Sub _ -> FloatBAFlag (F Indicator.Float.Sub)
  | S.Sum { timeperiod } -> FloatBAFlag (F (Indicator.Float.Sum { timeperiod }))
  | S.T3 { timeperiod; v_factor } ->
    FloatBAFlag (F (Indicator.Float.T3 { timeperiod; v_factor }))
  | S.Tan _ -> FloatBAFlag (F Indicator.Float.Tan)
  | S.Tanh _ -> FloatBAFlag (F Indicator.Float.Tanh)
  | S.Tema { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Tema { timeperiod }))
  | S.Trange _ -> FloatBAFlag (F Indicator.Float.Trange)
  | S.Trima { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Trima { timeperiod }))
  | S.Trix { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Trix { timeperiod }))
  | S.Tsf { timeperiod } -> FloatBAFlag (F (Indicator.Float.Tsf { timeperiod }))
  | S.Typprice _ -> FloatBAFlag (F Indicator.Float.TypPrice)
  | S.Ultosc { timeperiod1; timeperiod2; timeperiod3 } ->
    FloatBAFlag
      (F (Indicator.Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 }))
  | S.Var { timeperiod; nb_dev } ->
    FloatBAFlag (F (Indicator.Float.Var { timeperiod; nb_dev }))
  | S.Wclprice _ -> FloatBAFlag (F Indicator.Float.WclPrice)
  | S.Willr { timeperiod } ->
    FloatBAFlag (F (Indicator.Float.Willr { timeperiod }))
  | S.Wma { timeperiod } -> FloatBAFlag (F (Indicator.Float.Wma { timeperiod }))

let to_string (Pack x : t) =
  let str = Safe.to_string x in
  str

let pp = fun fmt (Pack x : t) -> Format.fprintf fmt "%a" Safe.pp x
let hash (Pack x : t) = Safe.hash x
let equal (Pack x : t) (Pack y : t) = Safe.equal x y
