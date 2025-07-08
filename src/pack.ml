type t = Pack : ('a, 'b) Wrappers.t -> t

let pack x = Pack x

let calculate ?i (Pack params) (source : Input_source.t)
    (output : Output_destination.t) =
  match (params, source, output) with
  | ( Wrappers.Accbands _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA3 (x, y, z) ) ->
    Wrappers.calculate ?i params source (x, y, z)
  | ( Wrappers.Acos _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Ad _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Add _,
      Input_source.FloatBA2 (s1, s2),
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params (s1, s2) output
  | ( Wrappers.Adosc _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Adx _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Adxr _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Apo _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Aroon _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA2 (o1, o2) ) ->
    Wrappers.calculate ?i params source (o1, o2)
  | ( Wrappers.Aroonosc _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Asin _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Atan _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Atr _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Avgprice _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Avgdev _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Bbands _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA3 (o1, o2, o3) ) ->
    Wrappers.calculate ?i params source (o1, o2, o3)
  | ( Wrappers.Beta _,
      Input_source.FloatBA2 (s1, s2),
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params (s1, s2) output
  | Wrappers.Bop _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Cci _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl2crows _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3blackcrows _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3inside _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3linestrike _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3outside _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3starsinsouth _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdl3whitesoldiers _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlabandonedbaby _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdladvanceblock _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlbelthold _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlbreakaway _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlclosingmarubozu _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlconcealbabyswall _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlcounterattack _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdldarkcloudcover _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdldoji _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdldojistar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdldragonflydoji _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlengulfing _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdleveningdojistar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdleveningstar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlgapsidesidewhite _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlgravestonedoji _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhammer _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhangingman _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlharami _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlharamicross _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhighwave _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhikkake _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhikkakemod _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlhomingpigeon _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlidentical3crows _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlinneck _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlinvertedhammer _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlkicking _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlkickingbylength _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlladderbottom _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdllongleggeddoji _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdllongline _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlmarubozu _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlmatchinglow _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlmathold _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlmorningdojistar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlmorningstar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlonneck _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlpiercing _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlrickshawman _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlrisefall3methods _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlseparatinglines _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlshootingstar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlshortline _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlspinningtop _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlstalledpattern _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlsticksandwich _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdltakuri _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdltasukigap _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlthrusting _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdltristar _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlunique3river _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlupsidegap2crows _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cdlxsidegap3methods _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ceil _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Cmo _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Correl _, FloatBA2 (s0, s1), Output_destination.FloatBA output ->
    Wrappers.calculate ?i params (s0, s1) output
  | ( Wrappers.Cos _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Cosh _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Dema _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Div _, FloatBA2 (s0, s1), Output_destination.FloatBA output ->
    Wrappers.calculate ?i params (s0, s1) output
  | Wrappers.Dx _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Ema _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Exp _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Floor _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ht_dcperiod _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ht_dcphase _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Ht_phasor _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | Wrappers.Ht_sine _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | ( Wrappers.Ht_trendline _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ht_trendmode _,
      Input_source.Ohlcv source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Imi _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Kama _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Linearreg _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Linearreg_angle _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Linearreg_intercept _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Linearreg_slope _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ln _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Log10 _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Ma _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Macd _, Input_source.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    Wrappers.calculate ?i params source (o0, o1, o2)
  | Wrappers.Macdext _, Input_source.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    Wrappers.calculate ?i params source (o0, o1, o2)
  | Wrappers.Macdfix _, Input_source.Ohlcv source, FloatBA3 (o0, o1, o2) ->
    Wrappers.calculate ?i params source (o0, o1, o2)
  | Wrappers.Mama _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | Wrappers.Mavp _, FloatBA2 (s0, s1), Output_destination.FloatBA output ->
    Wrappers.calculate ?i params (s0, s1) output
  | ( Wrappers.Max _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Maxindex _,
      Input_source.FloatBA source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Medprice _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Mfi _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Midpoint _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Midprice _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Min _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Minindex _,
      Input_source.FloatBA source,
      Output_destination.IntBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Minmax _, Input_source.FloatBA source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | ( Wrappers.Minmaxindex _,
      Input_source.FloatBA source,
      Output_destination.IntBA2 (o0, o1) ) ->
    Wrappers.calculate ?i params source (o0, o1)
  | ( Wrappers.Minus_di _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Minus_dm _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Mom _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Mult _, FloatBA2 (s0, s1), Output_destination.FloatBA output ->
    Wrappers.calculate ?i params (s0, s1) output
  | ( Wrappers.Natr _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Obv _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Plus_di _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Plus_dm _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Ppo _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Roc _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Rocp _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Rocr _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Rocr100 _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Rsi _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.Sar _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Sarext _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Sin _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Sinh _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Sma _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Sqrt _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Stddev _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Stoch _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | Wrappers.Stochf _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | Wrappers.Stochrsi _, Input_source.Ohlcv source, FloatBA2 (o0, o1) ->
    Wrappers.calculate ?i params source (o0, o1)
  | Wrappers.Sub _, FloatBA2 (s0, s1), Output_destination.FloatBA output ->
    Wrappers.calculate ?i params (s0, s1) output
  | Wrappers.Sum _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | Wrappers.T3 _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Tan _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Tanh _,
      Input_source.FloatBA source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Tema _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Trange _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Trima _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Trix _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Tsf _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Typprice _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Ultosc _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Var _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Wclprice _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | ( Wrappers.Willr _,
      Input_source.Ohlcv source,
      Output_destination.FloatBA output ) ->
    Wrappers.calculate ?i params source output
  | Wrappers.Wma _, Input_source.Ohlcv source, Output_destination.FloatBA output
    ->
    Wrappers.calculate ?i params source output
  | _ ->
    Error
      (`FatalError
         "Invalid input or output source/destinations chosen in TA-Lib")

let input_flag_from_wrapper : t -> Input_source.Flag.t =
 fun (Pack wrapper_params) ->
  match wrapper_params with
  | Wrappers.Medprice _
  | Wrappers.Dema _
  | Wrappers.Accbands _
  | Wrappers.Ad _
  | Wrappers.Adosc _
  | Wrappers.Adx _
  | Wrappers.Adxr _
  | Wrappers.Apo _
  | Wrappers.Aroon _
  | Wrappers.Aroonosc _
  | Wrappers.Atr _
  | Wrappers.Avgprice _
  | Wrappers.Avgdev _
  | Wrappers.Bbands _
  | Wrappers.Bop _
  | Wrappers.Cci _
  | Wrappers.Cdl2crows _
  | Wrappers.Cdl3blackcrows _
  | Wrappers.Cdl3inside _
  | Wrappers.Cdl3linestrike _
  | Wrappers.Cdl3outside _
  | Wrappers.Cdl3starsinsouth _
  | Wrappers.Cdl3whitesoldiers _
  | Wrappers.Cdlabandonedbaby _
  | Wrappers.Cdladvanceblock _
  | Wrappers.Cdlbelthold _
  | Wrappers.Cdlbreakaway _
  | Wrappers.Cdlclosingmarubozu _
  | Wrappers.Cdlconcealbabyswall _
  | Wrappers.Cdlcounterattack _
  | Wrappers.Cdldarkcloudcover _
  | Wrappers.Cdldoji _
  | Wrappers.Cdldojistar _
  | Wrappers.Cdldragonflydoji _
  | Wrappers.Cdlengulfing _
  | Wrappers.Cdleveningdojistar _
  | Wrappers.Cdleveningstar _
  | Wrappers.Cdlgapsidesidewhite _
  | Wrappers.Cdlgravestonedoji _
  | Wrappers.Cdlhammer _
  | Wrappers.Cdlhangingman _
  | Wrappers.Cdlharami _
  | Wrappers.Cdlharamicross _
  | Wrappers.Cdlhighwave _
  | Wrappers.Cdlhikkake _
  | Wrappers.Cdlhikkakemod _
  | Wrappers.Cdlhomingpigeon _
  | Wrappers.Cdlidentical3crows _
  | Wrappers.Cdlinneck _
  | Wrappers.Cdlinvertedhammer _
  | Wrappers.Cdlkicking _
  | Wrappers.Cdlkickingbylength _
  | Wrappers.Cdlladderbottom _
  | Wrappers.Cdllongleggeddoji _
  | Wrappers.Cdllongline _
  | Wrappers.Cdlmarubozu _
  | Wrappers.Cdlmatchinglow _
  | Wrappers.Cdlmathold _
  | Wrappers.Cdlmorningdojistar _
  | Wrappers.Cdlmorningstar _
  | Wrappers.Cdlonneck _
  | Wrappers.Cdlpiercing _
  | Wrappers.Cdlrickshawman _
  | Wrappers.Cdlrisefall3methods _
  | Wrappers.Cdlseparatinglines _
  | Wrappers.Cdlshootingstar _
  | Wrappers.Cdlshortline _
  | Wrappers.Cdlspinningtop _
  | Wrappers.Cdlstalledpattern _
  | Wrappers.Cdlsticksandwich _
  | Wrappers.Cdltakuri _
  | Wrappers.Cdltasukigap _
  | Wrappers.Cdlthrusting _
  | Wrappers.Cdltristar _
  | Wrappers.Cdlunique3river _
  | Wrappers.Cdlupsidegap2crows _
  | Wrappers.Cdlxsidegap3methods _
  | Wrappers.Cmo _
  | Wrappers.Dx _
  | Wrappers.Ema _
  | Wrappers.Ht_dcperiod _
  | Wrappers.Ht_dcphase _
  | Wrappers.Ht_phasor _
  | Wrappers.Ht_sine _
  | Wrappers.Ht_trendline _
  | Wrappers.Ht_trendmode _
  | Wrappers.Imi _
  | Wrappers.Kama _
  | Wrappers.Linearreg _
  | Wrappers.Linearreg_angle _
  | Wrappers.Linearreg_intercept _
  | Wrappers.Linearreg_slope _
  | Wrappers.Ma _
  | Wrappers.Macd _
  | Wrappers.Macdext _
  | Wrappers.Macdfix _
  | Wrappers.Mama _
  | Wrappers.Mfi _
  | Wrappers.Midprice _
  | Wrappers.Minus_di _
  | Wrappers.Minus_dm _
  | Wrappers.Mom _
  | Wrappers.Natr _
  | Wrappers.Obv _
  | Wrappers.Plus_di _
  | Wrappers.Plus_dm _
  | Wrappers.Ppo _
  | Wrappers.Roc _
  | Wrappers.Rocp _
  | Wrappers.Rocr _
  | Wrappers.Rocr100 _
  | Wrappers.Rsi _
  | Wrappers.Sar _
  | Wrappers.Sarext _
  | Wrappers.Sma _
  | Wrappers.Stoch _
  | Wrappers.Stochf _
  | Wrappers.Stochrsi _
  | Wrappers.Sum _
  | Wrappers.T3 _
  | Wrappers.Tema _
  | Wrappers.Trange _
  | Wrappers.Trima _
  | Wrappers.Trix _
  | Wrappers.Tsf _
  | Wrappers.Typprice _
  | Wrappers.Ultosc _
  | Wrappers.Var _
  | Wrappers.Wclprice _
  | Wrappers.Willr _
  | Wrappers.Wma _ ->
    OhlcvFlag
  | Wrappers.Sin _
  | Wrappers.Sinh _
  | Wrappers.Acos _
  | Wrappers.Asin _
  | Wrappers.Atan _
  | Wrappers.Ceil _
  | Wrappers.Cos _
  | Wrappers.Cosh _
  | Wrappers.Exp _
  | Wrappers.Floor _
  | Wrappers.Ln _
  | Wrappers.Log10 _
  | Wrappers.Max _
  | Wrappers.Maxindex _
  | Wrappers.Midpoint _
  | Wrappers.Min _
  | Wrappers.Minindex _
  | Wrappers.Minmax _
  | Wrappers.Minmaxindex _
  | Wrappers.Sqrt _
  | Wrappers.Stddev _
  | Wrappers.Tan _
  | Wrappers.Tanh _ ->
    FloatBAFlag
  | Wrappers.Add _
  | Wrappers.Beta _
  | Wrappers.Correl _
  | Wrappers.Div _
  | Wrappers.Mavp _
  | Wrappers.Mult _
  | Wrappers.Sub _ ->
    FloatBA2Flag

let output_flag_from_wrapper : t -> Output_destination.Flag.t =
 fun (Pack wrapper_params) ->
  match wrapper_params with
  | Wrappers.Accbands _ ->
    Output_destination.Flag.FloatBA3Flag
      ( F Type.Float.UpperBBand,
        F Type.Float.MiddleBBand,
        F Type.Float.LowerBBand )
  | Wrappers.Acos _ -> FloatBAFlag (F Type.Float.Acos)
  | Wrappers.Ad _ -> FloatBAFlag (F Type.Float.Ad)
  | Wrappers.Add _ -> FloatBAFlag (F Type.Float.Add)
  | Wrappers.Adosc _ -> FloatBAFlag (F Type.Float.Adosc)
  | Wrappers.Adx _ -> FloatBAFlag (F Type.Float.Adx)
  | Wrappers.Adxr _ -> FloatBAFlag (F Type.Float.Adxr)
  | Wrappers.Apo _ -> FloatBAFlag (F Type.Float.Apo)
  | Wrappers.Aroon _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.Aroon_Down, F Type.Float.Aroon_Up)
  | Wrappers.Aroonosc _ -> FloatBAFlag (F Type.Float.AroonOsc)
  | Wrappers.Asin _ -> FloatBAFlag (F Type.Float.Asin)
  | Wrappers.Atan _ -> FloatBAFlag (F Type.Float.Atan)
  | Wrappers.Atr _ -> FloatBAFlag (F Type.Float.Atr)
  | Wrappers.Avgprice _ -> FloatBAFlag (F Type.Float.AvgPrice)
  | Wrappers.Avgdev _ -> FloatBAFlag (F Type.Float.Stddev)
  | Wrappers.Bbands _ ->
    Output_destination.Flag.FloatBA3Flag
      ( F Type.Float.UpperBBand,
        F Type.Float.MiddleBBand,
        F Type.Float.LowerBBand )
  | Wrappers.Beta _ -> FloatBAFlag (F Type.Float.Beta)
  | Wrappers.Bop _ -> FloatBAFlag (F Type.Float.Willr)
  | Wrappers.Cci _ -> FloatBAFlag (F Type.Float.Cci)
  | Wrappers.Cdl2crows _ -> IntBAFlag (I Type.Int.Cdl2Crows)
  | Wrappers.Cdl3blackcrows _ -> IntBAFlag (I Type.Int.Cdl3BlackCrows)
  | Wrappers.Cdl3inside _ -> IntBAFlag (I Type.Int.Cdl3Inside)
  | Wrappers.Cdl3linestrike _ -> IntBAFlag (I Type.Int.Cdl3LineStrike)
  | Wrappers.Cdl3outside _ -> IntBAFlag (I Type.Int.Cdl3Outside)
  | Wrappers.Cdl3starsinsouth _ -> IntBAFlag (I Type.Int.Cdl3StarsInSouth)
  | Wrappers.Cdl3whitesoldiers _ -> IntBAFlag (I Type.Int.Cdl3WhiteSoldiers)
  | Wrappers.Cdlabandonedbaby _ -> IntBAFlag (I Type.Int.CdlAbandonedBaby)
  | Wrappers.Cdladvanceblock _ -> IntBAFlag (I Type.Int.CdlAdvanceBlock)
  | Wrappers.Cdlbelthold _ -> IntBAFlag (I Type.Int.CdlBeltHold)
  | Wrappers.Cdlbreakaway _ -> IntBAFlag (I Type.Int.CdlBreakaway)
  | Wrappers.Cdlclosingmarubozu _ -> IntBAFlag (I Type.Int.CdlClosingMarubozu)
  | Wrappers.Cdlconcealbabyswall _ -> IntBAFlag (I Type.Int.CdlConcealBabySwall)
  | Wrappers.Cdlcounterattack _ -> IntBAFlag (I Type.Int.CdlCounterAttack)
  | Wrappers.Cdldarkcloudcover _ -> IntBAFlag (I Type.Int.CdlDarkCloudCover)
  | Wrappers.Cdldoji _ -> IntBAFlag (I Type.Int.CdlDoji)
  | Wrappers.Cdldojistar _ -> IntBAFlag (I Type.Int.CdlDojiStar)
  | Wrappers.Cdldragonflydoji _ -> IntBAFlag (I Type.Int.CdlDragonflyDoji)
  | Wrappers.Cdlengulfing _ -> IntBAFlag (I Type.Int.CdlEngulfing)
  | Wrappers.Cdleveningdojistar _ -> IntBAFlag (I Type.Int.CdlEveningDojiStar)
  | Wrappers.Cdleveningstar _ -> IntBAFlag (I Type.Int.CdlEveningStar)
  | Wrappers.Cdlgapsidesidewhite _ -> IntBAFlag (I Type.Int.CdlGapSideSideWhite)
  | Wrappers.Cdlgravestonedoji _ -> IntBAFlag (I Type.Int.CdlGravestoneDoji)
  | Wrappers.Cdlhammer _ -> IntBAFlag (I Type.Int.CdlHammer)
  | Wrappers.Cdlhangingman _ -> IntBAFlag (I Type.Int.CdlHangingMan)
  | Wrappers.Cdlharami _ -> IntBAFlag (I Type.Int.CdlHarami)
  | Wrappers.Cdlharamicross _ -> IntBAFlag (I Type.Int.CdlHaramiCross)
  | Wrappers.Cdlhighwave _ -> IntBAFlag (I Type.Int.CdlHighWave)
  | Wrappers.Cdlhikkake _ -> IntBAFlag (I Type.Int.CdlHikkake)
  | Wrappers.Cdlhikkakemod _ -> IntBAFlag (I Type.Int.CdlHikkakeMod)
  | Wrappers.Cdlhomingpigeon _ -> IntBAFlag (I Type.Int.CdlHomingPigeon)
  | Wrappers.Cdlidentical3crows _ -> IntBAFlag (I Type.Int.CdlIdentical3Crows)
  | Wrappers.Cdlinneck _ -> IntBAFlag (I Type.Int.CdlInNeck)
  | Wrappers.Cdlinvertedhammer _ -> IntBAFlag (I Type.Int.CdlInvertedHammer)
  | Wrappers.Cdlkicking _ -> IntBAFlag (I Type.Int.CdlKicking)
  | Wrappers.Cdlkickingbylength _ -> IntBAFlag (I Type.Int.CdlKickingByLength)
  | Wrappers.Cdlladderbottom _ -> IntBAFlag (I Type.Int.CdlLadderBottom)
  | Wrappers.Cdllongleggeddoji _ -> IntBAFlag (I Type.Int.CdlLongLeggedDoji)
  | Wrappers.Cdllongline _ -> IntBAFlag (I Type.Int.CdlLongLine)
  | Wrappers.Cdlmarubozu _ -> IntBAFlag (I Type.Int.CdlMarubozu)
  | Wrappers.Cdlmatchinglow _ -> IntBAFlag (I Type.Int.CdlMatchingLow)
  | Wrappers.Cdlmathold _ -> IntBAFlag (I Type.Int.CdlMatHold)
  | Wrappers.Cdlmorningdojistar _ -> IntBAFlag (I Type.Int.CdlMorningDojiStar)
  | Wrappers.Cdlmorningstar _ -> IntBAFlag (I Type.Int.CdlMorningStar)
  | Wrappers.Cdlonneck _ -> IntBAFlag (I Type.Int.CdlOnNeck)
  | Wrappers.Cdlpiercing _ -> IntBAFlag (I Type.Int.CdlPiercing)
  | Wrappers.Cdlrickshawman _ -> IntBAFlag (I Type.Int.CdlRickshawMan)
  | Wrappers.Cdlrisefall3methods _ -> IntBAFlag (I Type.Int.CdlRiseFall3Methods)
  | Wrappers.Cdlseparatinglines _ -> IntBAFlag (I Type.Int.CdlSeparatingLines)
  | Wrappers.Cdlshootingstar _ -> IntBAFlag (I Type.Int.CdlShootingStar)
  | Wrappers.Cdlshortline _ -> IntBAFlag (I Type.Int.CdlShortLine)
  | Wrappers.Cdlspinningtop _ -> IntBAFlag (I Type.Int.CdlSpinningTop)
  | Wrappers.Cdlstalledpattern _ -> IntBAFlag (I Type.Int.CdlStalledPattern)
  | Wrappers.Cdlsticksandwich _ -> IntBAFlag (I Type.Int.CdlStickSandwich)
  | Wrappers.Cdltakuri _ -> IntBAFlag (I Type.Int.CdlTakuri)
  | Wrappers.Cdltasukigap _ -> IntBAFlag (I Type.Int.CdlTasukiGap)
  | Wrappers.Cdlthrusting _ -> IntBAFlag (I Type.Int.CdlThrusting)
  | Wrappers.Cdltristar _ -> IntBAFlag (I Type.Int.CdlTristar)
  | Wrappers.Cdlunique3river _ -> IntBAFlag (I Type.Int.CdlUnique3River)
  | Wrappers.Cdlupsidegap2crows _ -> IntBAFlag (I Type.Int.CdlUpsideGap2Crows)
  | Wrappers.Cdlxsidegap3methods _ -> IntBAFlag (I Type.Int.CdlXSideGap3Methods)
  | Wrappers.Ceil _ -> FloatBAFlag (F Type.Float.Ceil)
  | Wrappers.Cmo _ -> FloatBAFlag (F Type.Float.Cmo)
  | Wrappers.Correl _ -> FloatBAFlag (F Type.Float.Correl)
  | Wrappers.Cos _ -> FloatBAFlag (F Type.Float.Cos)
  | Wrappers.Cosh _ -> FloatBAFlag (F Type.Float.Cosh)
  | Wrappers.Dema _ -> FloatBAFlag (F Type.Float.Dema)
  | Wrappers.Div _ -> FloatBAFlag (F Type.Float.Div)
  | Wrappers.Dx _ -> FloatBAFlag (F Type.Float.Dx)
  | Wrappers.Ema _ -> FloatBAFlag (F Type.Float.Ema)
  | Wrappers.Exp _ -> FloatBAFlag (F Type.Float.Exp)
  | Wrappers.Floor _ -> FloatBAFlag (F Type.Float.Floor)
  | Wrappers.Ht_dcperiod _ -> FloatBAFlag (F Type.Float.HtDcPeriod)
  | Wrappers.Ht_dcphase _ -> FloatBAFlag (F Type.Float.HtDcPhase)
  | Wrappers.Ht_phasor _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.HtPhasor_InPhase, F Type.Float.HtPhasor_Quadrature)
  | Wrappers.Ht_sine _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.HtSine_Sine, F Type.Float.HtSine_LeadSine)
  | Wrappers.Ht_trendline _ -> FloatBAFlag (F Type.Float.HtTrendline)
  | Wrappers.Ht_trendmode _ -> FloatBAFlag (F Type.Float.HtTrendMode)
  | Wrappers.Imi _ -> FloatBAFlag (F Type.Float.Mom)
  | Wrappers.Kama _ -> FloatBAFlag (F Type.Float.Kama)
  | Wrappers.Linearreg _ -> FloatBAFlag (F Type.Float.Linearreg)
  | Wrappers.Linearreg_angle _ -> FloatBAFlag (F Type.Float.LinearregAngle)
  | Wrappers.Linearreg_intercept _ ->
    FloatBAFlag (F Type.Float.LinearregIntercept)
  | Wrappers.Linearreg_slope _ -> FloatBAFlag (F Type.Float.LinearregSlope)
  | Wrappers.Ln _ -> FloatBAFlag (F Type.Float.Ln)
  | Wrappers.Log10 _ -> FloatBAFlag (F Type.Float.Log10)
  | Wrappers.Ma _ -> FloatBAFlag (F Type.Float.Ma)
  | Wrappers.Macd _ ->
    Output_destination.Flag.FloatBA3Flag
      ( F Type.Float.Macd_MACD,
        F Type.Float.Macd_MACDSignal,
        F Type.Float.Macd_MACDHist )
  | Wrappers.Macdext _ ->
    Output_destination.Flag.FloatBA3Flag
      ( F Type.Float.MacdExt_MACD,
        F Type.Float.MacdExt_MACDSignal,
        F Type.Float.MacdExt_MACDHist )
  | Wrappers.Macdfix _ ->
    Output_destination.Flag.FloatBA3Flag
      ( F Type.Float.MacdFix_MACD,
        F Type.Float.MacdFix_MACDSignal,
        F Type.Float.MacdFix_MACDHist )
  | Wrappers.Mama _ ->
    Output_destination.Flag.FloatBA2Flag (F Type.Float.Mama, F Type.Float.Mama)
  | Wrappers.Mavp _ -> FloatBAFlag (F Type.Float.Mavp)
  | Wrappers.Max _ -> FloatBAFlag (F Type.Float.Max)
  | Wrappers.Maxindex _ -> IntBAFlag (I Type.Int.MaxIndex)
  | Wrappers.Medprice _ -> FloatBAFlag (F Type.Float.MedPrice)
  | Wrappers.Mfi _ -> FloatBAFlag (F Type.Float.Mfi)
  | Wrappers.Midpoint _ -> FloatBAFlag (F Type.Float.Midpoint)
  | Wrappers.Midprice _ -> FloatBAFlag (F Type.Float.Midprice)
  | Wrappers.Min _ -> FloatBAFlag (F Type.Float.Min)
  | Wrappers.Minindex _ -> IntBAFlag (I Type.Int.MinIndex)
  | Wrappers.Minmax _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.MinMax_Min, F Type.Float.MinMax_Max)
  | Wrappers.Minmaxindex _ ->
    Output_destination.Flag.IntBA2Flag
      (I Type.Int.MinMaxIndex_Min, I Type.Int.MinMaxIndex_Max)
  | Wrappers.Minus_di _ -> FloatBAFlag (F Type.Float.MinusDI)
  | Wrappers.Minus_dm _ -> FloatBAFlag (F Type.Float.MinusDM)
  | Wrappers.Mom _ -> FloatBAFlag (F Type.Float.Mom)
  | Wrappers.Mult _ -> FloatBAFlag (F Type.Float.Mult)
  | Wrappers.Natr _ -> FloatBAFlag (F Type.Float.Natr)
  | Wrappers.Obv _ -> FloatBAFlag (F Type.Float.Obv)
  | Wrappers.Plus_di _ -> FloatBAFlag (F Type.Float.PlusDI)
  | Wrappers.Plus_dm _ -> FloatBAFlag (F Type.Float.PlusDM)
  | Wrappers.Ppo _ -> FloatBAFlag (F Type.Float.Ppo)
  | Wrappers.Roc _ -> FloatBAFlag (F Type.Float.Roc)
  | Wrappers.Rocp _ -> FloatBAFlag (F Type.Float.Rocp)
  | Wrappers.Rocr _ -> FloatBAFlag (F Type.Float.Rocr)
  | Wrappers.Rocr100 _ -> FloatBAFlag (F Type.Float.Rocr100)
  | Wrappers.Rsi _ -> FloatBAFlag (F Type.Float.Rsi)
  | Wrappers.Sar _ -> FloatBAFlag (F Type.Float.Sar)
  | Wrappers.Sarext _ -> FloatBAFlag (F Type.Float.Sarext)
  | Wrappers.Sin _ -> FloatBAFlag (F Type.Float.Sin)
  | Wrappers.Sinh _ -> FloatBAFlag (F Type.Float.Sinh)
  | Wrappers.Sma _ -> FloatBAFlag (F Type.Float.Sma)
  | Wrappers.Sqrt _ -> FloatBAFlag (F Type.Float.Sqrt)
  | Wrappers.Stddev _ -> FloatBAFlag (F Type.Float.Stddev)
  | Wrappers.Stoch _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.Stoch_SlowK, F Type.Float.Stoch_SlowD)
  | Wrappers.Stochf _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.StochF_FastK, F Type.Float.StochF_FastD)
  | Wrappers.Stochrsi _ ->
    Output_destination.Flag.FloatBA2Flag
      (F Type.Float.StochRsi_FastK, F Type.Float.StochRsi_FastD)
  | Wrappers.Sub _ -> FloatBAFlag (F Type.Float.Sub)
  | Wrappers.Sum _ -> FloatBAFlag (F Type.Float.Sum)
  | Wrappers.T3 _ -> FloatBAFlag (F Type.Float.T3)
  | Wrappers.Tan _ -> FloatBAFlag (F Type.Float.Tan)
  | Wrappers.Tanh _ -> FloatBAFlag (F Type.Float.Tanh)
  | Wrappers.Tema _ -> FloatBAFlag (F Type.Float.Tema)
  | Wrappers.Trange _ -> FloatBAFlag (F Type.Float.Trange)
  | Wrappers.Trima _ -> FloatBAFlag (F Type.Float.Trima)
  | Wrappers.Trix _ -> FloatBAFlag (F Type.Float.Trix)
  | Wrappers.Tsf _ -> FloatBAFlag (F Type.Float.Tsf)
  | Wrappers.Typprice _ -> FloatBAFlag (F Type.Float.TypPrice)
  | Wrappers.Ultosc _ -> FloatBAFlag (F Type.Float.Ultosc)
  | Wrappers.Var _ -> FloatBAFlag (F Type.Float.Var)
  | Wrappers.Wclprice _ -> FloatBAFlag (F Type.Float.WclPrice)
  | Wrappers.Willr _ -> FloatBAFlag (F Type.Float.Willr)
  | Wrappers.Wma _ -> FloatBAFlag (F Type.Float.Wma)
