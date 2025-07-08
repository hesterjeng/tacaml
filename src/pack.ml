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
      (`Fatal "Invalid input or output source/destinations chosen in TA-Lib")
