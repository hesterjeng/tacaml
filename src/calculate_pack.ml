open Pack

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
