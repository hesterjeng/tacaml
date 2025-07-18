open Safe

type t =
  | Ohlcv of Ohlcv.t
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba

module Flag = struct
  type t = OhlcvFlag | FloatBAFlag | FloatBA2Flag

  let of_pack : Pack.t -> t =
   fun (Pack wrapper_params) ->
    match wrapper_params with
    | Medprice _
    | Dema _
    | Accbands _
    | Ad _
    | Adosc _
    | Adx _
    | Adxr _
    | Apo _
    | Aroon _
    | Aroonosc _
    | Atr _
    | Avgprice _
    | Avgdev _
    | Bbands _
    | Bop _
    | Cci _
    | Cdl2crows _
    | Cdl3blackcrows _
    | Cdl3inside _
    | Cdl3linestrike _
    | Cdl3outside _
    | Cdl3starsinsouth _
    | Cdl3whitesoldiers _
    | Cdlabandonedbaby _
    | Cdladvanceblock _
    | Cdlbelthold _
    | Cdlbreakaway _
    | Cdlclosingmarubozu _
    | Cdlconcealbabyswall _
    | Cdlcounterattack _
    | Cdldarkcloudcover _
    | Cdldoji _
    | Cdldojistar _
    | Cdldragonflydoji _
    | Cdlengulfing _
    | Cdleveningdojistar _
    | Cdleveningstar _
    | Cdlgapsidesidewhite _
    | Cdlgravestonedoji _
    | Cdlhammer _
    | Cdlhangingman _
    | Cdlharami _
    | Cdlharamicross _
    | Cdlhighwave _
    | Cdlhikkake _
    | Cdlhikkakemod _
    | Cdlhomingpigeon _
    | Cdlidentical3crows _
    | Cdlinneck _
    | Cdlinvertedhammer _
    | Cdlkicking _
    | Cdlkickingbylength _
    | Cdlladderbottom _
    | Cdllongleggeddoji _
    | Cdllongline _
    | Cdlmarubozu _
    | Cdlmatchinglow _
    | Cdlmathold _
    | Cdlmorningdojistar _
    | Cdlmorningstar _
    | Cdlonneck _
    | Cdlpiercing _
    | Cdlrickshawman _
    | Cdlrisefall3methods _
    | Cdlseparatinglines _
    | Cdlshootingstar _
    | Cdlshortline _
    | Cdlspinningtop _
    | Cdlstalledpattern _
    | Cdlsticksandwich _
    | Cdltakuri _
    | Cdltasukigap _
    | Cdlthrusting _
    | Cdltristar _
    | Cdlunique3river _
    | Cdlupsidegap2crows _
    | Cdlxsidegap3methods _
    | Cmo _
    | Dx _
    | Ema _
    | Ht_dcperiod _
    | Ht_dcphase _
    | Ht_phasor _
    | Ht_sine _
    | Ht_trendline _
    | Ht_trendmode _
    | Imi _
    | Kama _
    | Linearreg _
    | Linearreg_angle _
    | Linearreg_intercept _
    | Linearreg_slope _
    | Ma _
    | Macd _
    | Macdext _
    | Macdfix _
    | Mama _
    | Mfi _
    | Midprice _
    | Minus_di _
    | Minus_dm _
    | Mom _
    | Natr _
    | Obv _
    | Plus_di _
    | Plus_dm _
    | Ppo _
    | Roc _
    | Rocp _
    | Rocr _
    | Rocr100 _
    | Rsi _
    | Sar _
    | Sarext _
    | Sma _
    | Stoch _
    | Stochf _
    | Stochrsi _
    | Sum _
    | T3 _
    | Tema _
    | Trange _
    | Trima _
    | Trix _
    | Tsf _
    | Typprice _
    | Ultosc _
    | Var _
    | Wclprice _
    | Willr _
    | Wma _ ->
      OhlcvFlag
    | Sin _
    | Sinh _
    | Acos _
    | Asin _
    | Atan _
    | Ceil _
    | Cos _
    | Cosh _
    | Exp _
    | Floor _
    | Ln _
    | Log10 _
    | Max _
    | Maxindex _
    | Midpoint _
    | Min _
    | Minindex _
    | Minmax _
    | Minmaxindex _
    | Sqrt _
    | Stddev _
    | Tan _
    | Tanh _ ->
      FloatBAFlag
    | Add _
    | Beta _
    | Correl _
    | Div _
    | Mavp _
    | Mult _
    | Sub _ ->
      FloatBA2Flag
end

let to_string = function
  | Ohlcv _ -> "Ohlcv"
  | FloatBA _ -> "FloatBA"
  | FloatBA2 _ -> "FloatBA2"
