open Wrappers

type t = Ohlcv | FloatBA | FloatBA2

let get_input_descriptor : type a b. (a, b) Wrappers.t -> t =
 fun params ->
  match params with
  | Accbands _ -> Ohlcv
  | Acos _ -> FloatBA
  | Ad _ -> Ohlcv
  | Add _ -> FloatBA2
  | Adosc _ -> Ohlcv
  | Adx _ -> Ohlcv
  | Adxr _ -> Ohlcv
  | Apo _ -> FloatBA
  | Aroon _ -> Ohlcv
  | Aroonosc _ -> Ohlcv
  | Asin _ -> FloatBA
  | Atan _ -> FloatBA
  | Atr _ -> Ohlcv
  | Avgprice _ -> Ohlcv
  | Avgdev _ -> FloatBA
  | Bbands _ -> FloatBA
  | Beta _ -> FloatBA2
  | Bop _ -> Ohlcv
  | Cci _ -> Ohlcv
  | Cdl2crows _ -> Ohlcv
  | Cdl3blackcrows _ -> Ohlcv
  | Cdl3inside _ -> Ohlcv
  | Cdl3linestrike _ -> Ohlcv
  | Cdl3outside _ -> Ohlcv
  | Cdl3starsinsouth _ -> Ohlcv
  | Cdl3whitesoldiers _ -> Ohlcv
  | Cdlabandonedbaby _ -> Ohlcv
  | Cdladvanceblock _ -> Ohlcv
  | Cdlbelthold _ -> Ohlcv
  | Cdlbreakaway _ -> Ohlcv
  | Cdlclosingmarubozu _ -> Ohlcv
  | Cdlconcealbabyswall _ -> Ohlcv
  | Cdlcounterattack _ -> Ohlcv
  | Cdldarkcloudcover _ -> Ohlcv
  | Cdldoji _ -> Ohlcv
  | Cdldojistar _ -> Ohlcv
  | Cdldragonflydoji _ -> Ohlcv
  | Cdlengulfing _ -> Ohlcv
  | Cdleveningdojistar _ -> Ohlcv
  | Cdleveningstar _ -> Ohlcv
  | Cdlgapsidesidewhite _ -> Ohlcv
  | Cdlgravestonedoji _ -> Ohlcv
  | Cdlhammer _ -> Ohlcv
  | Cdlhangingman _ -> Ohlcv
  | Cdlharami _ -> Ohlcv
  | Cdlharamicross _ -> Ohlcv
  | Cdlhighwave _ -> Ohlcv
  | Cdlhikkake _ -> Ohlcv
  | Cdlhikkakemod _ -> Ohlcv
  | Cdlhomingpigeon _ -> Ohlcv
  | Cdlidentical3crows _ -> Ohlcv
  | Cdlinneck _ -> Ohlcv
  | Cdlinvertedhammer _ -> Ohlcv
  | Cdlkicking _ -> Ohlcv
  | Cdlkickingbylength _ -> Ohlcv
  | Cdlladderbottom _ -> Ohlcv
  | Cdllongleggeddoji _ -> Ohlcv
  | Cdllongline _ -> Ohlcv
  | Cdlmarubozu _ -> Ohlcv
  | Cdlmatchinglow _ -> Ohlcv
  | Cdlmathold _ -> Ohlcv
  | Cdlmorningdojistar _ -> Ohlcv
  | Cdlmorningstar _ -> Ohlcv
  | Cdlonneck _ -> Ohlcv
  | Cdlpiercing _ -> Ohlcv
  | Cdlrickshawman _ -> Ohlcv
  | Cdlrisefall3methods _ -> Ohlcv
  | Cdlseparatinglines _ -> Ohlcv
  | Cdlshootingstar _ -> Ohlcv
  | Cdlshortline _ -> Ohlcv
  | Cdlspinningtop _ -> Ohlcv
  | Cdlstalledpattern _ -> Ohlcv
  | Cdlsticksandwich _ -> Ohlcv
  | Cdltakuri _ -> Ohlcv
  | Cdltasukigap _ -> Ohlcv
  | Cdlthrusting _ -> Ohlcv
  | Cdltristar _ -> Ohlcv
  | Cdlunique3river _ -> Ohlcv
  | Cdlupsidegap2crows _ -> Ohlcv
  | Cdlxsidegap3methods _ -> Ohlcv
  | Ceil _ -> FloatBA
  | Cmo _ -> FloatBA
  | Correl _ -> FloatBA2
  | Cos _ -> FloatBA
  | Cosh _ -> FloatBA
  | Dema _ -> FloatBA
  | Div _ -> FloatBA2
  | Dx _ -> Ohlcv
  | Ema _ -> FloatBA
  | Exp _ -> FloatBA
  | Floor _ -> FloatBA
  | Ht_dcperiod _ -> FloatBA
  | Ht_dcphase _ -> FloatBA
  | Ht_phasor _ -> FloatBA
  | Ht_sine _ -> FloatBA
  | Ht_trendline _ -> FloatBA
  | Ht_trendmode _ -> FloatBA
  | Imi _ -> Ohlcv
  | Kama _ -> FloatBA
  | Linearreg _ -> FloatBA
  | Linearreg_angle _ -> FloatBA
  | Linearreg_intercept _ -> FloatBA
  | Linearreg_slope _ -> FloatBA
  | Ln _ -> FloatBA
  | Log10 _ -> FloatBA
  | Ma _ -> FloatBA
  | Macd _ -> FloatBA
  | Macdext _ -> FloatBA
  | Macdfix _ -> FloatBA
  | Mama _ -> FloatBA
  | Mavp _ -> FloatBA2
  | Max _ -> FloatBA
  | Maxindex _ -> FloatBA
  | Medprice _ -> Ohlcv
  | Mfi _ -> Ohlcv
  | Midpoint _ -> FloatBA
  | Midprice _ -> Ohlcv
  | Min _ -> FloatBA
  | Minindex _ -> FloatBA
  | Minmax _ -> FloatBA
  | Minmaxindex _ -> FloatBA
  | Minus_di _ -> Ohlcv
  | Minus_dm _ -> Ohlcv
  | Mom _ -> FloatBA
  | Mult _ -> FloatBA2
  | Natr _ -> Ohlcv
  | Obv _ -> Ohlcv
  | Plus_di _ -> Ohlcv
  | Plus_dm _ -> Ohlcv
  | Ppo _ -> FloatBA
  | Roc _ -> FloatBA
  | Rocp _ -> FloatBA
  | Rocr _ -> FloatBA
  | Rocr100 _ -> FloatBA
  | Rsi _ -> FloatBA
  | Sar _ -> Ohlcv
  | Sarext _ -> Ohlcv
  | Sin _ -> FloatBA
  | Sinh _ -> FloatBA
  | Sma _ -> FloatBA
  | Sqrt _ -> FloatBA
  | Stddev _ -> FloatBA
  | Stoch _ -> Ohlcv
  | Stochf _ -> Ohlcv
  | Stochrsi _ -> FloatBA
  | Sub _ -> FloatBA2
  | Sum _ -> FloatBA
  | T3 _ -> FloatBA
  | Tan _ -> FloatBA
  | Tanh _ -> FloatBA
  | Tema _ -> FloatBA
  | Trange _ -> Ohlcv
  | Trima _ -> FloatBA
  | Trix _ -> FloatBA
  | Tsf _ -> FloatBA
  | Typprice _ -> Ohlcv
  | Ultosc _ -> Ohlcv
  | Var _ -> FloatBA
  | Wclprice _ -> Ohlcv
  | Willr _ -> Ohlcv
  | Wma _ -> FloatBA

(* let calculate (type a b) ?i (indicator : ('a, 'b) Wrappers.t) (input_ohclv : 'c -> Ohlcv.t) *)
(*     (input_floatba : 'c -> Wrappers.float_ba) *)
(*     (input_floatba2 : 'c -> Wrappers.float_ba * Wrappers.float_ba) *)
(*     (output_float_ba : 'c -> int -> Wrappers.float_ba) *)
(*     (output_int_ba : 'c -> int -> Wrappers.float_ba) (data : 'c) = *)
(*   let output = Output.get indicator in *)
(*   match get_input_descriptor indicator with *)
(*   | FloatBA2 -> *)
(*     (let ba2 = input_floatba2 data in *)
(*      let calc : 'a = Wrappers.calculate ?i indicator ba2 in *)
(*      match output with *)
(*      | OneF index -> *)
(*        let output = output_float_ba data index in *)
(*        calc output *)
(*      | TwoF (i1, i2) -> *)
(*        (\* let output = output_float_ba data i1, output_float_ba data i2 in *\) *)
(*        (\* let res = Wrappers.calculate ?i indicator ba2 output in *\) *)
(*        (\* res *\) *)
(*      | _ -> invalid_arg "WIP" *)

(*     ) *)
(*   | _ -> invalid_arg "WIP" *)
