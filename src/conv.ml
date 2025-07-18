(** Conversion module between Safe.t and Indicator.t types *)

module S = Safe

(** Convert a Safe.t indicator to a list of Indicator.t outputs *)
let safe_to_indicators : type a b. (a, b) S.t -> Indicator.t list = function
  | S.Accbands { timeperiod } ->
    [ F (Indicator.Float.UpperBBand { timeperiod; nb_dev_up = 2.0; nb_dev_dn = 2.0; ma_type = Ma_type.Sma });
      F (Indicator.Float.MiddleBBand { timeperiod; nb_dev_up = 2.0; nb_dev_dn = 2.0; ma_type = Ma_type.Sma });
      F (Indicator.Float.LowerBBand { timeperiod; nb_dev_up = 2.0; nb_dev_dn = 2.0; ma_type = Ma_type.Sma }) ]
  | S.Acos _ -> [ F Indicator.Float.Acos ]
  | S.Ad _ -> [ F Indicator.Float.Ad ]
  | S.Add _ -> [ F Indicator.Float.Add ]
  | S.Adosc { fast_period; slow_period } -> [ F (Indicator.Float.Adosc { fast_period; slow_period }) ]
  | S.Adx { timeperiod } -> [ F (Indicator.Float.Adx { timeperiod }) ]
  | S.Adxr { timeperiod } -> [ F (Indicator.Float.Adxr { timeperiod }) ]
  | S.Apo { fast_period; slow_period; ma_type } -> [ F (Indicator.Float.Apo { fast_period; slow_period; ma_type }) ]
  | S.Aroon { timeperiod } -> 
    [ F (Indicator.Float.Aroon_Down { timeperiod }); 
      F (Indicator.Float.Aroon_Up { timeperiod }) ]
  | S.Aroonosc { timeperiod } -> [ F (Indicator.Float.AroonOsc { timeperiod }) ]
  | S.Asin _ -> [ F Indicator.Float.Asin ]
  | S.Atan _ -> [ F Indicator.Float.Atan ]
  | S.Atr { timeperiod } -> [ F (Indicator.Float.Atr { timeperiod }) ]
  | S.Avgprice _ -> [ F Indicator.Float.AvgPrice ]
  | S.Avgdev { timeperiod } -> [ F (Indicator.Float.Avgdev { timeperiod }) ]
  | S.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
    [ F (Indicator.Float.UpperBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type });
      F (Indicator.Float.MiddleBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type });
      F (Indicator.Float.LowerBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) ]
  | S.Beta { timeperiod } -> [ F (Indicator.Float.Beta { timeperiod }) ]
  | S.Bop _ -> [ F Indicator.Float.Bop ]
  | S.Cci { timeperiod } -> [ F (Indicator.Float.Cci { timeperiod }) ]
  | S.Cdl2crows _ -> [ I Indicator.Int.Cdl2Crows ]
  | S.Cdl3blackcrows _ -> [ I Indicator.Int.Cdl3BlackCrows ]
  | S.Cdl3inside _ -> [ I Indicator.Int.Cdl3Inside ]
  | S.Cdl3linestrike _ -> [ I Indicator.Int.Cdl3LineStrike ]
  | S.Cdl3outside _ -> [ I Indicator.Int.Cdl3Outside ]
  | S.Cdl3starsinsouth _ -> [ I Indicator.Int.Cdl3StarsInSouth ]
  | S.Cdl3whitesoldiers _ -> [ I Indicator.Int.Cdl3WhiteSoldiers ]
  | S.Cdlabandonedbaby { penetration } -> [ I (Indicator.Int.CdlAbandonedBaby { penetration }) ]
  | S.Cdladvanceblock _ -> [ I Indicator.Int.CdlAdvanceBlock ]
  | S.Cdlbelthold _ -> [ I Indicator.Int.CdlBeltHold ]
  | S.Cdlbreakaway _ -> [ I Indicator.Int.CdlBreakaway ]
  | S.Cdlclosingmarubozu _ -> [ I Indicator.Int.CdlClosingMarubozu ]
  | S.Cdlconcealbabyswall _ -> [ I Indicator.Int.CdlConcealBabySwall ]
  | S.Cdlcounterattack _ -> [ I Indicator.Int.CdlCounterAttack ]
  | S.Cdldarkcloudcover { penetration } -> [ I (Indicator.Int.CdlDarkCloudCover { penetration }) ]
  | S.Cdldoji _ -> [ I Indicator.Int.CdlDoji ]
  | S.Cdldojistar _ -> [ I Indicator.Int.CdlDojiStar ]
  | S.Cdldragonflydoji _ -> [ I Indicator.Int.CdlDragonflyDoji ]
  | S.Cdlengulfing _ -> [ I Indicator.Int.CdlEngulfing ]
  | S.Cdleveningdojistar { penetration } -> [ I (Indicator.Int.CdlEveningDojiStar { penetration }) ]
  | S.Cdleveningstar { penetration } -> [ I (Indicator.Int.CdlEveningStar { penetration }) ]
  | S.Cdlgapsidesidewhite _ -> [ I Indicator.Int.CdlGapSideSideWhite ]
  | S.Cdlgravestonedoji _ -> [ I Indicator.Int.CdlGravestoneDoji ]
  | S.Cdlhammer _ -> [ I Indicator.Int.CdlHammer ]
  | S.Cdlhangingman _ -> [ I Indicator.Int.CdlHangingMan ]
  | S.Cdlharami _ -> [ I Indicator.Int.CdlHarami ]
  | S.Cdlharamicross _ -> [ I Indicator.Int.CdlHaramiCross ]
  | S.Cdlhighwave _ -> [ I Indicator.Int.CdlHighWave ]
  | S.Cdlhikkake _ -> [ I Indicator.Int.CdlHikkake ]
  | S.Cdlhikkakemod _ -> [ I Indicator.Int.CdlHikkakeMod ]
  | S.Cdlhomingpigeon _ -> [ I Indicator.Int.CdlHomingPigeon ]
  | S.Cdlidentical3crows _ -> [ I Indicator.Int.CdlIdentical3Crows ]
  | S.Cdlinneck _ -> [ I Indicator.Int.CdlInNeck ]
  | S.Cdlinvertedhammer _ -> [ I Indicator.Int.CdlInvertedHammer ]
  | S.Cdlkicking _ -> [ I Indicator.Int.CdlKicking ]
  | S.Cdlkickingbylength _ -> [ I Indicator.Int.CdlKickingByLength ]
  | S.Cdlladderbottom _ -> [ I Indicator.Int.CdlLadderBottom ]
  | S.Cdllongleggeddoji _ -> [ I Indicator.Int.CdlLongLeggedDoji ]
  | S.Cdllongline _ -> [ I Indicator.Int.CdlLongLine ]
  | S.Cdlmarubozu _ -> [ I Indicator.Int.CdlMarubozu ]
  | S.Cdlmatchinglow _ -> [ I Indicator.Int.CdlMatchingLow ]
  | S.Cdlmathold { penetration } -> [ I (Indicator.Int.CdlMatHold { penetration }) ]
  | S.Cdlmorningdojistar { penetration } -> [ I (Indicator.Int.CdlMorningDojiStar { penetration }) ]
  | S.Cdlmorningstar { penetration } -> [ I (Indicator.Int.CdlMorningStar { penetration }) ]
  | S.Cdlonneck _ -> [ I Indicator.Int.CdlOnNeck ]
  | S.Cdlpiercing _ -> [ I Indicator.Int.CdlPiercing ]
  | S.Cdlrickshawman _ -> [ I Indicator.Int.CdlRickshawMan ]
  | S.Cdlrisefall3methods _ -> [ I Indicator.Int.CdlRiseFall3Methods ]
  | S.Cdlseparatinglines _ -> [ I Indicator.Int.CdlSeparatingLines ]
  | S.Cdlshootingstar _ -> [ I Indicator.Int.CdlShootingStar ]
  | S.Cdlshortline _ -> [ I Indicator.Int.CdlShortLine ]
  | S.Cdlspinningtop _ -> [ I Indicator.Int.CdlSpinningTop ]
  | S.Cdlstalledpattern _ -> [ I Indicator.Int.CdlStalledPattern ]
  | S.Cdlsticksandwich _ -> [ I Indicator.Int.CdlStickSandwich ]
  | S.Cdltakuri _ -> [ I Indicator.Int.CdlTakuri ]
  | S.Cdltasukigap _ -> [ I Indicator.Int.CdlTasukiGap ]
  | S.Cdlthrusting _ -> [ I Indicator.Int.CdlThrusting ]
  | S.Cdltristar _ -> [ I Indicator.Int.CdlTristar ]
  | S.Cdlunique3river _ -> [ I Indicator.Int.CdlUnique3River ]
  | S.Cdlupsidegap2crows _ -> [ I Indicator.Int.CdlUpsideGap2Crows ]
  | S.Cdlxsidegap3methods _ -> [ I Indicator.Int.CdlXSideGap3Methods ]
  | S.Ceil _ -> [ F Indicator.Float.Ceil ]
  | S.Cmo { timeperiod } -> [ F (Indicator.Float.Cmo { timeperiod }) ]
  | S.Correl { timeperiod } -> [ F (Indicator.Float.Correl { timeperiod }) ]
  | S.Cos _ -> [ F Indicator.Float.Cos ]
  | S.Cosh _ -> [ F Indicator.Float.Cosh ]
  | S.Dema { timeperiod } -> [ F (Indicator.Float.Dema { timeperiod }) ]
  | S.Div _ -> [ F Indicator.Float.Div ]
  | S.Dx { timeperiod } -> [ F (Indicator.Float.Dx { timeperiod }) ]
  | S.Ema { timeperiod } -> [ F (Indicator.Float.Ema { timeperiod }) ]
  | S.Exp _ -> [ F Indicator.Float.Exp ]
  | S.Floor _ -> [ F Indicator.Float.Floor ]
  | S.Ht_dcperiod _ -> [ F Indicator.Float.HtDcPeriod ]
  | S.Ht_dcphase _ -> [ F Indicator.Float.HtDcPhase ]
  | S.Ht_phasor _ -> 
    [ F Indicator.Float.HtPhasor_InPhase; 
      F Indicator.Float.HtPhasor_Quadrature ]
  | S.Ht_sine _ -> 
    [ F Indicator.Float.HtSine_Sine; 
      F Indicator.Float.HtSine_LeadSine ]
  | S.Ht_trendline _ -> [ F Indicator.Float.HtTrendline ]
  | S.Ht_trendmode _ -> [ I Indicator.Int.HtTrendMode ]
  | S.Imi { timeperiod } -> [ F (Indicator.Float.Imi { timeperiod }) ]
  | S.Kama { timeperiod } -> [ F (Indicator.Float.Kama { timeperiod }) ]
  | S.Linearreg { timeperiod } -> [ F (Indicator.Float.Linearreg { timeperiod }) ]
  | S.Linearreg_angle { timeperiod } -> [ F (Indicator.Float.LinearregAngle { timeperiod }) ]
  | S.Linearreg_intercept { timeperiod } -> [ F (Indicator.Float.LinearregIntercept { timeperiod }) ]
  | S.Linearreg_slope { timeperiod } -> [ F (Indicator.Float.LinearregSlope { timeperiod }) ]
  | S.Ln _ -> [ F Indicator.Float.Ln ]
  | S.Log10 _ -> [ F Indicator.Float.Log10 ]
  | S.Ma { timeperiod; ma_type } -> [ F (Indicator.Float.Ma { timeperiod; ma_type }) ]
  | S.Macd { fast_period; slow_period; signal_period } ->
    [ F (Indicator.Float.Macd_MACD { fast_period; slow_period; signal_period });
      F (Indicator.Float.Macd_MACDSignal { fast_period; slow_period; signal_period });
      F (Indicator.Float.Macd_MACDHist { fast_period; slow_period; signal_period }) ]
  | S.Macdext { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type } ->
    [ F (Indicator.Float.MacdExt_MACD { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type });
      F (Indicator.Float.MacdExt_MACDSignal { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type });
      F (Indicator.Float.MacdExt_MACDHist { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type }) ]
  | S.Macdfix { signal_period } ->
    [ F (Indicator.Float.MacdFix_MACD { signal_period });
      F (Indicator.Float.MacdFix_MACDSignal { signal_period });
      F (Indicator.Float.MacdFix_MACDHist { signal_period }) ]
  | S.Mama { fast_limit; slow_limit } ->
    [ F (Indicator.Float.Mama { fast_limit; slow_limit });
      F (Indicator.Float.Mama_MAMA { fast_limit; slow_limit });
      F (Indicator.Float.Mama_FAMA { fast_limit; slow_limit }) ]
  | S.Mavp { min_period; max_period; ma_type } -> [ F (Indicator.Float.Mavp { min_period; max_period; ma_type }) ]
  | S.Max { timeperiod } -> [ F (Indicator.Float.Max { timeperiod }) ]
  | S.Maxindex { timeperiod } -> [ I (Indicator.Int.MaxIndex { timeperiod }) ]
  | S.Medprice _ -> [ F Indicator.Float.MedPrice ]
  | S.Mfi { timeperiod } -> [ F (Indicator.Float.Mfi { timeperiod }) ]
  | S.Midpoint { timeperiod } -> [ F (Indicator.Float.Midpoint { timeperiod }) ]
  | S.Midprice { timeperiod } -> [ F (Indicator.Float.Midprice { timeperiod }) ]
  | S.Min { timeperiod } -> [ F (Indicator.Float.Min { timeperiod }) ]
  | S.Minindex { timeperiod } -> [ I (Indicator.Int.MinIndex { timeperiod }) ]
  | S.Minmax { timeperiod } ->
    [ F (Indicator.Float.MinMax_Min { timeperiod });
      F (Indicator.Float.MinMax_Max { timeperiod }) ]
  | S.Minmaxindex { timeperiod } ->
    [ I (Indicator.Int.MinMaxIndex_Min { timeperiod });
      I (Indicator.Int.MinMaxIndex_Max { timeperiod }) ]
  | S.Minus_di { timeperiod } -> [ F (Indicator.Float.MinusDI { timeperiod }) ]
  | S.Minus_dm { timeperiod } -> [ F (Indicator.Float.MinusDM { timeperiod }) ]
  | S.Mom { timeperiod } -> [ F (Indicator.Float.Mom { timeperiod }) ]
  | S.Mult _ -> [ F Indicator.Float.Mult ]
  | S.Natr { timeperiod } -> [ F (Indicator.Float.Natr { timeperiod }) ]
  | S.Obv _ -> [ F Indicator.Float.Obv ]
  | S.Plus_di { timeperiod } -> [ F (Indicator.Float.PlusDI { timeperiod }) ]
  | S.Plus_dm { timeperiod } -> [ F (Indicator.Float.PlusDM { timeperiod }) ]
  | S.Ppo { fast_period; slow_period; ma_type } -> [ F (Indicator.Float.Ppo { fast_period; slow_period; ma_type }) ]
  | S.Roc { timeperiod } -> [ F (Indicator.Float.Roc { timeperiod }) ]
  | S.Rocp { timeperiod } -> [ F (Indicator.Float.Rocp { timeperiod }) ]
  | S.Rocr { timeperiod } -> [ F (Indicator.Float.Rocr { timeperiod }) ]
  | S.Rocr100 { timeperiod } -> [ F (Indicator.Float.Rocr100 { timeperiod }) ]
  | S.Rsi { timeperiod } -> [ F (Indicator.Float.Rsi { timeperiod }) ]
  | S.Sar { acceleration; maximum } -> [ F (Indicator.Float.Sar { acceleration; maximum }) ]
  | S.Sarext { start_value; offset_on_reverse; acceleration_init_long; acceleration_long; acceleration_max_long; acceleration_init_short; acceleration_short; acceleration_max_short } ->
    [ F (Indicator.Float.Sarext { start_value; offset_on_reverse; acceleration_init_long; acceleration_long; acceleration_max_long; acceleration_init_short; acceleration_short; acceleration_max_short }) ]
  | S.Sin _ -> [ F Indicator.Float.Sin ]
  | S.Sinh _ -> [ F Indicator.Float.Sinh ]
  | S.Sma { timeperiod } -> [ F (Indicator.Float.Sma { timeperiod }) ]
  | S.Sqrt _ -> [ F Indicator.Float.Sqrt ]
  | S.Stddev { timeperiod; nb_dev } -> [ F (Indicator.Float.Stddev { timeperiod; nb_dev }) ]
  | S.Stoch { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type } ->
    [ F (Indicator.Float.Stoch_SlowK { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type });
      F (Indicator.Float.Stoch_SlowD { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type }) ]
  | S.Stochf { fast_k_period; fast_d_period; fast_d_ma_type } ->
    [ F (Indicator.Float.StochF_FastK { fast_k_period; fast_d_period; fast_d_ma_type });
      F (Indicator.Float.StochF_FastD { fast_k_period; fast_d_period; fast_d_ma_type }) ]
  | S.Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
    [ F (Indicator.Float.StochRsi_FastK { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type });
      F (Indicator.Float.StochRsi_FastD { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }) ]
  | S.Sub _ -> [ F Indicator.Float.Sub ]
  | S.Sum { timeperiod } -> [ F (Indicator.Float.Sum { timeperiod }) ]
  | S.T3 { timeperiod; v_factor } -> [ F (Indicator.Float.T3 { timeperiod; v_factor }) ]
  | S.Tan _ -> [ F Indicator.Float.Tan ]
  | S.Tanh _ -> [ F Indicator.Float.Tanh ]
  | S.Tema { timeperiod } -> [ F (Indicator.Float.Tema { timeperiod }) ]
  | S.Trange _ -> [ F Indicator.Float.Trange ]
  | S.Trima { timeperiod } -> [ F (Indicator.Float.Trima { timeperiod }) ]
  | S.Trix { timeperiod } -> [ F (Indicator.Float.Trix { timeperiod }) ]
  | S.Tsf { timeperiod } -> [ F (Indicator.Float.Tsf { timeperiod }) ]
  | S.Typprice _ -> [ F Indicator.Float.TypPrice ]
  | S.Ultosc { timeperiod1; timeperiod2; timeperiod3 } -> [ F (Indicator.Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 }) ]
  | S.Var { timeperiod; nb_dev } -> [ F (Indicator.Float.Var { timeperiod; nb_dev }) ]
  | S.Wclprice _ -> [ F Indicator.Float.WclPrice ]
  | S.Willr { timeperiod } -> [ F (Indicator.Float.Willr { timeperiod }) ]
  | S.Wma { timeperiod } -> [ F (Indicator.Float.Wma { timeperiod }) ]

(** Convert an Indicator.t back to its corresponding Safe.t *)
let indicator_to_safe : Indicator.t -> Pack.t = function
  | F (Indicator.Float.UpperBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) ->
    Pack.pack (S.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type })
  | F (Indicator.Float.MiddleBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) ->
    Pack.pack (S.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type })
  | F (Indicator.Float.LowerBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) ->
    Pack.pack (S.Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type })
  | F Indicator.Float.Acos -> Pack.pack (S.Acos ())
  | F Indicator.Float.Ad -> Pack.pack (S.Ad ())
  | F Indicator.Float.Add -> Pack.pack (S.Add ())
  | F (Indicator.Float.Adosc { fast_period; slow_period }) ->
    Pack.pack (S.Adosc { fast_period; slow_period })
  | F (Indicator.Float.Adx { timeperiod }) ->
    Pack.pack (S.Adx { timeperiod })
  | F (Indicator.Float.Adxr { timeperiod }) ->
    Pack.pack (S.Adxr { timeperiod })
  | F (Indicator.Float.Apo { fast_period; slow_period; ma_type }) ->
    Pack.pack (S.Apo { fast_period; slow_period; ma_type })
  | F (Indicator.Float.Aroon_Down { timeperiod }) ->
    Pack.pack (S.Aroon { timeperiod })
  | F (Indicator.Float.Aroon_Up { timeperiod }) ->
    Pack.pack (S.Aroon { timeperiod })
  | F (Indicator.Float.AroonOsc { timeperiod }) ->
    Pack.pack (S.Aroonosc { timeperiod })
  | F Indicator.Float.Asin -> Pack.pack (S.Asin ())
  | F Indicator.Float.Atan -> Pack.pack (S.Atan ())
  | F (Indicator.Float.Atr { timeperiod }) ->
    Pack.pack (S.Atr { timeperiod })
  | F Indicator.Float.AvgPrice -> Pack.pack (S.Avgprice ())
  | F (Indicator.Float.Avgdev { timeperiod }) ->
    Pack.pack (S.Avgdev { timeperiod })
  | F (Indicator.Float.Beta { timeperiod }) ->
    Pack.pack (S.Beta { timeperiod })
  | F Indicator.Float.Bop -> Pack.pack (S.Bop ())
  | F (Indicator.Float.Cci { timeperiod }) ->
    Pack.pack (S.Cci { timeperiod })
  | F Indicator.Float.Ceil -> Pack.pack (S.Ceil ())
  | F (Indicator.Float.Cmo { timeperiod }) ->
    Pack.pack (S.Cmo { timeperiod })
  | F (Indicator.Float.Correl { timeperiod }) ->
    Pack.pack (S.Correl { timeperiod })
  | F Indicator.Float.Cos -> Pack.pack (S.Cos ())
  | F Indicator.Float.Cosh -> Pack.pack (S.Cosh ())
  | F (Indicator.Float.Dema { timeperiod }) ->
    Pack.pack (S.Dema { timeperiod })
  | F Indicator.Float.Div -> Pack.pack (S.Div ())
  | F (Indicator.Float.Dx { timeperiod }) ->
    Pack.pack (S.Dx { timeperiod })
  | F (Indicator.Float.Ema { timeperiod }) ->
    Pack.pack (S.Ema { timeperiod })
  | F Indicator.Float.Exp -> Pack.pack (S.Exp ())
  | F Indicator.Float.Floor -> Pack.pack (S.Floor ())
  | F Indicator.Float.HtDcPeriod -> Pack.pack (S.Ht_dcperiod ())
  | F Indicator.Float.HtDcPhase -> Pack.pack (S.Ht_dcphase ())
  | F Indicator.Float.HtPhasor_InPhase -> Pack.pack (S.Ht_phasor ())
  | F Indicator.Float.HtPhasor_Quadrature -> Pack.pack (S.Ht_phasor ())
  | F Indicator.Float.HtSine_Sine -> Pack.pack (S.Ht_sine ())
  | F Indicator.Float.HtSine_LeadSine -> Pack.pack (S.Ht_sine ())
  | F Indicator.Float.HtTrendline -> Pack.pack (S.Ht_trendline ())
  | F (Indicator.Float.Imi { timeperiod }) ->
    Pack.pack (S.Imi { timeperiod })
  | F (Indicator.Float.Kama { timeperiod }) ->
    Pack.pack (S.Kama { timeperiod })
  | F (Indicator.Float.Linearreg { timeperiod }) ->
    Pack.pack (S.Linearreg { timeperiod })
  | F (Indicator.Float.LinearregAngle { timeperiod }) ->
    Pack.pack (S.Linearreg_angle { timeperiod })
  | F (Indicator.Float.LinearregIntercept { timeperiod }) ->
    Pack.pack (S.Linearreg_intercept { timeperiod })
  | F (Indicator.Float.LinearregSlope { timeperiod }) ->
    Pack.pack (S.Linearreg_slope { timeperiod })
  | F Indicator.Float.Ln -> Pack.pack (S.Ln ())
  | F Indicator.Float.Log10 -> Pack.pack (S.Log10 ())
  | F (Indicator.Float.Ma { timeperiod; ma_type }) ->
    Pack.pack (S.Ma { timeperiod; ma_type })
  | F (Indicator.Float.Macd_MACD { fast_period; slow_period; signal_period }) ->
    Pack.pack (S.Macd { fast_period; slow_period; signal_period })
  | F (Indicator.Float.Macd_MACDSignal { fast_period; slow_period; signal_period }) ->
    Pack.pack (S.Macd { fast_period; slow_period; signal_period })
  | F (Indicator.Float.Macd_MACDHist { fast_period; slow_period; signal_period }) ->
    Pack.pack (S.Macd { fast_period; slow_period; signal_period })
  | F (Indicator.Float.MacdExt_MACD { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type }) ->
    Pack.pack (S.Macdext { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type })
  | F (Indicator.Float.MacdExt_MACDSignal { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type }) ->
    Pack.pack (S.Macdext { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type })
  | F (Indicator.Float.MacdExt_MACDHist { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type }) ->
    Pack.pack (S.Macdext { fast_period; fast_ma_type; slow_period; slow_ma_type; signal_period; signal_ma_type })
  | F (Indicator.Float.MacdFix_MACD { signal_period }) ->
    Pack.pack (S.Macdfix { signal_period })
  | F (Indicator.Float.MacdFix_MACDSignal { signal_period }) ->
    Pack.pack (S.Macdfix { signal_period })
  | F (Indicator.Float.MacdFix_MACDHist { signal_period }) ->
    Pack.pack (S.Macdfix { signal_period })
  | F (Indicator.Float.Mama { fast_limit; slow_limit }) ->
    Pack.pack (S.Mama { fast_limit; slow_limit })
  | F (Indicator.Float.Mama_MAMA { fast_limit; slow_limit }) ->
    Pack.pack (S.Mama { fast_limit; slow_limit })
  | F (Indicator.Float.Mama_FAMA { fast_limit; slow_limit }) ->
    Pack.pack (S.Mama { fast_limit; slow_limit })
  | F (Indicator.Float.Mavp { min_period; max_period; ma_type }) ->
    Pack.pack (S.Mavp { min_period; max_period; ma_type })
  | F (Indicator.Float.Max { timeperiod }) ->
    Pack.pack (S.Max { timeperiod })
  | F Indicator.Float.MedPrice -> Pack.pack (S.Medprice ())
  | F (Indicator.Float.Mfi { timeperiod }) ->
    Pack.pack (S.Mfi { timeperiod })
  | F (Indicator.Float.Midpoint { timeperiod }) ->
    Pack.pack (S.Midpoint { timeperiod })
  | F (Indicator.Float.Midprice { timeperiod }) ->
    Pack.pack (S.Midprice { timeperiod })
  | F (Indicator.Float.Min { timeperiod }) ->
    Pack.pack (S.Min { timeperiod })
  | F (Indicator.Float.MinMax_Min { timeperiod }) ->
    Pack.pack (S.Minmax { timeperiod })
  | F (Indicator.Float.MinMax_Max { timeperiod }) ->
    Pack.pack (S.Minmax { timeperiod })
  | F (Indicator.Float.MinusDI { timeperiod }) ->
    Pack.pack (S.Minus_di { timeperiod })
  | F (Indicator.Float.MinusDM { timeperiod }) ->
    Pack.pack (S.Minus_dm { timeperiod })
  | F (Indicator.Float.Mom { timeperiod }) ->
    Pack.pack (S.Mom { timeperiod })
  | F Indicator.Float.Mult -> Pack.pack (S.Mult ())
  | F (Indicator.Float.Natr { timeperiod }) ->
    Pack.pack (S.Natr { timeperiod })
  | F Indicator.Float.Obv -> Pack.pack (S.Obv ())
  | F (Indicator.Float.PlusDI { timeperiod }) ->
    Pack.pack (S.Plus_di { timeperiod })
  | F (Indicator.Float.PlusDM { timeperiod }) ->
    Pack.pack (S.Plus_dm { timeperiod })
  | F (Indicator.Float.Ppo { fast_period; slow_period; ma_type }) ->
    Pack.pack (S.Ppo { fast_period; slow_period; ma_type })
  | F (Indicator.Float.Roc { timeperiod }) ->
    Pack.pack (S.Roc { timeperiod })
  | F (Indicator.Float.Rocp { timeperiod }) ->
    Pack.pack (S.Rocp { timeperiod })
  | F (Indicator.Float.Rocr { timeperiod }) ->
    Pack.pack (S.Rocr { timeperiod })
  | F (Indicator.Float.Rocr100 { timeperiod }) ->
    Pack.pack (S.Rocr100 { timeperiod })
  | F (Indicator.Float.Rsi { timeperiod }) ->
    Pack.pack (S.Rsi { timeperiod })
  | F (Indicator.Float.Sar { acceleration; maximum }) ->
    Pack.pack (S.Sar { acceleration; maximum })
  | F (Indicator.Float.Sarext { start_value; offset_on_reverse; acceleration_init_long; acceleration_long; acceleration_max_long; acceleration_init_short; acceleration_short; acceleration_max_short }) ->
    Pack.pack (S.Sarext { start_value; offset_on_reverse; acceleration_init_long; acceleration_long; acceleration_max_long; acceleration_init_short; acceleration_short; acceleration_max_short })
  | F Indicator.Float.Sin -> Pack.pack (S.Sin ())
  | F Indicator.Float.Sinh -> Pack.pack (S.Sinh ())
  | F (Indicator.Float.Sma { timeperiod }) ->
    Pack.pack (S.Sma { timeperiod })
  | F Indicator.Float.Sqrt -> Pack.pack (S.Sqrt ())
  | F (Indicator.Float.Stddev { timeperiod; nb_dev }) ->
    Pack.pack (S.Stddev { timeperiod; nb_dev })
  | F (Indicator.Float.Stoch_SlowK { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type }) ->
    Pack.pack (S.Stoch { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type })
  | F (Indicator.Float.Stoch_SlowD { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type }) ->
    Pack.pack (S.Stoch { fast_k_period; slow_k_period; slow_k_ma_type; slow_d_period; slow_d_ma_type })
  | F (Indicator.Float.StochF_FastK { fast_k_period; fast_d_period; fast_d_ma_type }) ->
    Pack.pack (S.Stochf { fast_k_period; fast_d_period; fast_d_ma_type })
  | F (Indicator.Float.StochF_FastD { fast_k_period; fast_d_period; fast_d_ma_type }) ->
    Pack.pack (S.Stochf { fast_k_period; fast_d_period; fast_d_ma_type })
  | F (Indicator.Float.StochRsi_FastK { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }) ->
    Pack.pack (S.Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type })
  | F (Indicator.Float.StochRsi_FastD { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }) ->
    Pack.pack (S.Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type })
  | F Indicator.Float.Sub -> Pack.pack (S.Sub ())
  | F (Indicator.Float.Sum { timeperiod }) ->
    Pack.pack (S.Sum { timeperiod })
  | F (Indicator.Float.T3 { timeperiod; v_factor }) ->
    Pack.pack (S.T3 { timeperiod; v_factor })
  | F Indicator.Float.Tan -> Pack.pack (S.Tan ())
  | F Indicator.Float.Tanh -> Pack.pack (S.Tanh ())
  | F (Indicator.Float.Tema { timeperiod }) ->
    Pack.pack (S.Tema { timeperiod })
  | F Indicator.Float.Trange -> Pack.pack (S.Trange ())
  | F (Indicator.Float.Trima { timeperiod }) ->
    Pack.pack (S.Trima { timeperiod })
  | F (Indicator.Float.Trix { timeperiod }) ->
    Pack.pack (S.Trix { timeperiod })
  | F (Indicator.Float.Tsf { timeperiod }) ->
    Pack.pack (S.Tsf { timeperiod })
  | F Indicator.Float.TypPrice -> Pack.pack (S.Typprice ())
  | F (Indicator.Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 }) ->
    Pack.pack (S.Ultosc { timeperiod1; timeperiod2; timeperiod3 })
  | F (Indicator.Float.Var { timeperiod; nb_dev }) ->
    Pack.pack (S.Var { timeperiod; nb_dev })
  | F Indicator.Float.WclPrice -> Pack.pack (S.Wclprice ())
  | F (Indicator.Float.Willr { timeperiod }) ->
    Pack.pack (S.Willr { timeperiod })
  | F (Indicator.Float.Wma { timeperiod }) ->
    Pack.pack (S.Wma { timeperiod })
  (* Integer indicators *)
  | I Indicator.Int.HtTrendMode -> Pack.pack (S.Ht_trendmode ())
  | I Indicator.Int.Cdl2Crows -> Pack.pack (S.Cdl2crows ())
  | I Indicator.Int.Cdl3BlackCrows -> Pack.pack (S.Cdl3blackcrows ())
  | I Indicator.Int.Cdl3Inside -> Pack.pack (S.Cdl3inside ())
  | I Indicator.Int.Cdl3LineStrike -> Pack.pack (S.Cdl3linestrike ())
  | I Indicator.Int.Cdl3Outside -> Pack.pack (S.Cdl3outside ())
  | I Indicator.Int.Cdl3StarsInSouth -> Pack.pack (S.Cdl3starsinsouth ())
  | I Indicator.Int.Cdl3WhiteSoldiers -> Pack.pack (S.Cdl3whitesoldiers ())
  | I (Indicator.Int.CdlAbandonedBaby { penetration }) ->
    Pack.pack (S.Cdlabandonedbaby { penetration })
  | I Indicator.Int.CdlAdvanceBlock -> Pack.pack (S.Cdladvanceblock ())
  | I Indicator.Int.CdlBeltHold -> Pack.pack (S.Cdlbelthold ())
  | I Indicator.Int.CdlBreakaway -> Pack.pack (S.Cdlbreakaway ())
  | I Indicator.Int.CdlClosingMarubozu -> Pack.pack (S.Cdlclosingmarubozu ())
  | I Indicator.Int.CdlConcealBabySwall -> Pack.pack (S.Cdlconcealbabyswall ())
  | I Indicator.Int.CdlCounterAttack -> Pack.pack (S.Cdlcounterattack ())
  | I (Indicator.Int.CdlDarkCloudCover { penetration }) ->
    Pack.pack (S.Cdldarkcloudcover { penetration })
  | I Indicator.Int.CdlDoji -> Pack.pack (S.Cdldoji ())
  | I Indicator.Int.CdlDojiStar -> Pack.pack (S.Cdldojistar ())
  | I Indicator.Int.CdlDragonflyDoji -> Pack.pack (S.Cdldragonflydoji ())
  | I Indicator.Int.CdlEngulfing -> Pack.pack (S.Cdlengulfing ())
  | I (Indicator.Int.CdlEveningDojiStar { penetration }) ->
    Pack.pack (S.Cdleveningdojistar { penetration })
  | I (Indicator.Int.CdlEveningStar { penetration }) ->
    Pack.pack (S.Cdleveningstar { penetration })
  | I Indicator.Int.CdlGapSideSideWhite -> Pack.pack (S.Cdlgapsidesidewhite ())
  | I Indicator.Int.CdlGravestoneDoji -> Pack.pack (S.Cdlgravestonedoji ())
  | I Indicator.Int.CdlHammer -> Pack.pack (S.Cdlhammer ())
  | I Indicator.Int.CdlHangingMan -> Pack.pack (S.Cdlhangingman ())
  | I Indicator.Int.CdlHarami -> Pack.pack (S.Cdlharami ())
  | I Indicator.Int.CdlHaramiCross -> Pack.pack (S.Cdlharamicross ())
  | I Indicator.Int.CdlHighWave -> Pack.pack (S.Cdlhighwave ())
  | I Indicator.Int.CdlHikkake -> Pack.pack (S.Cdlhikkake ())
  | I Indicator.Int.CdlHikkakeMod -> Pack.pack (S.Cdlhikkakemod ())
  | I Indicator.Int.CdlHomingPigeon -> Pack.pack (S.Cdlhomingpigeon ())
  | I Indicator.Int.CdlIdentical3Crows -> Pack.pack (S.Cdlidentical3crows ())
  | I Indicator.Int.CdlInNeck -> Pack.pack (S.Cdlinneck ())
  | I Indicator.Int.CdlInvertedHammer -> Pack.pack (S.Cdlinvertedhammer ())
  | I Indicator.Int.CdlKicking -> Pack.pack (S.Cdlkicking ())
  | I Indicator.Int.CdlKickingByLength -> Pack.pack (S.Cdlkickingbylength ())
  | I Indicator.Int.CdlLadderBottom -> Pack.pack (S.Cdlladderbottom ())
  | I Indicator.Int.CdlLongLeggedDoji -> Pack.pack (S.Cdllongleggeddoji ())
  | I Indicator.Int.CdlLongLine -> Pack.pack (S.Cdllongline ())
  | I Indicator.Int.CdlMarubozu -> Pack.pack (S.Cdlmarubozu ())
  | I Indicator.Int.CdlMatchingLow -> Pack.pack (S.Cdlmatchinglow ())
  | I (Indicator.Int.CdlMatHold { penetration }) ->
    Pack.pack (S.Cdlmathold { penetration })
  | I (Indicator.Int.CdlMorningDojiStar { penetration }) ->
    Pack.pack (S.Cdlmorningdojistar { penetration })
  | I (Indicator.Int.CdlMorningStar { penetration }) ->
    Pack.pack (S.Cdlmorningstar { penetration })
  | I Indicator.Int.CdlOnNeck -> Pack.pack (S.Cdlonneck ())
  | I Indicator.Int.CdlPiercing -> Pack.pack (S.Cdlpiercing ())
  | I Indicator.Int.CdlRickshawMan -> Pack.pack (S.Cdlrickshawman ())
  | I Indicator.Int.CdlRiseFall3Methods -> Pack.pack (S.Cdlrisefall3methods ())
  | I Indicator.Int.CdlSeparatingLines -> Pack.pack (S.Cdlseparatinglines ())
  | I Indicator.Int.CdlShootingStar -> Pack.pack (S.Cdlshootingstar ())
  | I Indicator.Int.CdlShortLine -> Pack.pack (S.Cdlshortline ())
  | I Indicator.Int.CdlSpinningTop -> Pack.pack (S.Cdlspinningtop ())
  | I Indicator.Int.CdlStalledPattern -> Pack.pack (S.Cdlstalledpattern ())
  | I Indicator.Int.CdlStickSandwich -> Pack.pack (S.Cdlsticksandwich ())
  | I Indicator.Int.CdlTakuri -> Pack.pack (S.Cdltakuri ())
  | I Indicator.Int.CdlTasukiGap -> Pack.pack (S.Cdltasukigap ())
  | I Indicator.Int.CdlThrusting -> Pack.pack (S.Cdlthrusting ())
  | I Indicator.Int.CdlTristar -> Pack.pack (S.Cdltristar ())
  | I Indicator.Int.CdlUnique3River -> Pack.pack (S.Cdlunique3river ())
  | I Indicator.Int.CdlUpsideGap2Crows -> Pack.pack (S.Cdlupsidegap2crows ())
  | I Indicator.Int.CdlXSideGap3Methods -> Pack.pack (S.Cdlxsidegap3methods ())
  | I (Indicator.Int.MaxIndex { timeperiod }) ->
    Pack.pack (S.Maxindex { timeperiod })
  | I (Indicator.Int.MinIndex { timeperiod }) ->
    Pack.pack (S.Minindex { timeperiod })
  | I (Indicator.Int.MinMaxIndex_Min { timeperiod }) ->
    Pack.pack (S.Minmaxindex { timeperiod })
  | I (Indicator.Int.MinMaxIndex_Max { timeperiod }) ->
    Pack.pack (S.Minmaxindex { timeperiod })