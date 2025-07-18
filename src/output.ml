open Safe

type t =
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba
  | FloatBA3 of float_ba * float_ba * float_ba
  | IntBA of int_ba
  | IntBA2 of int_ba * int_ba

module Flag = struct
  type t =
    | FloatBAFlag of Indicator.t
    | FloatBA2Flag of Indicator.t * Indicator.t
    | FloatBA3Flag of Indicator.t * Indicator.t * Indicator.t
    | IntBAFlag of Indicator.t
    | IntBA2Flag of Indicator.t * Indicator.t

  let of_pack : Pack.t -> t =
   fun (Pack wrapper_params) ->
    match wrapper_params with
    | Accbands { timeperiod } ->
      FloatBA3Flag
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
    | Acos _ -> FloatBAFlag (F Indicator.Float.Acos)
    | Ad _ -> FloatBAFlag (F Indicator.Float.Ad)
    | Add _ -> FloatBAFlag (F Indicator.Float.Add)
    | Adosc { fast_period; slow_period } ->
      FloatBAFlag (F (Indicator.Float.Adosc { fast_period; slow_period }))
    | Adx { timeperiod } -> FloatBAFlag (F (Indicator.Float.Adx { timeperiod }))
    | Adxr { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Adxr { timeperiod }))
    | Apo { fast_period; slow_period; ma_type } ->
      FloatBAFlag
        (F (Indicator.Float.Apo { fast_period; slow_period; ma_type }))
    | Aroon { timeperiod } ->
      FloatBA2Flag
        ( F (Indicator.Float.Aroon_Down { timeperiod }),
          F (Indicator.Float.Aroon_Up { timeperiod }) )
    | Aroonosc { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.AroonOsc { timeperiod }))
    | Asin _ -> FloatBAFlag (F Indicator.Float.Asin)
    | Atan _ -> FloatBAFlag (F Indicator.Float.Atan)
    | Atr { timeperiod } -> FloatBAFlag (F (Indicator.Float.Atr { timeperiod }))
    | Avgprice _ -> FloatBAFlag (F Indicator.Float.AvgPrice)
    | Avgdev { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Avgdev { timeperiod }))
    | Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
      FloatBA3Flag
        ( F
            (Indicator.Float.UpperBBand
               { timeperiod; nb_dev_up; nb_dev_dn; ma_type }),
          F
            (Indicator.Float.MiddleBBand
               { timeperiod; nb_dev_up; nb_dev_dn; ma_type }),
          F
            (Indicator.Float.LowerBBand
               { timeperiod; nb_dev_up; nb_dev_dn; ma_type }) )
    | Beta { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Beta { timeperiod }))
    | Bop _ -> FloatBAFlag (F Indicator.Float.Bop)
    | Cci { timeperiod } -> FloatBAFlag (F (Indicator.Float.Cci { timeperiod }))
    | Cdl2crows _ -> IntBAFlag (I Indicator.Int.Cdl2Crows)
    | Cdl3blackcrows _ -> IntBAFlag (I Indicator.Int.Cdl3BlackCrows)
    | Cdl3inside _ -> IntBAFlag (I Indicator.Int.Cdl3Inside)
    | Cdl3linestrike _ -> IntBAFlag (I Indicator.Int.Cdl3LineStrike)
    | Cdl3outside _ -> IntBAFlag (I Indicator.Int.Cdl3Outside)
    | Cdl3starsinsouth _ -> IntBAFlag (I Indicator.Int.Cdl3StarsInSouth)
    | Cdl3whitesoldiers _ -> IntBAFlag (I Indicator.Int.Cdl3WhiteSoldiers)
    | Cdlabandonedbaby { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlAbandonedBaby { penetration }))
    | Cdladvanceblock _ -> IntBAFlag (I Indicator.Int.CdlAdvanceBlock)
    | Cdlbelthold _ -> IntBAFlag (I Indicator.Int.CdlBeltHold)
    | Cdlbreakaway _ -> IntBAFlag (I Indicator.Int.CdlBreakaway)
    | Cdlclosingmarubozu _ -> IntBAFlag (I Indicator.Int.CdlClosingMarubozu)
    | Cdlconcealbabyswall _ -> IntBAFlag (I Indicator.Int.CdlConcealBabySwall)
    | Cdlcounterattack _ -> IntBAFlag (I Indicator.Int.CdlCounterAttack)
    | Cdldarkcloudcover { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlDarkCloudCover { penetration }))
    | Cdldoji _ -> IntBAFlag (I Indicator.Int.CdlDoji)
    | Cdldojistar _ -> IntBAFlag (I Indicator.Int.CdlDojiStar)
    | Cdldragonflydoji _ -> IntBAFlag (I Indicator.Int.CdlDragonflyDoji)
    | Cdlengulfing _ -> IntBAFlag (I Indicator.Int.CdlEngulfing)
    | Cdleveningdojistar { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlEveningDojiStar { penetration }))
    | Cdleveningstar { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlEveningStar { penetration }))
    | Cdlgapsidesidewhite _ -> IntBAFlag (I Indicator.Int.CdlGapSideSideWhite)
    | Cdlgravestonedoji _ -> IntBAFlag (I Indicator.Int.CdlGravestoneDoji)
    | Cdlhammer _ -> IntBAFlag (I Indicator.Int.CdlHammer)
    | Cdlhangingman _ -> IntBAFlag (I Indicator.Int.CdlHangingMan)
    | Cdlharami _ -> IntBAFlag (I Indicator.Int.CdlHarami)
    | Cdlharamicross _ -> IntBAFlag (I Indicator.Int.CdlHaramiCross)
    | Cdlhighwave _ -> IntBAFlag (I Indicator.Int.CdlHighWave)
    | Cdlhikkake _ -> IntBAFlag (I Indicator.Int.CdlHikkake)
    | Cdlhikkakemod _ -> IntBAFlag (I Indicator.Int.CdlHikkakeMod)
    | Cdlhomingpigeon _ -> IntBAFlag (I Indicator.Int.CdlHomingPigeon)
    | Cdlidentical3crows _ -> IntBAFlag (I Indicator.Int.CdlIdentical3Crows)
    | Cdlinneck _ -> IntBAFlag (I Indicator.Int.CdlInNeck)
    | Cdlinvertedhammer _ -> IntBAFlag (I Indicator.Int.CdlInvertedHammer)
    | Cdlkicking _ -> IntBAFlag (I Indicator.Int.CdlKicking)
    | Cdlkickingbylength _ -> IntBAFlag (I Indicator.Int.CdlKickingByLength)
    | Cdlladderbottom _ -> IntBAFlag (I Indicator.Int.CdlLadderBottom)
    | Cdllongleggeddoji _ -> IntBAFlag (I Indicator.Int.CdlLongLeggedDoji)
    | Cdllongline _ -> IntBAFlag (I Indicator.Int.CdlLongLine)
    | Cdlmarubozu _ -> IntBAFlag (I Indicator.Int.CdlMarubozu)
    | Cdlmatchinglow _ -> IntBAFlag (I Indicator.Int.CdlMatchingLow)
    | Cdlmathold { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlMatHold { penetration }))
    | Cdlmorningdojistar { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlMorningDojiStar { penetration }))
    | Cdlmorningstar { penetration } ->
      IntBAFlag (I (Indicator.Int.CdlMorningStar { penetration }))
    | Cdlonneck _ -> IntBAFlag (I Indicator.Int.CdlOnNeck)
    | Cdlpiercing _ -> IntBAFlag (I Indicator.Int.CdlPiercing)
    | Cdlrickshawman _ -> IntBAFlag (I Indicator.Int.CdlRickshawMan)
    | Cdlrisefall3methods _ -> IntBAFlag (I Indicator.Int.CdlRiseFall3Methods)
    | Cdlseparatinglines _ -> IntBAFlag (I Indicator.Int.CdlSeparatingLines)
    | Cdlshootingstar _ -> IntBAFlag (I Indicator.Int.CdlShootingStar)
    | Cdlshortline _ -> IntBAFlag (I Indicator.Int.CdlShortLine)
    | Cdlspinningtop _ -> IntBAFlag (I Indicator.Int.CdlSpinningTop)
    | Cdlstalledpattern _ -> IntBAFlag (I Indicator.Int.CdlStalledPattern)
    | Cdlsticksandwich _ -> IntBAFlag (I Indicator.Int.CdlStickSandwich)
    | Cdltakuri _ -> IntBAFlag (I Indicator.Int.CdlTakuri)
    | Cdltasukigap _ -> IntBAFlag (I Indicator.Int.CdlTasukiGap)
    | Cdlthrusting _ -> IntBAFlag (I Indicator.Int.CdlThrusting)
    | Cdltristar _ -> IntBAFlag (I Indicator.Int.CdlTristar)
    | Cdlunique3river _ -> IntBAFlag (I Indicator.Int.CdlUnique3River)
    | Cdlupsidegap2crows _ -> IntBAFlag (I Indicator.Int.CdlUpsideGap2Crows)
    | Cdlxsidegap3methods _ -> IntBAFlag (I Indicator.Int.CdlXSideGap3Methods)
    | Ceil _ -> FloatBAFlag (F Indicator.Float.Ceil)
    | Cmo { timeperiod } -> FloatBAFlag (F (Indicator.Float.Cmo { timeperiod }))
    | Correl { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Correl { timeperiod }))
    | Cos _ -> FloatBAFlag (F Indicator.Float.Cos)
    | Cosh _ -> FloatBAFlag (F Indicator.Float.Cosh)
    | Dema { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Dema { timeperiod }))
    | Div _ -> FloatBAFlag (F Indicator.Float.Div)
    | Dx { timeperiod } -> FloatBAFlag (F (Indicator.Float.Dx { timeperiod }))
    | Ema { timeperiod } -> FloatBAFlag (F (Indicator.Float.Ema { timeperiod }))
    | Exp _ -> FloatBAFlag (F Indicator.Float.Exp)
    | Floor _ -> FloatBAFlag (F Indicator.Float.Floor)
    | Ht_dcperiod _ -> FloatBAFlag (F Indicator.Float.HtDcPeriod)
    | Ht_dcphase _ -> FloatBAFlag (F Indicator.Float.HtDcPhase)
    | Ht_phasor _ ->
      FloatBA2Flag
        ( F Indicator.Float.HtPhasor_InPhase,
          F Indicator.Float.HtPhasor_Quadrature )
    | Ht_sine _ ->
      FloatBA2Flag
        (F Indicator.Float.HtSine_Sine, F Indicator.Float.HtSine_LeadSine)
    | Ht_trendline _ -> FloatBAFlag (F Indicator.Float.HtTrendline)
    | Ht_trendmode _ -> IntBAFlag (I Indicator.Int.HtTrendMode)
    | Imi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Imi { timeperiod }))
    | Kama { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Kama { timeperiod }))
    | Linearreg { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Linearreg { timeperiod }))
    | Linearreg_angle { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.LinearregAngle { timeperiod }))
    | Linearreg_intercept { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.LinearregIntercept { timeperiod }))
    | Linearreg_slope { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.LinearregSlope { timeperiod }))
    | Ln _ -> FloatBAFlag (F Indicator.Float.Ln)
    | Log10 _ -> FloatBAFlag (F Indicator.Float.Log10)
    | Ma { timeperiod; ma_type } ->
      FloatBAFlag (F (Indicator.Float.Ma { timeperiod; ma_type }))
    | Macd { fast_period; slow_period; signal_period } ->
      FloatBA3Flag
        ( F
            (Indicator.Float.Macd_MACD
               { fast_period; slow_period; signal_period }),
          F
            (Indicator.Float.Macd_MACDSignal
               { fast_period; slow_period; signal_period }),
          F
            (Indicator.Float.Macd_MACDHist
               { fast_period; slow_period; signal_period }) )
    | Macdext
        {
          fast_period;
          fast_ma_type;
          slow_period;
          slow_ma_type;
          signal_period;
          signal_ma_type;
        } ->
      FloatBA3Flag
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
    | Macdfix { signal_period } ->
      FloatBA3Flag
        ( F (Indicator.Float.MacdFix_MACD { signal_period }),
          F (Indicator.Float.MacdFix_MACDSignal { signal_period }),
          F (Indicator.Float.MacdFix_MACDHist { signal_period }) )
    | Mama { fast_limit; slow_limit } ->
      FloatBA2Flag
        ( F (Indicator.Float.Mama_MAMA { fast_limit; slow_limit }),
          F (Indicator.Float.Mama_FAMA { fast_limit; slow_limit }) )
    | Mavp { min_period; max_period; ma_type } ->
      FloatBAFlag (F (Indicator.Float.Mavp { min_period; max_period; ma_type }))
    | Max { timeperiod } -> FloatBAFlag (F (Indicator.Float.Max { timeperiod }))
    | Maxindex { timeperiod } ->
      IntBAFlag (I (Indicator.Int.MaxIndex { timeperiod }))
    | Medprice _ -> FloatBAFlag (F Indicator.Float.MedPrice)
    | Mfi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Mfi { timeperiod }))
    | Midpoint { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Midpoint { timeperiod }))
    | Midprice { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Midprice { timeperiod }))
    | Min { timeperiod } -> FloatBAFlag (F (Indicator.Float.Min { timeperiod }))
    | Minindex { timeperiod } ->
      IntBAFlag (I (Indicator.Int.MinIndex { timeperiod }))
    | Minmax { timeperiod } ->
      FloatBA2Flag
        ( F (Indicator.Float.MinMax_Min { timeperiod }),
          F (Indicator.Float.MinMax_Max { timeperiod }) )
    | Minmaxindex { timeperiod } ->
      IntBA2Flag
        ( I (Indicator.Int.MinMaxIndex_Min { timeperiod }),
          I (Indicator.Int.MinMaxIndex_Max { timeperiod }) )
    | Minus_di { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.MinusDI { timeperiod }))
    | Minus_dm { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.MinusDM { timeperiod }))
    | Mom { timeperiod } -> FloatBAFlag (F (Indicator.Float.Mom { timeperiod }))
    | Mult _ -> FloatBAFlag (F Indicator.Float.Mult)
    | Natr { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Natr { timeperiod }))
    | Obv _ -> FloatBAFlag (F Indicator.Float.Obv)
    | Plus_di { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.PlusDI { timeperiod }))
    | Plus_dm { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.PlusDM { timeperiod }))
    | Ppo { fast_period; slow_period; ma_type } ->
      FloatBAFlag
        (F (Indicator.Float.Ppo { fast_period; slow_period; ma_type }))
    | Roc { timeperiod } -> FloatBAFlag (F (Indicator.Float.Roc { timeperiod }))
    | Rocp { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Rocp { timeperiod }))
    | Rocr { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Rocr { timeperiod }))
    | Rocr100 { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Rocr100 { timeperiod }))
    | Rsi { timeperiod } -> FloatBAFlag (F (Indicator.Float.Rsi { timeperiod }))
    | Sar { acceleration; maximum } ->
      FloatBAFlag (F (Indicator.Float.Sar { acceleration; maximum }))
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
    | Sin _ -> FloatBAFlag (F Indicator.Float.Sin)
    | Sinh _ -> FloatBAFlag (F Indicator.Float.Sinh)
    | Sma { timeperiod } -> FloatBAFlag (F (Indicator.Float.Sma { timeperiod }))
    | Sqrt _ -> FloatBAFlag (F Indicator.Float.Sqrt)
    | Stddev { timeperiod; nb_dev } ->
      FloatBAFlag (F (Indicator.Float.Stddev { timeperiod; nb_dev }))
    | Stoch
        {
          fast_k_period;
          slow_k_period;
          slow_k_ma_type;
          slow_d_period;
          slow_d_ma_type;
        } ->
      FloatBA2Flag
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
    | Stochf { fast_k_period; fast_d_period; fast_d_ma_type } ->
      FloatBA2Flag
        ( F
            (Indicator.Float.StochF_FastK
               { fast_k_period; fast_d_period; fast_d_ma_type }),
          F
            (Indicator.Float.StochF_FastD
               { fast_k_period; fast_d_period; fast_d_ma_type }) )
    | Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
      FloatBA2Flag
        ( F
            (Indicator.Float.StochRsi_FastK
               { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }),
          F
            (Indicator.Float.StochRsi_FastD
               { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type }) )
    | Sub _ -> FloatBAFlag (F Indicator.Float.Sub)
    | Sum { timeperiod } -> FloatBAFlag (F (Indicator.Float.Sum { timeperiod }))
    | T3 { timeperiod; v_factor } ->
      FloatBAFlag (F (Indicator.Float.T3 { timeperiod; v_factor }))
    | Tan _ -> FloatBAFlag (F Indicator.Float.Tan)
    | Tanh _ -> FloatBAFlag (F Indicator.Float.Tanh)
    | Tema { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Tema { timeperiod }))
    | Trange _ -> FloatBAFlag (F Indicator.Float.Trange)
    | Trima { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Trima { timeperiod }))
    | Trix { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Trix { timeperiod }))
    | Tsf { timeperiod } -> FloatBAFlag (F (Indicator.Float.Tsf { timeperiod }))
    | Typprice _ -> FloatBAFlag (F Indicator.Float.TypPrice)
    | Ultosc { timeperiod1; timeperiod2; timeperiod3 } ->
      FloatBAFlag
        (F (Indicator.Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 }))
    | Var { timeperiod; nb_dev } ->
      FloatBAFlag (F (Indicator.Float.Var { timeperiod; nb_dev }))
    | Wclprice _ -> FloatBAFlag (F Indicator.Float.WclPrice)
    | Willr { timeperiod } ->
      FloatBAFlag (F (Indicator.Float.Willr { timeperiod }))
    | Wma { timeperiod } -> FloatBAFlag (F (Indicator.Float.Wma { timeperiod }))
end

let to_string = function
  | FloatBA _ -> "FloatBA"
  | FloatBA2 _ -> "FloatBA2"
  | FloatBA3 _ -> "FloatBA3"
  | IntBA _ -> "IntBA"
  | IntBA2 _ -> "IntBA2"
