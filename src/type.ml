module Float = struct
  type t =
    | UpperBBand
    | MiddleBBand
    | LowerBBand
    | Dema
    | Ema
    | HtTrendline
    | Kama
    | Ma
    | Mama
    | Mavp
    | Midpoint
    | Midprice
    | Sar
    | Sarext
    | Sma
    | T3
    | Tema
    | Trima
    | Wma
    | Adx
    | Adxr
    | Apo
    | AroonOsc
    | Cci
    | Cmo
    | Dx
    | Macd_MACD
    | Macd_MACDSignal
    | Macd_MACDHist
    | MacdExt_MACD
    | MacdExt_MACDSignal
    | MacdExt_MACDHist
    | MacdFix_MACD
    | MacdFix_MACDSignal
    | MacdFix_MACDHist
    | Mfi
    | MinusDI
    | MinusDM
    | Mom
    | PlusDI
    | PlusDM
    | Ppo
    | Roc
    | Rocp
    | Rocr
    | Rocr100
    | Rsi
    | Stoch_SlowK
    | Stoch_SlowD
    | StochF_FastK
    | StochF_FastD
    | StochRsi_FastK
    | StochRsi_FastD
    | Trix
    | Ultosc
    | Willr
    | Ad
    | Adosc
    | Obv
    | Atr
    | Natr
    | Trange
    | AvgPrice
    | MedPrice
    | TypPrice
    | WclPrice
    | HtDcPeriod
    | HtDcPhase
    | HtPhasor_InPhase
    | HtPhasor_Quadrature
    | HtSine_Sine
    | HtSine_LeadSine
    | HtTrendMode
    | Aroon_Down
    | Aroon_Up
    | Beta
    | Correl
    | Linearreg
    | LinearregAngle
    | LinearregIntercept
    | LinearregSlope
    | MinMax_Min
    | MinMax_Max
    | Stddev
    | Tsf
    | Var
    | Acos
    | Asin
    | Atan
    | Ceil
    | Cos
    | Cosh
    | Exp
    | Floor
    | Ln
    | Log10
    | Sin
    | Sinh
    | Sqrt
    | Tan
    | Tanh
    | Add
    | Div
    | Max
    | Min
    | Mult
    | Sub
    | Sum

  let of_int = function
    | 0 -> UpperBBand
    | 1 -> MiddleBBand
    | 2 -> LowerBBand
    | 3 -> Dema
    | 4 -> Ema
    | 5 -> HtTrendline
    | 6 -> Kama
    | 7 -> Ma
    | 8 -> Mama
    | 9 -> Mavp
    | 10 -> Midpoint
    | 11 -> Midprice
    | 12 -> Sar
    | 13 -> Sarext
    | 14 -> Sma
    | 15 -> T3
    | 16 -> Tema
    | 17 -> Trima
    | 18 -> Wma
    | 19 -> Adx
    | 20 -> Adxr
    | 21 -> Apo
    | 22 -> AroonOsc
    | 23 -> Cci
    | 24 -> Cmo
    | 25 -> Dx
    | 26 -> Macd_MACD
    | 27 -> Macd_MACDSignal
    | 28 -> Macd_MACDHist
    | 29 -> MacdExt_MACD
    | 30 -> MacdExt_MACDSignal
    | 31 -> MacdExt_MACDHist
    | 32 -> MacdFix_MACD
    | 33 -> MacdFix_MACDSignal
    | 34 -> MacdFix_MACDHist
    | 35 -> Mfi
    | 36 -> MinusDI
    | 37 -> MinusDM
    | 38 -> Mom
    | 39 -> PlusDI
    | 40 -> PlusDM
    | 41 -> Ppo
    | 42 -> Roc
    | 43 -> Rocp
    | 44 -> Rocr
    | 45 -> Rocr100
    | 46 -> Rsi
    | 47 -> Stoch_SlowK
    | 48 -> Stoch_SlowD
    | 49 -> StochF_FastK
    | 50 -> StochF_FastD
    | 51 -> StochRsi_FastK
    | 52 -> StochRsi_FastD
    | 53 -> Trix
    | 54 -> Ultosc
    | 55 -> Willr
    | 56 -> Ad
    | 57 -> Adosc
    | 58 -> Obv
    | 59 -> Atr
    | 60 -> Natr
    | 61 -> Trange
    | 62 -> AvgPrice
    | 63 -> MedPrice
    | 64 -> TypPrice
    | 65 -> WclPrice
    | 66 -> HtDcPeriod
    | 67 -> HtDcPhase
    | 68 -> HtPhasor_InPhase
    | 69 -> HtPhasor_Quadrature
    | 70 -> HtSine_Sine
    | 71 -> HtSine_LeadSine
    | 72 -> HtTrendMode
    | 73 -> Aroon_Down
    | 74 -> Aroon_Up
    | 75 -> Beta
    | 76 -> Correl
    | 77 -> Linearreg
    | 78 -> LinearregAngle
    | 79 -> LinearregIntercept
    | 80 -> LinearregSlope
    | 81 -> MinMax_Min
    | 82 -> MinMax_Max
    | 83 -> Stddev
    | 84 -> Tsf
    | 85 -> Var
    | 86 -> Acos
    | 87 -> Asin
    | 88 -> Atan
    | 89 -> Ceil
    | 90 -> Cos
    | 91 -> Cosh
    | 92 -> Exp
    | 93 -> Floor
    | 94 -> Ln
    | 95 -> Log10
    | 96 -> Sin
    | 97 -> Sinh
    | 98 -> Sqrt
    | 99 -> Tan
    | 100 -> Tanh
    | 101 -> Add
    | 102 -> Div
    | 103 -> Max
    | 104 -> Min
    | 105 -> Mult
    | 106 -> Sub
    | 107 -> Sum
    | _ -> failwith "Unknown float indicator"

  let to_int = function
    | UpperBBand -> 0
    | MiddleBBand -> 1
    | LowerBBand -> 2
    | Dema -> 3
    | Ema -> 4
    | HtTrendline -> 5
    | Kama -> 6
    | Ma -> 7
    | Mama -> 8
    | Mavp -> 9
    | Midpoint -> 10
    | Midprice -> 11
    | Sar -> 12
    | Sarext -> 13
    | Sma -> 14
    | T3 -> 15
    | Tema -> 16
    | Trima -> 17
    | Wma -> 18
    | Adx -> 19
    | Adxr -> 20
    | Apo -> 21
    | AroonOsc -> 22
    | Cci -> 23
    | Cmo -> 24
    | Dx -> 25
    | Macd_MACD -> 26
    | Macd_MACDSignal -> 27
    | Macd_MACDHist -> 28
    | MacdExt_MACD -> 29
    | MacdExt_MACDSignal -> 30
    | MacdExt_MACDHist -> 31
    | MacdFix_MACD -> 32
    | MacdFix_MACDSignal -> 33
    | MacdFix_MACDHist -> 34
    | Mfi -> 35
    | MinusDI -> 36
    | MinusDM -> 37
    | Mom -> 38
    | PlusDI -> 39
    | PlusDM -> 40
    | Ppo -> 41
    | Roc -> 42
    | Rocp -> 43
    | Rocr -> 44
    | Rocr100 -> 45
    | Rsi -> 46
    | Stoch_SlowK -> 47
    | Stoch_SlowD -> 48
    | StochF_FastK -> 49
    | StochF_FastD -> 50
    | StochRsi_FastK -> 51
    | StochRsi_FastD -> 52
    | Trix -> 53
    | Ultosc -> 54
    | Willr -> 55
    | Ad -> 56
    | Adosc -> 57
    | Obv -> 58
    | Atr -> 59
    | Natr -> 60
    | Trange -> 61
    | AvgPrice -> 62
    | MedPrice -> 63
    | TypPrice -> 64
    | WclPrice -> 65
    | HtDcPeriod -> 66
    | HtDcPhase -> 67
    | HtPhasor_InPhase -> 68
    | HtPhasor_Quadrature -> 69
    | HtSine_Sine -> 70
    | HtSine_LeadSine -> 71
    | HtTrendMode -> 72
    | Aroon_Down -> 73
    | Aroon_Up -> 74
    | Beta -> 75
    | Correl -> 76
    | Linearreg -> 77
    | LinearregAngle -> 78
    | LinearregIntercept -> 79
    | LinearregSlope -> 80
    | MinMax_Min -> 81
    | MinMax_Max -> 82
    | Stddev -> 83
    | Tsf -> 84
    | Var -> 85
    | Acos -> 86
    | Asin -> 87
    | Atan -> 88
    | Ceil -> 89
    | Cos -> 90
    | Cosh -> 91
    | Exp -> 92
    | Floor -> 93
    | Ln -> 94
    | Log10 -> 95
    | Sin -> 96
    | Sinh -> 97
    | Sqrt -> 98
    | Tan -> 99
    | Tanh -> 100
    | Add -> 101
    | Div -> 102
    | Max -> 103
    | Min -> 104
    | Mult -> 105
    | Sub -> 106
    | Sum -> 107
end

module Int = struct
  type t =
    | Cdl2Crows
    | Cdl3BlackCrows
    | Cdl3Inside
    | Cdl3LineStrike
    | Cdl3Outside
    | Cdl3StarsInSouth
    | Cdl3WhiteSoldiers
    | CdlAbandonedBaby
    | CdlAdvanceBlock
    | CdlBeltHold
    | CdlBreakaway
    | CdlClosingMarubozu
    | CdlConcealBabySwall
    | CdlCounterAttack
    | CdlDarkCloudCover
    | CdlDoji
    | CdlDojiStar
    | CdlDragonflyDoji
    | CdlEngulfing
    | CdlEveningDojiStar
    | CdlEveningStar
    | CdlGapSideSideWhite
    | CdlGravestoneDoji
    | CdlHammer
    | CdlHangingMan
    | CdlHarami
    | CdlHaramiCross
    | CdlHighWave
    | CdlHikkake
    | CdlHikkakeMod
    | CdlHomingPigeon
    | CdlIdentical3Crows
    | CdlInNeck
    | CdlInvertedHammer
    | CdlKicking
    | CdlKickingByLength
    | CdlLadderBottom
    | CdlLongLeggedDoji
    | CdlLongLine
    | CdlMarubozu
    | CdlMatchingLow
    | CdlMatHold
    | CdlMorningDojiStar
    | CdlMorningStar
    | CdlOnNeck
    | CdlPiercing
    | CdlRickshawMan
    | CdlRiseFall3Methods
    | CdlSeparatingLines
    | CdlShootingStar
    | CdlShortLine
    | CdlSpinningTop
    | CdlStalledPattern
    | CdlStickSandwich
    | CdlTakuri
    | CdlTasukiGap
    | CdlThrusting
    | CdlTristar
    | CdlUnique3River
    | CdlUpsideGap2Crows
    | CdlXSideGap3Methods
    | MaxIndex
    | MinIndex
    | MinMaxIndex_Min
    | MinMaxIndex_Max

  let of_int = function
    | 0 -> Cdl2Crows
    | 1 -> Cdl3BlackCrows
    | 2 -> Cdl3Inside
    | 3 -> Cdl3LineStrike
    | 4 -> Cdl3Outside
    | 5 -> Cdl3StarsInSouth
    | 6 -> Cdl3WhiteSoldiers
    | 7 -> CdlAbandonedBaby
    | 8 -> CdlAdvanceBlock
    | 9 -> CdlBeltHold
    | 10 -> CdlBreakaway
    | 11 -> CdlClosingMarubozu
    | 12 -> CdlConcealBabySwall
    | 13 -> CdlCounterAttack
    | 14 -> CdlDarkCloudCover
    | 15 -> CdlDoji
    | 16 -> CdlDojiStar
    | 17 -> CdlDragonflyDoji
    | 18 -> CdlEngulfing
    | 19 -> CdlEveningDojiStar
    | 20 -> CdlEveningStar
    | 21 -> CdlGapSideSideWhite
    | 22 -> CdlGravestoneDoji
    | 23 -> CdlHammer
    | 24 -> CdlHangingMan
    | 25 -> CdlHarami
    | 26 -> CdlHaramiCross
    | 27 -> CdlHighWave
    | 28 -> CdlHikkake
    | 29 -> CdlHikkakeMod
    | 30 -> CdlHomingPigeon
    | 31 -> CdlIdentical3Crows
    | 32 -> CdlInNeck
    | 33 -> CdlInvertedHammer
    | 34 -> CdlKicking
    | 35 -> CdlKickingByLength
    | 36 -> CdlLadderBottom
    | 37 -> CdlLongLeggedDoji
    | 38 -> CdlLongLine
    | 39 -> CdlMarubozu
    | 40 -> CdlMatchingLow
    | 41 -> CdlMatHold
    | 42 -> CdlMorningDojiStar
    | 43 -> CdlMorningStar
    | 44 -> CdlOnNeck
    | 45 -> CdlPiercing
    | 46 -> CdlRickshawMan
    | 47 -> CdlRiseFall3Methods
    | 48 -> CdlSeparatingLines
    | 49 -> CdlShootingStar
    | 50 -> CdlShortLine
    | 51 -> CdlSpinningTop
    | 52 -> CdlStalledPattern
    | 53 -> CdlStickSandwich
    | 54 -> CdlTakuri
    | 55 -> CdlTasukiGap
    | 56 -> CdlThrusting
    | 57 -> CdlTristar
    | 58 -> CdlUnique3River
    | 59 -> CdlUpsideGap2Crows
    | 60 -> CdlXSideGap3Methods
    | 61 -> MaxIndex
    | 62 -> MinIndex
    | 63 -> MinMaxIndex_Min
    | 64 -> MinMaxIndex_Max
    | _ -> failwith "Unknown int indicator"

  let to_int = function
    | Cdl2Crows -> 0
    | Cdl3BlackCrows -> 1
    | Cdl3Inside -> 2
    | Cdl3LineStrike -> 3
    | Cdl3Outside -> 4
    | Cdl3StarsInSouth -> 5
    | Cdl3WhiteSoldiers -> 6
    | CdlAbandonedBaby -> 7
    | CdlAdvanceBlock -> 8
    | CdlBeltHold -> 9
    | CdlBreakaway -> 10
    | CdlClosingMarubozu -> 11
    | CdlConcealBabySwall -> 12
    | CdlCounterAttack -> 13
    | CdlDarkCloudCover -> 14
    | CdlDoji -> 15
    | CdlDojiStar -> 16
    | CdlDragonflyDoji -> 17
    | CdlEngulfing -> 18
    | CdlEveningDojiStar -> 19
    | CdlEveningStar -> 20
    | CdlGapSideSideWhite -> 21
    | CdlGravestoneDoji -> 22
    | CdlHammer -> 23
    | CdlHangingMan -> 24
    | CdlHarami -> 25
    | CdlHaramiCross -> 26
    | CdlHighWave -> 27
    | CdlHikkake -> 28
    | CdlHikkakeMod -> 29
    | CdlHomingPigeon -> 30
    | CdlIdentical3Crows -> 31
    | CdlInNeck -> 32
    | CdlInvertedHammer -> 33
    | CdlKicking -> 34
    | CdlKickingByLength -> 35
    | CdlLadderBottom -> 36
    | CdlLongLeggedDoji -> 37
    | CdlLongLine -> 38
    | CdlMarubozu -> 39
    | CdlMatchingLow -> 40
    | CdlMatHold -> 41
    | CdlMorningDojiStar -> 42
    | CdlMorningStar -> 43
    | CdlOnNeck -> 44
    | CdlPiercing -> 45
    | CdlRickshawMan -> 46
    | CdlRiseFall3Methods -> 47
    | CdlSeparatingLines -> 48
    | CdlShootingStar -> 49
    | CdlShortLine -> 50
    | CdlSpinningTop -> 51
    | CdlStalledPattern -> 52
    | CdlStickSandwich -> 53
    | CdlTakuri -> 54
    | CdlTasukiGap -> 55
    | CdlThrusting -> 56
    | CdlTristar -> 57
    | CdlUnique3River -> 58
    | CdlUpsideGap2Crows -> 59
    | CdlXSideGap3Methods -> 60
    | MaxIndex -> 61
    | MinIndex -> 62
    | MinMaxIndex_Min -> 63
    | MinMaxIndex_Max -> 64
end

type t =
  | F of Float.t
  | I of Int.t
