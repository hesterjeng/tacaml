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
    | Fama
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
    | Macd
    | MacdExt
    | MacdFix
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
    | Beta
    | Correl
    | Linearreg
    | LinearregAngle
    | LinearregIntercept
    | LinearregSlope
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
    | 9 -> Fama
    | 10 -> Mavp
    | 11 -> Midpoint
    | 12 -> Midprice
    | 13 -> Sar
    | 14 -> Sarext
    | 15 -> Sma
    | 16 -> T3
    | 17 -> Tema
    | 18 -> Trima
    | 19 -> Wma
    | 20 -> Adx
    | 21 -> Adxr
    | 22 -> Apo
    | 23 -> AroonOsc
    | 24 -> Cci
    | 25 -> Cmo
    | 26 -> Dx
    | 27 -> Macd
    | 28 -> MacdExt
    | 29 -> MacdFix
    | 30 -> Mfi
    | 31 -> MinusDI
    | 32 -> MinusDM
    | 33 -> Mom
    | 34 -> PlusDI
    | 35 -> PlusDM
    | 36 -> Ppo
    | 37 -> Roc
    | 38 -> Rocp
    | 39 -> Rocr
    | 40 -> Rocr100
    | 41 -> Rsi
    | 42 -> StochRsi_FastK
    | 43 -> StochRsi_FastD
    | 44 -> Trix
    | 45 -> Ultosc
    | 46 -> Willr
    | 47 -> Ad
    | 48 -> Adosc
    | 49 -> Obv
    | 50 -> Atr
    | 51 -> Natr
    | 52 -> Trange
    | 53 -> AvgPrice
    | 54 -> MedPrice
    | 55 -> TypPrice
    | 56 -> WclPrice
    | 57 -> HtDcPeriod
    | 58 -> HtDcPhase
    | 59 -> HtPhasor_InPhase
    | 60 -> HtPhasor_Quadrature
    | 61 -> HtSine_Sine
    | 62 -> HtSine_LeadSine
    | 63 -> HtTrendMode
    | 64 -> Beta
    | 65 -> Correl
    | 66 -> Linearreg
    | 67 -> LinearregAngle
    | 68 -> LinearregIntercept
    | 69 -> LinearregSlope
    | 70 -> Stddev
    | 71 -> Tsf
    | 72 -> Var
    | 73 -> Acos
    | 74 -> Asin
    | 75 -> Atan
    | 76 -> Ceil
    | 77 -> Cos
    | 78 -> Cosh
    | 79 -> Exp
    | 80 -> Floor
    | 81 -> Ln
    | 82 -> Log10
    | 83 -> Sin
    | 84 -> Sinh
    | 85 -> Sqrt
    | 86 -> Tan
    | 87 -> Tanh
    | 88 -> Add
    | 89 -> Div
    | 90 -> Max
    | 91 -> Min
    | 92 -> Mult
    | 93 -> Sub
    | 94 -> Sum
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
    | Fama -> 9
    | Mavp -> 10
    | Midpoint -> 11
    | Midprice -> 12
    | Sar -> 13
    | Sarext -> 14
    | Sma -> 15
    | T3 -> 16
    | Tema -> 17
    | Trima -> 18
    | Wma -> 19
    | Adx -> 20
    | Adxr -> 21
    | Apo -> 22
    | AroonOsc -> 23
    | Cci -> 24
    | Cmo -> 25
    | Dx -> 26
    | Macd -> 27
    | MacdExt -> 28
    | MacdFix -> 29
    | Mfi -> 30
    | MinusDI -> 31
    | MinusDM -> 32
    | Mom -> 33
    | PlusDI -> 34
    | PlusDM -> 35
    | Ppo -> 36
    | Roc -> 37
    | Rocp -> 38
    | Rocr -> 39
    | Rocr100 -> 40
    | Rsi -> 41
    | StochRsi_FastK -> 42
    | StochRsi_FastD -> 43
    | Trix -> 44
    | Ultosc -> 45
    | Willr -> 46
    | Ad -> 47
    | Adosc -> 48
    | Obv -> 49
    | Atr -> 50
    | Natr -> 51
    | Trange -> 52
    | AvgPrice -> 53
    | MedPrice -> 54
    | TypPrice -> 55
    | WclPrice -> 56
    | HtDcPeriod -> 57
    | HtDcPhase -> 58
    | HtPhasor_InPhase -> 59
    | HtPhasor_Quadrature -> 60
    | HtSine_Sine -> 61
    | HtSine_LeadSine -> 62
    | HtTrendMode -> 63
    | Beta -> 64
    | Correl -> 65
    | Linearreg -> 66
    | LinearregAngle -> 67
    | LinearregIntercept -> 68
    | LinearregSlope -> 69
    | Stddev -> 70
    | Tsf -> 71
    | Var -> 72
    | Acos -> 73
    | Asin -> 74
    | Atan -> 75
    | Ceil -> 76
    | Cos -> 77
    | Cosh -> 78
    | Exp -> 79
    | Floor -> 80
    | Ln -> 81
    | Log10 -> 82
    | Sin -> 83
    | Sinh -> 84
    | Sqrt -> 85
    | Tan -> 86
    | Tanh -> 87
    | Add -> 88
    | Div -> 89
    | Max -> 90
    | Min -> 91
    | Mult -> 92
    | Sub -> 93
    | Sum -> 94
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
end
