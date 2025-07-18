module Float = struct
  type t =
    | UpperBBand of {
        timeperiod : int;
        nb_dev_up : float;
        nb_dev_dn : float;
        ma_type : Ma_type.t;
      }
    | MiddleBBand of {
        timeperiod : int;
        nb_dev_up : float;
        nb_dev_dn : float;
        ma_type : Ma_type.t;
      }
    | LowerBBand of {
        timeperiod : int;
        nb_dev_up : float;
        nb_dev_dn : float;
        ma_type : Ma_type.t;
      }
    | Dema of { timeperiod : int }
    | Ema of { timeperiod : int }
    | HtTrendline
    | Kama of { timeperiod : int }
    | Ma of { timeperiod : int; ma_type : Ma_type.t }
    | Mama of { fast_limit : float; slow_limit : float }
    | Mavp of { min_period : int; max_period : int; ma_type : Ma_type.t }
    | Midpoint of { timeperiod : int }
    | Midprice of { timeperiod : int }
    | Sar of { acceleration : float; maximum : float }
    | Sarext of {
        start_value : float;
        offset_on_reverse : float;
        acceleration_init_long : float;
        acceleration_long : float;
        acceleration_max_long : float;
        acceleration_init_short : float;
        acceleration_short : float;
        acceleration_max_short : float;
      }
    | Sma of { timeperiod : int }
    | T3 of { timeperiod : int; v_factor : float }
    | Tema of { timeperiod : int }
    | Trima of { timeperiod : int }
    | Wma of { timeperiod : int }
    | Adx of { timeperiod : int }
    | Adxr of { timeperiod : int }
    | Apo of { fast_period : int; slow_period : int; ma_type : Ma_type.t }
    | AroonOsc of { timeperiod : int }
    | Cci of { timeperiod : int }
    | Cmo of { timeperiod : int }
    | Dx of { timeperiod : int }
    | Macd_MACD of { fast_period : int; slow_period : int; signal_period : int }
    | Macd_MACDSignal of {
        fast_period : int;
        slow_period : int;
        signal_period : int;
      }
    | Macd_MACDHist of {
        fast_period : int;
        slow_period : int;
        signal_period : int;
      }
    | MacdExt_MACD of {
        fast_period : int;
        fast_ma_type : Ma_type.t;
        slow_period : int;
        slow_ma_type : Ma_type.t;
        signal_period : int;
        signal_ma_type : Ma_type.t;
      }
    | MacdExt_MACDSignal of {
        fast_period : int;
        fast_ma_type : Ma_type.t;
        slow_period : int;
        slow_ma_type : Ma_type.t;
        signal_period : int;
        signal_ma_type : Ma_type.t;
      }
    | MacdExt_MACDHist of {
        fast_period : int;
        fast_ma_type : Ma_type.t;
        slow_period : int;
        slow_ma_type : Ma_type.t;
        signal_period : int;
        signal_ma_type : Ma_type.t;
      }
    | MacdFix_MACD of { signal_period : int }
    | MacdFix_MACDSignal of { signal_period : int }
    | MacdFix_MACDHist of { signal_period : int }
    | Mfi of { timeperiod : int }
    | MinusDI of { timeperiod : int }
    | MinusDM of { timeperiod : int }
    | Mom of { timeperiod : int }
    | PlusDI of { timeperiod : int }
    | PlusDM of { timeperiod : int }
    | Ppo of { fast_period : int; slow_period : int; ma_type : Ma_type.t }
    | Roc of { timeperiod : int }
    | Rocp of { timeperiod : int }
    | Rocr of { timeperiod : int }
    | Rocr100 of { timeperiod : int }
    | Rsi of { timeperiod : int }
    | Stoch_SlowK of {
        fast_k_period : int;
        slow_k_period : int;
        slow_k_ma_type : Ma_type.t;
        slow_d_period : int;
        slow_d_ma_type : Ma_type.t;
      }
    | Stoch_SlowD of {
        fast_k_period : int;
        slow_k_period : int;
        slow_k_ma_type : Ma_type.t;
        slow_d_period : int;
        slow_d_ma_type : Ma_type.t;
      }
    | StochF_FastK of {
        fast_k_period : int;
        fast_d_period : int;
        fast_d_ma_type : Ma_type.t;
      }
    | StochF_FastD of {
        fast_k_period : int;
        fast_d_period : int;
        fast_d_ma_type : Ma_type.t;
      }
    | StochRsi_FastK of {
        timeperiod : int;
        fast_k_period : int;
        fast_d_period : int;
        fast_d_ma_type : Ma_type.t;
      }
    | StochRsi_FastD of {
        timeperiod : int;
        fast_k_period : int;
        fast_d_period : int;
        fast_d_ma_type : Ma_type.t;
      }
    | Trix of { timeperiod : int }
    | Ultosc of { timeperiod1 : int; timeperiod2 : int; timeperiod3 : int }
    | Willr of { timeperiod : int }
    | Ad
    | Adosc of { fast_period : int; slow_period : int }
    | Obv
    | Atr of { timeperiod : int }
    | Natr of { timeperiod : int }
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
    | Aroon_Down of { timeperiod : int }
    | Aroon_Up of { timeperiod : int }
    | Beta of { timeperiod : int }
    | Correl of { timeperiod : int }
    | Linearreg of { timeperiod : int }
    | LinearregAngle of { timeperiod : int }
    | LinearregIntercept of { timeperiod : int }
    | LinearregSlope of { timeperiod : int }
    | MinMax_Min of { timeperiod : int }
    | MinMax_Max of { timeperiod : int }
    | Stddev of { timeperiod : int; nb_dev : float }
    | Tsf of { timeperiod : int }
    | Var of { timeperiod : int; nb_dev : float }
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
    | Max of { timeperiod : int }
    | Min of { timeperiod : int }
    | Mult
    | Sub
    | Sum of { timeperiod : int }
    | Avgdev of { timeperiod : int }
    | Bop
    | Imi of { timeperiod : int }
    | Mama_MAMA of { fast_limit : float; slow_limit : float }
    | Mama_FAMA of { fast_limit : float; slow_limit : float }

  let to_string = function
    | UpperBBand _ -> "UpperBBand"
    | MiddleBBand _ -> "MiddleBBand"
    | LowerBBand _ -> "LowerBBand"
    | Dema _ -> "Dema"
    | Ema _ -> "Ema"
    | HtTrendline -> "HtTrendline"
    | Kama _ -> "Kama"
    | Ma _ -> "Ma"
    | Mama _ -> "Mama"
    | Mavp _ -> "Mavp"
    | Midpoint _ -> "Midpoint"
    | Midprice _ -> "Midprice"
    | Sar _ -> "Sar"
    | Sarext _ -> "Sarext"
    | Sma _ -> "Sma"
    | T3 _ -> "T3"
    | Tema _ -> "Tema"
    | Trima _ -> "Trima"
    | Wma _ -> "Wma"
    | Adx _ -> "Adx"
    | Adxr _ -> "Adxr"
    | Apo _ -> "Apo"
    | AroonOsc _ -> "AroonOsc"
    | Cci _ -> "Cci"
    | Cmo _ -> "Cmo"
    | Dx _ -> "Dx"
    | Macd_MACD _ -> "Macd_MACD"
    | Macd_MACDSignal _ -> "Macd_MACDSignal"
    | Macd_MACDHist _ -> "Macd_MACDHist"
    | MacdExt_MACD _ -> "MacdExt_MACD"
    | MacdExt_MACDSignal _ -> "MacdExt_MACDSignal"
    | MacdExt_MACDHist _ -> "MacdExt_MACDHist"
    | MacdFix_MACD _ -> "MacdFix_MACD"
    | MacdFix_MACDSignal _ -> "MacdFix_MACDSignal"
    | MacdFix_MACDHist _ -> "MacdFix_MACDHist"
    | Mfi _ -> "Mfi"
    | MinusDI _ -> "MinusDI"
    | MinusDM _ -> "MinusDM"
    | Mom _ -> "Mom"
    | PlusDI _ -> "PlusDI"
    | PlusDM _ -> "PlusDM"
    | Ppo _ -> "Ppo"
    | Roc _ -> "Roc"
    | Rocp _ -> "Rocp"
    | Rocr _ -> "Rocr"
    | Rocr100 _ -> "Rocr100"
    | Rsi _ -> "Rsi"
    | Stoch_SlowK _ -> "Stoch_SlowK"
    | Stoch_SlowD _ -> "Stoch_SlowD"
    | StochF_FastK _ -> "StochF_FastK"
    | StochF_FastD _ -> "StochF_FastD"
    | StochRsi_FastK _ -> "StochRsi_FastK"
    | StochRsi_FastD _ -> "StochRsi_FastD"
    | Trix _ -> "Trix"
    | Ultosc _ -> "Ultosc"
    | Willr _ -> "Willr"
    | Ad -> "Ad"
    | Adosc _ -> "Adosc"
    | Obv -> "Obv"
    | Atr _ -> "Atr"
    | Natr _ -> "Natr"
    | Trange -> "Trange"
    | AvgPrice -> "AvgPrice"
    | MedPrice -> "MedPrice"
    | TypPrice -> "TypPrice"
    | WclPrice -> "WclPrice"
    | HtDcPeriod -> "HtDcPeriod"
    | HtDcPhase -> "HtDcPhase"
    | HtPhasor_InPhase -> "HtPhasor_InPhase"
    | HtPhasor_Quadrature -> "HtPhasor_Quadrature"
    | HtSine_Sine -> "HtSine_Sine"
    | HtSine_LeadSine -> "HtSine_LeadSine"
    | Aroon_Down _ -> "Aroon_Down"
    | Aroon_Up _ -> "Aroon_Up"
    | Beta _ -> "Beta"
    | Correl _ -> "Correl"
    | Linearreg _ -> "Linearreg"
    | LinearregAngle _ -> "LinearregAngle"
    | LinearregIntercept _ -> "LinearregIntercept"
    | LinearregSlope _ -> "LinearregSlope"
    | MinMax_Min _ -> "MinMax_Min"
    | MinMax_Max _ -> "MinMax_Max"
    | Stddev _ -> "Stddev"
    | Tsf _ -> "Tsf"
    | Var _ -> "Var"
    | Acos -> "Acos"
    | Asin -> "Asin"
    | Atan -> "Atan"
    | Ceil -> "Ceil"
    | Cos -> "Cos"
    | Cosh -> "Cosh"
    | Exp -> "Exp"
    | Floor -> "Floor"
    | Ln -> "Ln"
    | Log10 -> "Log10"
    | Sin -> "Sin"
    | Sinh -> "Sinh"
    | Sqrt -> "Sqrt"
    | Tan -> "Tan"
    | Tanh -> "Tanh"
    | Add -> "Add"
    | Div -> "Div"
    | Max _ -> "Max"
    | Min _ -> "Min"
    | Mult -> "Mult"
    | Sub -> "Sub"
    | Sum _ -> "Sum"
    | Avgdev _ -> "Avgdev"
    | Bop -> "Bop"
    | Imi _ -> "Imi"
    | Mama_MAMA _ -> "Mama_MAMA"
    | Mama_FAMA _ -> "Mama_FAMA"
end

module Int = struct
  type t =
    | HtTrendMode
    | Cdl2Crows
    | Cdl3BlackCrows
    | Cdl3Inside
    | Cdl3LineStrike
    | Cdl3Outside
    | Cdl3StarsInSouth
    | Cdl3WhiteSoldiers
    | CdlAbandonedBaby of { penetration : float }
    | CdlAdvanceBlock
    | CdlBeltHold
    | CdlBreakaway
    | CdlClosingMarubozu
    | CdlConcealBabySwall
    | CdlCounterAttack
    | CdlDarkCloudCover of { penetration : float }
    | CdlDoji
    | CdlDojiStar
    | CdlDragonflyDoji
    | CdlEngulfing
    | CdlEveningDojiStar of { penetration : float }
    | CdlEveningStar of { penetration : float }
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
    | CdlMatHold of { penetration : float }
    | CdlMorningDojiStar of { penetration : float }
    | CdlMorningStar of { penetration : float }
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
    | MaxIndex of { timeperiod : int }
    | MinIndex of { timeperiod : int }
    | MinMaxIndex_Min of { timeperiod : int }
    | MinMaxIndex_Max of { timeperiod : int }

  let to_string = function
    | HtTrendMode -> "HtTrendMode"
    | Cdl2Crows -> "Cdl2Crows"
    | Cdl3BlackCrows -> "Cdl3BlackCrows"
    | Cdl3Inside -> "Cdl3Inside"
    | Cdl3LineStrike -> "Cdl3LineStrike"
    | Cdl3Outside -> "Cdl3Outside"
    | Cdl3StarsInSouth -> "Cdl3StarsInSouth"
    | Cdl3WhiteSoldiers -> "Cdl3WhiteSoldiers"
    | CdlAbandonedBaby _ -> "CdlAbandonedBaby"
    | CdlAdvanceBlock -> "CdlAdvanceBlock"
    | CdlBeltHold -> "CdlBeltHold"
    | CdlBreakaway -> "CdlBreakaway"
    | CdlClosingMarubozu -> "CdlClosingMarubozu"
    | CdlConcealBabySwall -> "CdlConcealBabySwall"
    | CdlCounterAttack -> "CdlCounterAttack"
    | CdlDarkCloudCover _ -> "CdlDarkCloudCover"
    | CdlDoji -> "CdlDoji"
    | CdlDojiStar -> "CdlDojiStar"
    | CdlDragonflyDoji -> "CdlDragonflyDoji"
    | CdlEngulfing -> "CdlEngulfing"
    | CdlEveningDojiStar _ -> "CdlEveningDojiStar"
    | CdlEveningStar _ -> "CdlEveningStar"
    | CdlGapSideSideWhite -> "CdlGapSideSideWhite"
    | CdlGravestoneDoji -> "CdlGravestoneDoji"
    | CdlHammer -> "CdlHammer"
    | CdlHangingMan -> "CdlHangingMan"
    | CdlHarami -> "CdlHarami"
    | CdlHaramiCross -> "CdlHaramiCross"
    | CdlHighWave -> "CdlHighWave"
    | CdlHikkake -> "CdlHikkake"
    | CdlHikkakeMod -> "CdlHikkakeMod"
    | CdlHomingPigeon -> "CdlHomingPigeon"
    | CdlIdentical3Crows -> "CdlIdentical3Crows"
    | CdlInNeck -> "CdlInNeck"
    | CdlInvertedHammer -> "CdlInvertedHammer"
    | CdlKicking -> "CdlKicking"
    | CdlKickingByLength -> "CdlKickingByLength"
    | CdlLadderBottom -> "CdlLadderBottom"
    | CdlLongLeggedDoji -> "CdlLongLeggedDoji"
    | CdlLongLine -> "CdlLongLine"
    | CdlMarubozu -> "CdlMarubozu"
    | CdlMatchingLow -> "CdlMatchingLow"
    | CdlMatHold _ -> "CdlMatHold"
    | CdlMorningDojiStar _ -> "CdlMorningDojiStar"
    | CdlMorningStar _ -> "CdlMorningStar"
    | CdlOnNeck -> "CdlOnNeck"
    | CdlPiercing -> "CdlPiercing"
    | CdlRickshawMan -> "CdlRickshawMan"
    | CdlRiseFall3Methods -> "CdlRiseFall3Methods"
    | CdlSeparatingLines -> "CdlSeparatingLines"
    | CdlShootingStar -> "CdlShootingStar"
    | CdlShortLine -> "CdlShortLine"
    | CdlSpinningTop -> "CdlSpinningTop"
    | CdlStalledPattern -> "CdlStalledPattern"
    | CdlStickSandwich -> "CdlStickSandwich"
    | CdlTakuri -> "CdlTakuri"
    | CdlTasukiGap -> "CdlTasukiGap"
    | CdlThrusting -> "CdlThrusting"
    | CdlTristar -> "CdlTristar"
    | CdlUnique3River -> "CdlUnique3River"
    | CdlUpsideGap2Crows -> "CdlUpsideGap2Crows"
    | CdlXSideGap3Methods -> "CdlXSideGap3Methods"
    | MaxIndex _ -> "MaxIndex"
    | MinIndex _ -> "MinIndex"
    | MinMaxIndex_Min _ -> "MinMaxIndex_Min"
    | MinMaxIndex_Max _ -> "MinMaxIndex_Max"
end

type t = F of Float.t | I of Int.t

let to_string = function
  | F f -> "F " ^ Float.to_string f
  | I i -> "I " ^ Int.to_string i

let pp = fun fmt x -> Format.fprintf fmt "@[%s@]" (to_string x)
