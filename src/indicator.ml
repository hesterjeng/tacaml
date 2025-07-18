let hash_fold_int = Ppx_hash_lib.Std.Hash.fold_int
let hash_fold_float = Ppx_hash_lib.Std.Hash.fold_float
let compare_int = Int.compare
let compare_float = Float.compare

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
  [@@deriving hash, compare, show]
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
  [@@deriving hash, compare, show]
end

type t = F of Float.t | I of Int.t [@@deriving hash, compare, show]
