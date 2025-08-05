let hash_fold_int = Ppx_hash_lib.Std.Hash.fold_int
let hash_fold_float = Ppx_hash_lib.Std.Hash.fold_float
let compare_int = Int.compare
let compare_float = Float.compare

module Float = struct
  type t =
    | UpperAccBand of { timeperiod : int }
    | MiddleAccBand of { timeperiod : int }
    | LowerAccBand of { timeperiod : int }
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
  [@@deriving hash, compare, show { with_path = false }]
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
  [@@deriving hash, compare, show { with_path = false }]
end

type t = F of Float.t | I of Int.t [@@deriving hash, compare, show]

let show x =
  match x with
  | F x -> Float.show x
  | I x -> Int.show x

let equal x y = compare x y = 0

let upper_bband ?(timeperiod = 20) ?(nb_dev_up = 2.0) ?(nb_dev_dn = 2.0)
    ?(ma_type = Ma_type.Sma) () =
  F (Float.UpperBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type })

let middle_bband ?(timeperiod = 20) ?(nb_dev_up = 2.0) ?(nb_dev_dn = 2.0)
    ?(ma_type = Ma_type.Sma) () =
  F (Float.MiddleBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type })

let lower_bband ?(timeperiod = 20) ?(nb_dev_up = 2.0) ?(nb_dev_dn = 2.0)
    ?(ma_type = Ma_type.Sma) () =
  F (Float.LowerBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type })

let dema ?(timeperiod = 14) () = F (Float.Dema { timeperiod })
let ema ?(timeperiod = 14) () = F (Float.Ema { timeperiod })
let ht_trendline () = F Float.HtTrendline
let kama ?(timeperiod = 14) () = F (Float.Kama { timeperiod })

let ma ?(timeperiod = 14) ?(ma_type = Ma_type.Sma) () =
  F (Float.Ma { timeperiod; ma_type })

let mama ?(fast_limit = 0.5) ?(slow_limit = 0.05) () =
  F (Float.Mama { fast_limit; slow_limit })

let mavp ?(min_period = 2) ?(max_period = 30) ?(ma_type = Ma_type.Sma) () =
  F (Float.Mavp { min_period; max_period; ma_type })

let midpoint ?(timeperiod = 14) () = F (Float.Midpoint { timeperiod })
let midprice ?(timeperiod = 14) () = F (Float.Midprice { timeperiod })

let sar ?(acceleration = 0.02) ?(maximum = 0.2) () =
  F (Float.Sar { acceleration; maximum })

let sarext ?(start_value = 0.0) ?(offset_on_reverse = 0.0)
    ?(acceleration_init_long = 0.02) ?(acceleration_long = 0.02)
    ?(acceleration_max_long = 0.2) ?(acceleration_init_short = 0.02)
    ?(acceleration_short = 0.02) ?(acceleration_max_short = 0.2) () =
  F
    (Float.Sarext
       {
         start_value;
         offset_on_reverse;
         acceleration_init_long;
         acceleration_long;
         acceleration_max_long;
         acceleration_init_short;
         acceleration_short;
         acceleration_max_short;
       })

let sma ?(timeperiod = 8) () = F (Float.Sma { timeperiod })

let t3 ?(timeperiod = 14) ?(v_factor = 0.7) () =
  F (Float.T3 { timeperiod; v_factor })

let tema ?(timeperiod = 14) () = F (Float.Tema { timeperiod })
let trima ?(timeperiod = 14) () = F (Float.Trima { timeperiod })
let wma ?(timeperiod = 14) () = F (Float.Wma { timeperiod })
let adx ?(timeperiod = 14) () = F (Float.Adx { timeperiod })
let adxr ?(timeperiod = 14) () = F (Float.Adxr { timeperiod })

let apo ?(fast_period = 12) ?(slow_period = 26) ?(ma_type = Ma_type.Sma) () =
  F (Float.Apo { fast_period; slow_period; ma_type })

let aroon_osc ?(timeperiod = 14) () = F (Float.AroonOsc { timeperiod })
let cci ?(timeperiod = 14) () = F (Float.Cci { timeperiod })
let cmo ?(timeperiod = 14) () = F (Float.Cmo { timeperiod })
let dx ?(timeperiod = 14) () = F (Float.Dx { timeperiod })

let macd_macd ?(fast_period = 12) ?(slow_period = 26) ?(signal_period = 9) () =
  F (Float.Macd_MACD { fast_period; slow_period; signal_period })

let macd_signal ?(fast_period = 12) ?(slow_period = 26) ?(signal_period = 9) ()
    =
  F (Float.Macd_MACDSignal { fast_period; slow_period; signal_period })

let macd_hist ?(fast_period = 12) ?(slow_period = 26) ?(signal_period = 9) () =
  F (Float.Macd_MACDHist { fast_period; slow_period; signal_period })

let macd_ext_macd ?(fast_period = 12) ?(fast_ma_type = Ma_type.Ema)
    ?(slow_period = 26) ?(slow_ma_type = Ma_type.Ema) ?(signal_period = 9)
    ?(signal_ma_type = Ma_type.Ema) () =
  F
    (Float.MacdExt_MACD
       {
         fast_period;
         fast_ma_type;
         slow_period;
         slow_ma_type;
         signal_period;
         signal_ma_type;
       })

let macd_ext_signal ?(fast_period = 12) ?(fast_ma_type = Ma_type.Ema)
    ?(slow_period = 26) ?(slow_ma_type = Ma_type.Ema) ?(signal_period = 9)
    ?(signal_ma_type = Ma_type.Ema) () =
  F
    (Float.MacdExt_MACDSignal
       {
         fast_period;
         fast_ma_type;
         slow_period;
         slow_ma_type;
         signal_period;
         signal_ma_type;
       })

let macd_ext_hist ?(fast_period = 12) ?(fast_ma_type = Ma_type.Ema)
    ?(slow_period = 26) ?(slow_ma_type = Ma_type.Ema) ?(signal_period = 9)
    ?(signal_ma_type = Ma_type.Ema) () =
  F
    (Float.MacdExt_MACDHist
       {
         fast_period;
         fast_ma_type;
         slow_period;
         slow_ma_type;
         signal_period;
         signal_ma_type;
       })

let macd_fix_macd ?(signal_period = 9) () =
  F (Float.MacdFix_MACD { signal_period })

let macd_fix_signal ?(signal_period = 9) () =
  F (Float.MacdFix_MACDSignal { signal_period })

let macd_fix_hist ?(signal_period = 9) () =
  F (Float.MacdFix_MACDHist { signal_period })

let mfi ?(timeperiod = 14) () = F (Float.Mfi { timeperiod })
let minus_di ?(timeperiod = 14) () = F (Float.MinusDI { timeperiod })
let minus_dm ?(timeperiod = 14) () = F (Float.MinusDM { timeperiod })
let mom ?(timeperiod = 14) () = F (Float.Mom { timeperiod })
let plus_di ?(timeperiod = 14) () = F (Float.PlusDI { timeperiod })
let plus_dm ?(timeperiod = 14) () = F (Float.PlusDM { timeperiod })

let ppo ?(fast_period = 12) ?(slow_period = 26) ?(ma_type = Ma_type.Sma) () =
  F (Float.Ppo { fast_period; slow_period; ma_type })

let roc ?(timeperiod = 14) () = F (Float.Roc { timeperiod })
let rocp ?(timeperiod = 14) () = F (Float.Rocp { timeperiod })
let rocr ?(timeperiod = 14) () = F (Float.Rocr { timeperiod })
let rocr100 ?(timeperiod = 14) () = F (Float.Rocr100 { timeperiod })
let rsi ?(timeperiod = 14) () = F (Float.Rsi { timeperiod })

let stoch_slow_k ?(fast_k_period = 5) ?(slow_k_period = 3)
    ?(slow_k_ma_type = Ma_type.Sma) ?(slow_d_period = 3)
    ?(slow_d_ma_type = Ma_type.Sma) () =
  F
    (Float.Stoch_SlowK
       {
         fast_k_period;
         slow_k_period;
         slow_k_ma_type;
         slow_d_period;
         slow_d_ma_type;
       })

let stoch_slow_d ?(fast_k_period = 5) ?(slow_k_period = 3)
    ?(slow_k_ma_type = Ma_type.Sma) ?(slow_d_period = 3)
    ?(slow_d_ma_type = Ma_type.Sma) () =
  F
    (Float.Stoch_SlowD
       {
         fast_k_period;
         slow_k_period;
         slow_k_ma_type;
         slow_d_period;
         slow_d_ma_type;
       })

let stoch_f_fast_k ?(fast_k_period = 5) ?(fast_d_period = 3)
    ?(fast_d_ma_type = Ma_type.Sma) () =
  F (Float.StochF_FastK { fast_k_period; fast_d_period; fast_d_ma_type })

let stoch_f_fast_d ?(fast_k_period = 5) ?(fast_d_period = 3)
    ?(fast_d_ma_type = Ma_type.Sma) () =
  F (Float.StochF_FastD { fast_k_period; fast_d_period; fast_d_ma_type })

let stoch_rsi_fast_k ?(timeperiod = 14) ?(fast_k_period = 5)
    ?(fast_d_period = 3) ?(fast_d_ma_type = Ma_type.Sma) () =
  F
    (Float.StochRsi_FastK
       { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type })

let stoch_rsi_fast_d ?(timeperiod = 14) ?(fast_k_period = 5)
    ?(fast_d_period = 3) ?(fast_d_ma_type = Ma_type.Sma) () =
  F
    (Float.StochRsi_FastD
       { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type })

let trix ?(timeperiod = 14) () = F (Float.Trix { timeperiod })

let ultosc ?(timeperiod1 = 7) ?(timeperiod2 = 14) ?(timeperiod3 = 28) () =
  F (Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 })

let willr ?(timeperiod = 14) () = F (Float.Willr { timeperiod })
let ad () = F Float.Ad

let adosc ?(fast_period = 3) ?(slow_period = 10) () =
  F (Float.Adosc { fast_period; slow_period })

let obv () = F Float.Obv
let atr ?(timeperiod = 14) () = F (Float.Atr { timeperiod })
let natr ?(timeperiod = 14) () = F (Float.Natr { timeperiod })
let trange () = F Float.Trange
let avg_price () = F Float.AvgPrice
let med_price () = F Float.MedPrice
let typ_price () = F Float.TypPrice
let wcl_price () = F Float.WclPrice
let ht_dc_period () = F Float.HtDcPeriod
let ht_dc_phase () = F Float.HtDcPhase
let ht_phasor_inphase () = F Float.HtPhasor_InPhase
let ht_phasor_quadrature () = F Float.HtPhasor_Quadrature
let ht_sine_sine () = F Float.HtSine_Sine
let ht_sine_leadsine () = F Float.HtSine_LeadSine
let aroon_down ?(timeperiod = 14) () = F (Float.Aroon_Down { timeperiod })
let aroon_up ?(timeperiod = 14) () = F (Float.Aroon_Up { timeperiod })
let beta ?(timeperiod = 5) () = F (Float.Beta { timeperiod })
let correl ?(timeperiod = 30) () = F (Float.Correl { timeperiod })
let linearreg ?(timeperiod = 14) () = F (Float.Linearreg { timeperiod })

let linearreg_angle ?(timeperiod = 14) () =
  F (Float.LinearregAngle { timeperiod })

let linearreg_intercept ?(timeperiod = 14) () =
  F (Float.LinearregIntercept { timeperiod })

let linearreg_slope ?(timeperiod = 14) () =
  F (Float.LinearregSlope { timeperiod })

let min_max_min ?(timeperiod = 30) () = F (Float.MinMax_Min { timeperiod })
let min_max_max ?(timeperiod = 30) () = F (Float.MinMax_Max { timeperiod })

let stddev ?(timeperiod = 5) ?(nb_dev = 1.0) () =
  F (Float.Stddev { timeperiod; nb_dev })

let tsf ?(timeperiod = 14) () = F (Float.Tsf { timeperiod })

let var ?(timeperiod = 5) ?(nb_dev = 1.0) () =
  F (Float.Var { timeperiod; nb_dev })

let acos () = F Float.Acos
let asin () = F Float.Asin
let atan () = F Float.Atan
let ceil () = F Float.Ceil
let cos () = F Float.Cos
let cosh () = F Float.Cosh
let exp () = F Float.Exp
let floor () = F Float.Floor
let ln () = F Float.Ln
let log10 () = F Float.Log10
let sin () = F Float.Sin
let sinh () = F Float.Sinh
let sqrt () = F Float.Sqrt
let tan () = F Float.Tan
let tanh () = F Float.Tanh
let add () = F Float.Add
let div () = F Float.Div
let max ?(timeperiod = 30) () = F (Float.Max { timeperiod })
let min ?(timeperiod = 30) () = F (Float.Min { timeperiod })
let mult () = F Float.Mult
let sub () = F Float.Sub
let sum ?(timeperiod = 30) () = F (Float.Sum { timeperiod })
let avgdev ?(timeperiod = 14) () = F (Float.Avgdev { timeperiod })
let bop () = F Float.Bop
let imi ?(timeperiod = 14) () = F (Float.Imi { timeperiod })

let mama_mama ?(fast_limit = 0.5) ?(slow_limit = 0.05) () =
  F (Float.Mama_MAMA { fast_limit; slow_limit })

let mama_fama ?(fast_limit = 0.5) ?(slow_limit = 0.05) () =
  F (Float.Mama_FAMA { fast_limit; slow_limit })

let ht_trend_mode () = I Int.HtTrendMode
let cdl_2crows () = I Int.Cdl2Crows
let cdl_3blackcrows () = I Int.Cdl3BlackCrows
let cdl_3inside () = I Int.Cdl3Inside
let cdl_3linestrike () = I Int.Cdl3LineStrike
let cdl_3outside () = I Int.Cdl3Outside
let cdl_3starsinsouth () = I Int.Cdl3StarsInSouth
let cdl_3whitesoldiers () = I Int.Cdl3WhiteSoldiers

let cdl_abandonedbaby ?(penetration = 0.3) () =
  I (Int.CdlAbandonedBaby { penetration })

let cdl_advanceblock () = I Int.CdlAdvanceBlock
let cdl_belthold () = I Int.CdlBeltHold
let cdl_breakaway () = I Int.CdlBreakaway
let cdl_closingmarubozu () = I Int.CdlClosingMarubozu
let cdl_concealbabyswall () = I Int.CdlConcealBabySwall
let cdl_counterattack () = I Int.CdlCounterAttack

let cdl_darkcloudcover ?(penetration = 0.5) () =
  I (Int.CdlDarkCloudCover { penetration })

let cdl_doji () = I Int.CdlDoji
let cdl_dojistar () = I Int.CdlDojiStar
let cdl_dragonflydoji () = I Int.CdlDragonflyDoji
let cdl_engulfing () = I Int.CdlEngulfing

let cdl_eveningdojistar ?(penetration = 0.3) () =
  I (Int.CdlEveningDojiStar { penetration })

let cdl_eveningstar ?(penetration = 0.3) () =
  I (Int.CdlEveningStar { penetration })

let cdl_gap_side_side_white () = I Int.CdlGapSideSideWhite
let cdl_gravestonedoji () = I Int.CdlGravestoneDoji
let cdl_hammer () = I Int.CdlHammer
let cdl_hangingman () = I Int.CdlHangingMan
let cdl_harami () = I Int.CdlHarami
let cdl_haramicross () = I Int.CdlHaramiCross
let cdl_highwave () = I Int.CdlHighWave
let cdl_hikkake () = I Int.CdlHikkake
let cdl_hikkakemod () = I Int.CdlHikkakeMod
let cdl_homingpigeon () = I Int.CdlHomingPigeon
let cdl_identical3crows () = I Int.CdlIdentical3Crows
let cdl_inneck () = I Int.CdlInNeck
let cdl_invertedhammer () = I Int.CdlInvertedHammer
let cdl_kicking () = I Int.CdlKicking
let cdl_kickingbylength () = I Int.CdlKickingByLength
let cdl_ladderbottom () = I Int.CdlLadderBottom
let cdl_longleggedDoji () = I Int.CdlLongLeggedDoji
let cdl_longline () = I Int.CdlLongLine
let cdl_marubozu () = I Int.CdlMarubozu
let cdl_matchinglow () = I Int.CdlMatchingLow
let cdl_mathold ?(penetration = 0.5) () = I (Int.CdlMatHold { penetration })

let cdl_morningdojistar ?(penetration = 0.3) () =
  I (Int.CdlMorningDojiStar { penetration })

let cdl_morningstar ?(penetration = 0.3) () =
  I (Int.CdlMorningStar { penetration })

let cdl_onneck () = I Int.CdlOnNeck
let cdl_piercing () = I Int.CdlPiercing
let cdl_rickshawman () = I Int.CdlRickshawMan
let cdl_risefall3methods () = I Int.CdlRiseFall3Methods
let cdl_separatinglines () = I Int.CdlSeparatingLines
let cdl_shootingstar () = I Int.CdlShootingStar
let cdl_shortline () = I Int.CdlShortLine
let cdl_spinningtop () = I Int.CdlSpinningTop
let cdl_stalledpattern () = I Int.CdlStalledPattern
let cdl_sticksandwich () = I Int.CdlStickSandwich
let cdl_takuri () = I Int.CdlTakuri
let cdl_tasukigap () = I Int.CdlTasukiGap
let cdl_thrusting () = I Int.CdlThrusting
let cdl_tristar () = I Int.CdlTristar
let cdl_unique3river () = I Int.CdlUnique3River
let cdl_upsidegap2crows () = I Int.CdlUpsideGap2Crows
let cdl_xsidegap3methods () = I Int.CdlXSideGap3Methods
let max_index ?(timeperiod = 30) () = I (Int.MaxIndex { timeperiod })
let min_index ?(timeperiod = 30) () = I (Int.MinIndex { timeperiod })

let min_max_index_min ?(timeperiod = 30) () =
  I (Int.MinMaxIndex_Min { timeperiod })

let min_max_index_max ?(timeperiod = 30) () =
  I (Int.MinMaxIndex_Max { timeperiod })

let accbands_upper ?(timeperiod = 20) () = F (Float.UpperAccBand { timeperiod })

let accbands_middle ?(timeperiod = 20) () =
  F (Float.MiddleAccBand { timeperiod })

let accbands_lower ?(timeperiod = 20) () = F (Float.LowerAccBand { timeperiod })

module Raw = struct
  let upper_bband timeperiod nb_dev_up nb_dev_dn =
    F (Float.UpperBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type = Ma_type.Ema })

  let middle_bband timeperiod nb_dev_up nb_dev_dn =
    F (Float.MiddleBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type = Ma_type.Ema })

  let lower_bband timeperiod nb_dev_up nb_dev_dn =
    F (Float.LowerBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type = Ma_type.Ema })

  let dema timeperiod = F (Float.Dema { timeperiod })
  let ema timeperiod = F (Float.Ema { timeperiod })
  let ht_trendline = F Float.HtTrendline
  let kama timeperiod = F (Float.Kama { timeperiod })

  let ma timeperiod =
    F (Float.Ma { timeperiod; ma_type = Ma_type.Ema })

  let mama fast_limit slow_limit =
    F (Float.Mama { fast_limit; slow_limit })

  let mavp min_period max_period =
    F (Float.Mavp { min_period; max_period; ma_type = Ma_type.Ema })

  let midpoint timeperiod = F (Float.Midpoint { timeperiod })
  let midprice timeperiod = F (Float.Midprice { timeperiod })

  let sar acceleration maximum =
    F (Float.Sar { acceleration; maximum })

  let sarext start_value offset_on_reverse acceleration_init_long acceleration_long acceleration_max_long acceleration_init_short acceleration_short acceleration_max_short =
    F
      (Float.Sarext
         {
           start_value;
           offset_on_reverse;
           acceleration_init_long;
           acceleration_long;
           acceleration_max_long;
           acceleration_init_short;
           acceleration_short;
           acceleration_max_short;
         })

  let sma timeperiod = F (Float.Sma { timeperiod })

  let t3 timeperiod v_factor =
    F (Float.T3 { timeperiod; v_factor })

  let tema timeperiod = F (Float.Tema { timeperiod })
  let trima timeperiod = F (Float.Trima { timeperiod })
  let wma timeperiod = F (Float.Wma { timeperiod })
  let adx timeperiod = F (Float.Adx { timeperiod })
  let adxr timeperiod = F (Float.Adxr { timeperiod })

  let apo fast_period slow_period =
    F (Float.Apo { fast_period; slow_period; ma_type = Ma_type.Ema })

  let aroon_osc timeperiod = F (Float.AroonOsc { timeperiod })
  let cci timeperiod = F (Float.Cci { timeperiod })
  let cmo timeperiod = F (Float.Cmo { timeperiod })
  let dx timeperiod = F (Float.Dx { timeperiod })

  let macd_macd fast_period slow_period signal_period =
    F (Float.Macd_MACD { fast_period; slow_period; signal_period })

  let macd_signal fast_period slow_period signal_period =
    F (Float.Macd_MACDSignal { fast_period; slow_period; signal_period })

  let macd_hist fast_period slow_period signal_period =
    F (Float.Macd_MACDHist { fast_period; slow_period; signal_period })

  let macd_ext_macd fast_period slow_period signal_period =
    F
      (Float.MacdExt_MACD
         {
           fast_period;
           fast_ma_type = Ma_type.Ema;
           slow_period;
           slow_ma_type = Ma_type.Ema;
           signal_period;
           signal_ma_type = Ma_type.Ema;
         })

  let macd_ext_signal fast_period slow_period signal_period =
    F
      (Float.MacdExt_MACDSignal
         {
           fast_period;
           fast_ma_type = Ma_type.Ema;
           slow_period;
           slow_ma_type = Ma_type.Ema;
           signal_period;
           signal_ma_type = Ma_type.Ema;
         })

  let macd_ext_hist fast_period slow_period signal_period =
    F
      (Float.MacdExt_MACDHist
         {
           fast_period;
           fast_ma_type = Ma_type.Ema;
           slow_period;
           slow_ma_type = Ma_type.Ema;
           signal_period;
           signal_ma_type = Ma_type.Ema;
         })

  let macd_fix_macd signal_period =
    F (Float.MacdFix_MACD { signal_period })

  let macd_fix_signal signal_period =
    F (Float.MacdFix_MACDSignal { signal_period })

  let macd_fix_hist signal_period =
    F (Float.MacdFix_MACDHist { signal_period })

  let mfi timeperiod = F (Float.Mfi { timeperiod })
  let minus_di timeperiod = F (Float.MinusDI { timeperiod })
  let minus_dm timeperiod = F (Float.MinusDM { timeperiod })
  let mom timeperiod = F (Float.Mom { timeperiod })
  let plus_di timeperiod = F (Float.PlusDI { timeperiod })
  let plus_dm timeperiod = F (Float.PlusDM { timeperiod })

  let ppo fast_period slow_period =
    F (Float.Ppo { fast_period; slow_period; ma_type = Ma_type.Ema })

  let roc timeperiod = F (Float.Roc { timeperiod })
  let rocp timeperiod = F (Float.Rocp { timeperiod })
  let rocr timeperiod = F (Float.Rocr { timeperiod })
  let rocr100 timeperiod = F (Float.Rocr100 { timeperiod })
  let rsi timeperiod = F (Float.Rsi { timeperiod })

  let stoch_slow_k fast_k_period slow_k_period slow_d_period =
    F
      (Float.Stoch_SlowK
         {
           fast_k_period;
           slow_k_period;
           slow_k_ma_type = Ma_type.Ema;
           slow_d_period;
           slow_d_ma_type = Ma_type.Ema;
         })

  let stoch_slow_d fast_k_period slow_k_period slow_d_period =
    F
      (Float.Stoch_SlowD
         {
           fast_k_period;
           slow_k_period;
           slow_k_ma_type = Ma_type.Ema;
           slow_d_period;
           slow_d_ma_type = Ma_type.Ema;
         })

  let stoch_f_fast_k fast_k_period fast_d_period =
    F (Float.StochF_FastK { fast_k_period; fast_d_period; fast_d_ma_type = Ma_type.Ema })

  let stoch_f_fast_d fast_k_period fast_d_period =
    F (Float.StochF_FastD { fast_k_period; fast_d_period; fast_d_ma_type = Ma_type.Ema })

  let stoch_rsi_fast_k timeperiod fast_k_period fast_d_period =
    F
      (Float.StochRsi_FastK
         { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type = Ma_type.Ema })

  let stoch_rsi_fast_d timeperiod fast_k_period fast_d_period =
    F
      (Float.StochRsi_FastD
         { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type = Ma_type.Ema })

  let trix timeperiod = F (Float.Trix { timeperiod })

  let ultosc timeperiod1 timeperiod2 timeperiod3 =
    F (Float.Ultosc { timeperiod1; timeperiod2; timeperiod3 })

  let willr timeperiod = F (Float.Willr { timeperiod })
  let ad = F Float.Ad

  let adosc fast_period slow_period =
    F (Float.Adosc { fast_period; slow_period })

  let obv = F Float.Obv
  let atr timeperiod = F (Float.Atr { timeperiod })
  let natr timeperiod = F (Float.Natr { timeperiod })
  let trange = F Float.Trange
  let avg_price = F Float.AvgPrice
  let med_price = F Float.MedPrice
  let typ_price = F Float.TypPrice
  let wcl_price = F Float.WclPrice
  let ht_dc_period = F Float.HtDcPeriod
  let ht_dc_phase = F Float.HtDcPhase
  let ht_phasor_inphase = F Float.HtPhasor_InPhase
  let ht_phasor_quadrature = F Float.HtPhasor_Quadrature
  let ht_sine_sine = F Float.HtSine_Sine
  let ht_sine_leadsine = F Float.HtSine_LeadSine
  let aroon_down timeperiod = F (Float.Aroon_Down { timeperiod })
  let aroon_up timeperiod = F (Float.Aroon_Up { timeperiod })
  let beta timeperiod = F (Float.Beta { timeperiod })
  let correl timeperiod = F (Float.Correl { timeperiod })
  let linearreg timeperiod = F (Float.Linearreg { timeperiod })

  let linearreg_angle timeperiod =
    F (Float.LinearregAngle { timeperiod })

  let linearreg_intercept timeperiod =
    F (Float.LinearregIntercept { timeperiod })

  let linearreg_slope timeperiod =
    F (Float.LinearregSlope { timeperiod })

  let min_max_min timeperiod = F (Float.MinMax_Min { timeperiod })
  let min_max_max timeperiod = F (Float.MinMax_Max { timeperiod })

  let stddev timeperiod nb_dev =
    F (Float.Stddev { timeperiod; nb_dev })

  let tsf timeperiod = F (Float.Tsf { timeperiod })

  let var timeperiod nb_dev =
    F (Float.Var { timeperiod; nb_dev })

  let acos = F Float.Acos
  let asin = F Float.Asin
  let atan = F Float.Atan
  let ceil = F Float.Ceil
  let cos = F Float.Cos
  let cosh = F Float.Cosh
  let exp = F Float.Exp
  let floor = F Float.Floor
  let ln = F Float.Ln
  let log10 = F Float.Log10
  let sin = F Float.Sin
  let sinh = F Float.Sinh
  let sqrt = F Float.Sqrt
  let tan = F Float.Tan
  let tanh = F Float.Tanh
  let add = F Float.Add
  let div = F Float.Div
  let max timeperiod = F (Float.Max { timeperiod })
  let min timeperiod = F (Float.Min { timeperiod })
  let mult = F Float.Mult
  let sub = F Float.Sub
  let sum timeperiod = F (Float.Sum { timeperiod })
  let avgdev timeperiod = F (Float.Avgdev { timeperiod })
  let bop = F Float.Bop
  let imi timeperiod = F (Float.Imi { timeperiod })

  let mama_mama fast_limit slow_limit =
    F (Float.Mama_MAMA { fast_limit; slow_limit })

  let mama_fama fast_limit slow_limit =
    F (Float.Mama_FAMA { fast_limit; slow_limit })

  let accbands_upper timeperiod = F (Float.UpperAccBand { timeperiod })
  let accbands_middle timeperiod = F (Float.MiddleAccBand { timeperiod })
  let accbands_lower timeperiod = F (Float.LowerAccBand { timeperiod })

  let ht_trend_mode = I Int.HtTrendMode
  let cdl_2crows = I Int.Cdl2Crows
  let cdl_3blackcrows = I Int.Cdl3BlackCrows
  let cdl_3inside = I Int.Cdl3Inside
  let cdl_3linestrike = I Int.Cdl3LineStrike
  let cdl_3outside = I Int.Cdl3Outside
  let cdl_3starsinsouth = I Int.Cdl3StarsInSouth
  let cdl_3whitesoldiers = I Int.Cdl3WhiteSoldiers

  let cdl_abandonedbaby penetration =
    I (Int.CdlAbandonedBaby { penetration })

  let cdl_advanceblock = I Int.CdlAdvanceBlock
  let cdl_belthold = I Int.CdlBeltHold
  let cdl_breakaway = I Int.CdlBreakaway
  let cdl_closingmarubozu = I Int.CdlClosingMarubozu
  let cdl_concealbabyswall = I Int.CdlConcealBabySwall
  let cdl_counterattack = I Int.CdlCounterAttack

  let cdl_darkcloudcover penetration =
    I (Int.CdlDarkCloudCover { penetration })

  let cdl_doji = I Int.CdlDoji
  let cdl_dojistar = I Int.CdlDojiStar
  let cdl_dragonflydoji = I Int.CdlDragonflyDoji
  let cdl_engulfing = I Int.CdlEngulfing

  let cdl_eveningdojistar penetration =
    I (Int.CdlEveningDojiStar { penetration })

  let cdl_eveningstar penetration =
    I (Int.CdlEveningStar { penetration })

  let cdl_gap_side_side_white = I Int.CdlGapSideSideWhite
  let cdl_gravestonedoji = I Int.CdlGravestoneDoji
  let cdl_hammer = I Int.CdlHammer
  let cdl_hangingman = I Int.CdlHangingMan
  let cdl_harami = I Int.CdlHarami
  let cdl_haramicross = I Int.CdlHaramiCross
  let cdl_highwave = I Int.CdlHighWave
  let cdl_hikkake = I Int.CdlHikkake
  let cdl_hikkakemod = I Int.CdlHikkakeMod
  let cdl_homingpigeon = I Int.CdlHomingPigeon
  let cdl_identical3crows = I Int.CdlIdentical3Crows
  let cdl_inneck = I Int.CdlInNeck
  let cdl_invertedhammer = I Int.CdlInvertedHammer
  let cdl_kicking = I Int.CdlKicking
  let cdl_kickingbylength = I Int.CdlKickingByLength
  let cdl_ladderbottom = I Int.CdlLadderBottom
  let cdl_longleggedDoji = I Int.CdlLongLeggedDoji
  let cdl_longline = I Int.CdlLongLine
  let cdl_marubozu = I Int.CdlMarubozu
  let cdl_matchinglow = I Int.CdlMatchingLow
  let cdl_mathold penetration = I (Int.CdlMatHold { penetration })

  let cdl_morningdojistar penetration =
    I (Int.CdlMorningDojiStar { penetration })

  let cdl_morningstar penetration =
    I (Int.CdlMorningStar { penetration })

  let cdl_onneck = I Int.CdlOnNeck
  let cdl_piercing = I Int.CdlPiercing
  let cdl_rickshawman = I Int.CdlRickshawMan
  let cdl_risefall3methods = I Int.CdlRiseFall3Methods
  let cdl_separatinglines = I Int.CdlSeparatingLines
  let cdl_shootingstar = I Int.CdlShootingStar
  let cdl_shortline = I Int.CdlShortLine
  let cdl_spinningtop = I Int.CdlSpinningTop
  let cdl_stalledpattern = I Int.CdlStalledPattern
  let cdl_sticksandwich = I Int.CdlStickSandwich
  let cdl_takuri = I Int.CdlTakuri
  let cdl_tasukigap = I Int.CdlTasukiGap
  let cdl_thrusting = I Int.CdlThrusting
  let cdl_tristar = I Int.CdlTristar
  let cdl_unique3river = I Int.CdlUnique3River
  let cdl_upsidegap2crows = I Int.CdlUpsideGap2Crows
  let cdl_xsidegap3methods = I Int.CdlXSideGap3Methods
  let max_index timeperiod = I (Int.MaxIndex { timeperiod })
  let min_index timeperiod = I (Int.MinIndex { timeperiod })

  let min_max_index_min timeperiod =
    I (Int.MinMaxIndex_Min { timeperiod })

  let min_max_index_max timeperiod =
    I (Int.MinMaxIndex_Max { timeperiod })
end
