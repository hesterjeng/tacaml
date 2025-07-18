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

  let hash = function
    | UpperBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
      Hash.combine5 (Hash.int 0) (Hash.int timeperiod) (Hash.poly nb_dev_up)
        (Hash.poly nb_dev_dn)
        (Hash.int (Ma_type.to_int ma_type))
    | MiddleBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
      Hash.combine5 (Hash.int 1) (Hash.int timeperiod) (Hash.poly nb_dev_up)
        (Hash.poly nb_dev_dn)
        (Hash.int (Ma_type.to_int ma_type))
    | LowerBBand { timeperiod; nb_dev_up; nb_dev_dn; ma_type } ->
      Hash.combine5 (Hash.int 2) (Hash.int timeperiod) (Hash.poly nb_dev_up)
        (Hash.poly nb_dev_dn)
        (Hash.int (Ma_type.to_int ma_type))
    | Dema { timeperiod } -> Hash.combine2 (Hash.int 3) (Hash.int timeperiod)
    | Ema { timeperiod } -> Hash.combine2 (Hash.int 4) (Hash.int timeperiod)
    | HtTrendline -> Hash.int 5
    | Kama { timeperiod } -> Hash.combine2 (Hash.int 0) (Hash.int timeperiod)
    | Ma { timeperiod; ma_type } ->
      Hash.combine3 (Hash.int 1) (Hash.int timeperiod)
        (Hash.int (Ma_type.to_int ma_type))
    | Mama { fast_limit; slow_limit } ->
      Hash.combine3 (Hash.int 2) (Hash.poly fast_limit) (Hash.poly slow_limit)
    | Mavp { min_period; max_period; ma_type } ->
      Hash.combine4 (Hash.int 3) (Hash.int min_period) (Hash.int max_period)
        (Hash.int (Ma_type.to_int ma_type))
    | Midpoint { timeperiod } ->
      Hash.combine2 (Hash.int 4) (Hash.int timeperiod)
    | Midprice { timeperiod } ->
      Hash.combine2 (Hash.int 5) (Hash.int timeperiod)
    | Sar { acceleration; maximum } ->
      Hash.combine3 (Hash.int 6) (Hash.poly acceleration) (Hash.poly maximum)
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
      Hash.combine2 (Hash.int 7)
        (Hash.combine6 (Hash.poly start_value)
           (Hash.poly offset_on_reverse)
           (Hash.poly acceleration_init_long)
           (Hash.poly acceleration_long)
           (Hash.poly acceleration_max_long)
           (Hash.combine3
              (Hash.poly acceleration_init_short)
              (Hash.poly acceleration_short)
              (Hash.poly acceleration_max_short)))
    | Sma { timeperiod } -> Hash.combine2 (Hash.int 8) (Hash.int timeperiod)
    | T3 { timeperiod; v_factor } ->
      Hash.combine3 (Hash.int 9) (Hash.int timeperiod) (Hash.poly v_factor)
    | Tema { timeperiod } -> Hash.combine2 (Hash.int 10) (Hash.int timeperiod)
    | Trima { timeperiod } -> Hash.combine2 (Hash.int 11) (Hash.int timeperiod)
    | Wma { timeperiod } -> Hash.combine2 (Hash.int 12) (Hash.int timeperiod)
    | Adx { timeperiod } -> Hash.combine2 (Hash.int 13) (Hash.int timeperiod)
    | Adxr { timeperiod } -> Hash.combine2 (Hash.int 14) (Hash.int timeperiod)
    | Apo { fast_period; slow_period; ma_type } ->
      Hash.combine4 (Hash.int 15) (Hash.int fast_period) (Hash.int slow_period)
        (Hash.int (Ma_type.to_int ma_type))
    | AroonOsc { timeperiod } ->
      Hash.combine2 (Hash.int 16) (Hash.int timeperiod)
    | Cci { timeperiod } -> Hash.combine2 (Hash.int 17) (Hash.int timeperiod)
    | Cmo { timeperiod } -> Hash.combine2 (Hash.int 18) (Hash.int timeperiod)
    | Dx { timeperiod } -> Hash.combine2 (Hash.int 19) (Hash.int timeperiod)
    | Macd_MACD { fast_period; slow_period; signal_period } ->
      Hash.combine4 (Hash.int 20) (Hash.int fast_period) (Hash.int slow_period)
        (Hash.int signal_period)
    | Macd_MACDSignal { fast_period; slow_period; signal_period } ->
      Hash.combine4 (Hash.int 21) (Hash.int fast_period) (Hash.int slow_period)
        (Hash.int signal_period)
    | Macd_MACDHist { fast_period; slow_period; signal_period } ->
      Hash.combine4 (Hash.int 22) (Hash.int fast_period) (Hash.int slow_period)
        (Hash.int signal_period)
    | MacdExt_MACD
        {
          fast_period;
          fast_ma_type;
          slow_period;
          slow_ma_type;
          signal_period;
          signal_ma_type;
        } ->
      Hash.combine6 (Hash.int 23) (Hash.int fast_period)
        (Hash.int (Ma_type.to_int fast_ma_type))
        (Hash.int slow_period)
        (Hash.int (Ma_type.to_int slow_ma_type))
        (Hash.combine2 (Hash.int signal_period)
           (Hash.int (Ma_type.to_int signal_ma_type)))
    | MacdExt_MACDSignal
        {
          fast_period;
          fast_ma_type;
          slow_period;
          slow_ma_type;
          signal_period;
          signal_ma_type;
        } ->
      Hash.combine6 (Hash.int 24) (Hash.int fast_period)
        (Hash.int (Ma_type.to_int fast_ma_type))
        (Hash.int slow_period)
        (Hash.int (Ma_type.to_int slow_ma_type))
        (Hash.combine2 (Hash.int signal_period)
           (Hash.int (Ma_type.to_int signal_ma_type)))
    | MacdExt_MACDHist
        {
          fast_period;
          fast_ma_type;
          slow_period;
          slow_ma_type;
          signal_period;
          signal_ma_type;
        } ->
      Hash.combine6 (Hash.int 25) (Hash.int fast_period)
        (Hash.int (Ma_type.to_int fast_ma_type))
        (Hash.int slow_period)
        (Hash.int (Ma_type.to_int slow_ma_type))
        (Hash.combine2 (Hash.int signal_period)
           (Hash.int (Ma_type.to_int signal_ma_type)))
    | MacdFix_MACD { signal_period } ->
      Hash.combine2 (Hash.int 26) (Hash.int signal_period)
    | MacdFix_MACDSignal { signal_period } ->
      Hash.combine2 (Hash.int 27) (Hash.int signal_period)
    | MacdFix_MACDHist { signal_period } ->
      Hash.combine2 (Hash.int 28) (Hash.int signal_period)
    | Mfi { timeperiod } -> Hash.combine2 (Hash.int 29) (Hash.int timeperiod)
    | MinusDI { timeperiod } ->
      Hash.combine2 (Hash.int 30) (Hash.int timeperiod)
    | MinusDM { timeperiod } ->
      Hash.combine2 (Hash.int 31) (Hash.int timeperiod)
    | Mom { timeperiod } -> Hash.combine2 (Hash.int 32) (Hash.int timeperiod)
    | PlusDI { timeperiod } -> Hash.combine2 (Hash.int 33) (Hash.int timeperiod)
    | PlusDM { timeperiod } -> Hash.combine2 (Hash.int 34) (Hash.int timeperiod)
    | Ppo { fast_period; slow_period; ma_type } ->
      Hash.combine4 (Hash.int 35) (Hash.int fast_period) (Hash.int slow_period)
        (Hash.int (Ma_type.to_int ma_type))
    | Roc { timeperiod } -> Hash.combine2 (Hash.int 36) (Hash.int timeperiod)
    | Rocp { timeperiod } -> Hash.combine2 (Hash.int 37) (Hash.int timeperiod)
    | Rocr { timeperiod } -> Hash.combine2 (Hash.int 38) (Hash.int timeperiod)
    | Rocr100 { timeperiod } ->
      Hash.combine2 (Hash.int 39) (Hash.int timeperiod)
    | Rsi { timeperiod } -> Hash.combine2 (Hash.int 40) (Hash.int timeperiod)
    | Stoch_SlowK
        {
          fast_k_period;
          slow_k_period;
          slow_k_ma_type;
          slow_d_period;
          slow_d_ma_type;
        } ->
      Hash.combine6 (Hash.int 41) (Hash.int fast_k_period)
        (Hash.int slow_k_period)
        (Hash.int (Ma_type.to_int slow_k_ma_type))
        (Hash.int slow_d_period)
        (Hash.int (Ma_type.to_int slow_d_ma_type))
    | Stoch_SlowD
        {
          fast_k_period;
          slow_k_period;
          slow_k_ma_type;
          slow_d_period;
          slow_d_ma_type;
        } ->
      Hash.combine6 (Hash.int 42) (Hash.int fast_k_period)
        (Hash.int slow_k_period)
        (Hash.int (Ma_type.to_int slow_k_ma_type))
        (Hash.int slow_d_period)
        (Hash.int (Ma_type.to_int slow_d_ma_type))
    | StochF_FastK { fast_k_period; fast_d_period; fast_d_ma_type } ->
      Hash.combine4 (Hash.int 43) (Hash.int fast_k_period)
        (Hash.int fast_d_period)
        (Hash.int (Ma_type.to_int fast_d_ma_type))
    | StochF_FastD { fast_k_period; fast_d_period; fast_d_ma_type } ->
      Hash.combine4 (Hash.int 44) (Hash.int fast_k_period)
        (Hash.int fast_d_period)
        (Hash.int (Ma_type.to_int fast_d_ma_type))
    | StochRsi_FastK
        { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
      Hash.combine5 (Hash.int 45) (Hash.int timeperiod) (Hash.int fast_k_period)
        (Hash.int fast_d_period)
        (Hash.int (Ma_type.to_int fast_d_ma_type))
    | StochRsi_FastD
        { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type } ->
      Hash.combine5 (Hash.int 46) (Hash.int timeperiod) (Hash.int fast_k_period)
        (Hash.int fast_d_period)
        (Hash.int (Ma_type.to_int fast_d_ma_type))
    | Trix { timeperiod } -> Hash.combine2 (Hash.int 47) (Hash.int timeperiod)
    | Ultosc { timeperiod1; timeperiod2; timeperiod3 } ->
      Hash.combine4 (Hash.int 48) (Hash.int timeperiod1) (Hash.int timeperiod2)
        (Hash.int timeperiod3)
    | Willr { timeperiod } -> Hash.combine2 (Hash.int 49) (Hash.int timeperiod)
    | Ad -> Hash.int 50
    | Adosc { fast_period; slow_period } ->
      Hash.combine3 (Hash.int 51) (Hash.int fast_period) (Hash.int slow_period)
    | Obv -> Hash.int 52
    | Atr { timeperiod } -> Hash.combine2 (Hash.int 53) (Hash.int timeperiod)
    | Natr { timeperiod } -> Hash.combine2 (Hash.int 54) (Hash.int timeperiod)
    | Trange -> Hash.int 55
    | AvgPrice -> Hash.int 56
    | MedPrice -> Hash.int 57
    | TypPrice -> Hash.int 58
    | WclPrice -> Hash.int 59
    | HtDcPeriod -> Hash.int 60
    | HtDcPhase -> Hash.int 61
    | HtPhasor_InPhase -> Hash.int 62
    | HtPhasor_Quadrature -> Hash.int 63
    | HtSine_Sine -> Hash.int 64
    | HtSine_LeadSine -> Hash.int 65
    | Aroon_Down { timeperiod } ->
      Hash.combine2 (Hash.int 66) (Hash.int timeperiod)
    | Aroon_Up { timeperiod } ->
      Hash.combine2 (Hash.int 67) (Hash.int timeperiod)
    | Beta { timeperiod } -> Hash.combine2 (Hash.int 68) (Hash.int timeperiod)
    | Correl { timeperiod } -> Hash.combine2 (Hash.int 69) (Hash.int timeperiod)
    | Linearreg { timeperiod } ->
      Hash.combine2 (Hash.int 70) (Hash.int timeperiod)
    | LinearregAngle { timeperiod } ->
      Hash.combine2 (Hash.int 71) (Hash.int timeperiod)
    | LinearregIntercept { timeperiod } ->
      Hash.combine2 (Hash.int 72) (Hash.int timeperiod)
    | LinearregSlope { timeperiod } ->
      Hash.combine2 (Hash.int 73) (Hash.int timeperiod)
    | MinMax_Min { timeperiod } ->
      Hash.combine2 (Hash.int 74) (Hash.int timeperiod)
    | MinMax_Max { timeperiod } ->
      Hash.combine2 (Hash.int 75) (Hash.int timeperiod)
    | Stddev { timeperiod; nb_dev } ->
      Hash.combine3 (Hash.int 76) (Hash.int timeperiod) (Hash.poly nb_dev)
    | Tsf { timeperiod } -> Hash.combine2 (Hash.int 77) (Hash.int timeperiod)
    | Var { timeperiod; nb_dev } ->
      Hash.combine3 (Hash.int 78) (Hash.int timeperiod) (Hash.poly nb_dev)
    | Acos -> Hash.int 79
    | Asin -> Hash.int 80
    | Atan -> Hash.int 81
    | Ceil -> Hash.int 82
    | Cos -> Hash.int 83
    | Cosh -> Hash.int 84
    | Exp -> Hash.int 85
    | Floor -> Hash.int 86
    | Ln -> Hash.int 87
    | Log10 -> Hash.int 88
    | Sin -> Hash.int 89
    | Sinh -> Hash.int 90
    | Sqrt -> Hash.int 91
    | Tan -> Hash.int 92
    | Tanh -> Hash.int 93
    | Add -> Hash.int 94
    | Div -> Hash.int 95
    | Max { timeperiod } -> Hash.combine2 (Hash.int 96) (Hash.int timeperiod)
    | Min { timeperiod } -> Hash.combine2 (Hash.int 97) (Hash.int timeperiod)
    | Mult -> Hash.int 98
    | Sub -> Hash.int 99
    | Sum { timeperiod } -> Hash.combine2 (Hash.int 100) (Hash.int timeperiod)
    | Avgdev { timeperiod } ->
      Hash.combine2 (Hash.int 101) (Hash.int timeperiod)
    | Bop -> Hash.int 102
    | Imi { timeperiod } -> Hash.combine2 (Hash.int 103) (Hash.int timeperiod)
    | Mama_MAMA { fast_limit; slow_limit } ->
      Hash.combine3 (Hash.int 104) (Hash.poly fast_limit) (Hash.poly slow_limit)
    | Mama_FAMA { fast_limit; slow_limit } ->
      Hash.combine3 (Hash.int 105) (Hash.poly fast_limit) (Hash.poly slow_limit)
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

  let hash = function
    | HtTrendMode -> Hash.int 0
    | Cdl2Crows -> Hash.int 1
    | Cdl3BlackCrows -> Hash.int 2
    | Cdl3Inside -> Hash.int 3
    | Cdl3LineStrike -> Hash.int 4
    | Cdl3Outside -> Hash.int 5
    | Cdl3StarsInSouth -> Hash.int 6
    | Cdl3WhiteSoldiers -> Hash.int 7
    | CdlAbandonedBaby { penetration } ->
      Hash.combine2 (Hash.int 8) (Hash.poly penetration)
    | CdlAdvanceBlock -> Hash.int 9
    | CdlBeltHold -> Hash.int 10
    | CdlBreakaway -> Hash.int 11
    | CdlClosingMarubozu -> Hash.int 12
    | CdlConcealBabySwall -> Hash.int 13
    | CdlCounterAttack -> Hash.int 14
    | CdlDarkCloudCover { penetration } ->
      Hash.combine2 (Hash.int 15) (Hash.poly penetration)
    | CdlDoji -> Hash.int 16
    | CdlDojiStar -> Hash.int 17
    | CdlDragonflyDoji -> Hash.int 18
    | CdlEngulfing -> Hash.int 19
    | CdlEveningDojiStar { penetration } ->
      Hash.combine2 (Hash.int 20) (Hash.poly penetration)
    | CdlEveningStar { penetration } ->
      Hash.combine2 (Hash.int 21) (Hash.poly penetration)
    | CdlGapSideSideWhite -> Hash.int 22
    | CdlGravestoneDoji -> Hash.int 23
    | CdlHammer -> Hash.int 24
    | CdlHangingMan -> Hash.int 25
    | CdlHarami -> Hash.int 26
    | CdlHaramiCross -> Hash.int 27
    | CdlHighWave -> Hash.int 28
    | CdlHikkake -> Hash.int 29
    | CdlHikkakeMod -> Hash.int 30
    | CdlHomingPigeon -> Hash.int 31
    | CdlIdentical3Crows -> Hash.int 32
    | CdlInNeck -> Hash.int 33
    | CdlInvertedHammer -> Hash.int 34
    | CdlKicking -> Hash.int 35
    | CdlKickingByLength -> Hash.int 36
    | CdlLadderBottom -> Hash.int 37
    | CdlLongLeggedDoji -> Hash.int 38
    | CdlLongLine -> Hash.int 39
    | CdlMarubozu -> Hash.int 40
    | CdlMatchingLow -> Hash.int 41
    | CdlMatHold { penetration } ->
      Hash.combine2 (Hash.int 42) (Hash.poly penetration)
    | CdlMorningDojiStar { penetration } ->
      Hash.combine2 (Hash.int 43) (Hash.poly penetration)
    | CdlMorningStar { penetration } ->
      Hash.combine2 (Hash.int 44) (Hash.poly penetration)
    | CdlOnNeck -> Hash.int 45
    | CdlPiercing -> Hash.int 46
    | CdlRickshawMan -> Hash.int 47
    | CdlRiseFall3Methods -> Hash.int 48
    | CdlSeparatingLines -> Hash.int 49
    | CdlShootingStar -> Hash.int 50
    | CdlShortLine -> Hash.int 51
    | CdlSpinningTop -> Hash.int 52
    | CdlStalledPattern -> Hash.int 53
    | CdlStickSandwich -> Hash.int 54
    | CdlTakuri -> Hash.int 55
    | CdlTasukiGap -> Hash.int 56
    | CdlThrusting -> Hash.int 57
    | CdlTristar -> Hash.int 58
    | CdlUnique3River -> Hash.int 59
    | CdlUpsideGap2Crows -> Hash.int 60
    | CdlXSideGap3Methods -> Hash.int 61
    | MaxIndex { timeperiod } ->
      Hash.combine2 (Hash.int 62) (Hash.int timeperiod)
    | MinIndex { timeperiod } ->
      Hash.combine2 (Hash.int 63) (Hash.int timeperiod)
    | MinMaxIndex_Min { timeperiod } ->
      Hash.combine2 (Hash.int 64) (Hash.int timeperiod)
    | MinMaxIndex_Max { timeperiod } ->
      Hash.combine2 (Hash.int 65) (Hash.int timeperiod)
end

type t = F of Float.t | I of Int.t

let to_string = function
  | F f -> "F " ^ Float.to_string f
  | I i -> "I " ^ Int.to_string i

let pp = fun fmt x -> Format.fprintf fmt "@[%s@]" (to_string x)

let hash = function
  | F f -> Hash.combine2 (Hash.int 0) (Float.hash f)
  | I i -> Hash.combine2 (Hash.int 1) (Int.hash i)
