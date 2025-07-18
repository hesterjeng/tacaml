(** Conversion module between Safe.t and Indicator.t types *)

val safe_to_indicators : ('a, 'b) Safe.t -> Indicator.t list
(** Convert a Safe.t indicator to a list of Indicator.t outputs.

    For most indicators, this returns a single-element list containing the
    corresponding Indicator.t. For indicators with multiple outputs (like MACD,
    Stochastic, Bollinger Bands), this returns multiple Indicator.t values.

    Example:
    - [safe_to_indicators (S.Sma { timeperiod = 14 })] returns
      [[F (Indicator.Float.Sma { timeperiod = 14 })]]
    - [safe_to_indicators (S.Macd { fast_period = 12; slow_period = 26;
       signal_period = 9 })] returns
      [[F (Indicator.Float.Macd_MACD { ... }); F
       (Indicator.Float.Macd_MACDSignal { ... }); F
       (Indicator.Float.Macd_MACDHist { ... })]] *)

val indicator_to_safe : Indicator.t -> Pack.t
(** Convert an Indicator.t back to its corresponding Safe.t.

    This is a total function - every Indicator.t has a corresponding Safe.t
    since all Indicator.t values are derived from Safe.t indicators. Multiple
    Indicator.t values from the same Safe.t (like MACD components) will all map
    back to the same Safe.t.

    Example:
    - [indicator_to_safe (F (Indicator.Float.Sma { timeperiod = 14 }))] returns
      [Pack.pack (S.Sma { timeperiod = 14 })]
    - [indicator_to_safe (F (Indicator.Float.Macd_MACD { fast_period = 12;
       slow_period = 26; signal_period = 9 }))] returns
      [Pack.pack (S.Macd { fast_period = 12; slow_period = 26; signal_period = 9
       })] *)
