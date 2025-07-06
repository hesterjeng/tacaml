[@@@warning "-33"]

open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let initialize = foreign "TA_Initialize" (void @-> returning int)

  let ta_atr =
    foreign "TA_ATR" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

  (* TA_LIB_API TA_RetCode TA_STOCH( int    startIdx, *)
  (*                                 int    endIdx, *)
  (*                                            const double inHigh[], *)
  (*                                            const double inLow[], *)
  (*                                            const double inClose[], *)
  (*                                            int           optInFastK_Period, /* From 1 to 100000 */ *)
  (*                                            int           optInSlowK_Period, /* From 1 to 100000 */ *)
  (*                                            TA_MAType     optInSlowK_MAType, *)
  (*                                            int           optInSlowD_Period, /* From 1 to 100000 */ *)
  (*                                            TA_MAType     optInSlowD_MAType, *)
  (*                                            int          *outBegIdx, *)
  (*                                            int          *outNBElement, *)
  (*                                            double        outSlowK[], *)
  (*                                            double        outSlowD[] ); *)
  let ta_stoch =
    foreign "TA_STOCH" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> int @-> int @-> int @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> ptr double @-> returning int

  (* TA_LIB_API TA_RetCode TA_SMA( int    startIdx, *)
  (*                               int    endIdx, *)
  (*                                          const double inReal[], *)
  (*                                          int           optInTimePeriod, /* From 2 to 100000 */ *)
  (*                                          int          *outBegIdx, *)
  (*                                          int          *outNBElement, *)
  (*                                          double        outReal[] ); *)

  let ta_sma =
    foreign "TA_SMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  (* TA_LIB_API TA_RetCode TA_RSI( int    startIdx, *)
  (*                               int    endIdx, *)
  (*                                          const double inReal[], *)
  (*                                          int           optInTimePeriod, /* From 2 to 100000 */ *)
  (*                                          int          *outBegIdx, *)
  (*                                          int          *outNBElement, *)
  (*                                          double        outReal[] ); *)

  let ta_rsi =
    foreign "TA_RSI" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_accbands =
    foreign "TA_ACCBANDS" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double @-> ptr double
    @-> ptr double @-> returning int

  let ta_acos =
    foreign "TA_ACOS" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_ad =
    foreign "TA_AD" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> ptr double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_add =
    foreign "TA_ADD" @@ int @-> int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_adosc =
    foreign "TA_ADOSC" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> int @-> int @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_adx =
    foreign "TA_ADX" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_adxr =
    foreign "TA_ADXR" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_apo =
    foreign "TA_APO" @@ int @-> int @-> ptr double @-> int @-> int @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_aroon =
    foreign "TA_AROON" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> ptr double @-> returning int

  let ta_aroonosc =
    foreign "TA_AROONOSC" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_asin =
    foreign "TA_ASIN" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_atan =
    foreign "TA_ATAN" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_avgprice =
    foreign "TA_AVGPRICE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_avgdev =
    foreign "TA_AVGDEV" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_bbands =
    foreign "TA_BBANDS" @@ int @-> int @-> ptr double @-> int @-> double
    @-> double @-> int @-> ptr int @-> ptr int @-> ptr double @-> ptr double
    @-> ptr double @-> returning int

  let ta_beta =
    foreign "TA_BETA" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_bop =
    foreign "TA_BOP" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> ptr double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_cci =
    foreign "TA_CCI" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_cdl2crows =
    foreign "TA_CDL2CROWS" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdl3blackcrows =
    foreign "TA_CDL3BLACKCROWS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdl3inside =
    foreign "TA_CDL3INSIDE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdl3linestrike =
    foreign "TA_CDL3LINESTRIKE"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdl3outside =
    foreign "TA_CDL3OUTSIDE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdl3starsinsouth =
    foreign "TA_CDL3STARSINSOUTH"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdl3whitesoldiers =
    foreign "TA_CDL3WHITESOLDIERS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlabandonedbaby =
    foreign "TA_CDLABANDONEDBABY"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdladvanceblock =
    foreign "TA_CDLADVANCEBLOCK"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlbelthold =
    foreign "TA_CDLBELTHOLD" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlbreakaway =
    foreign "TA_CDLBREAKAWAY" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlclosingmarubozu =
    foreign "TA_CDLCLOSINGMARUBOZU"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlconcealbabyswall =
    foreign "TA_CDLCONCEALBABYSWALL"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlcounterattack =
    foreign "TA_CDLCOUNTERATTACK"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdldarkcloudcover =
    foreign "TA_CDLDARKCLOUDCOVER"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdldoji =
    foreign "TA_CDLDOJI" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdldojistar =
    foreign "TA_CDLDOJISTAR" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdldragonflydoji =
    foreign "TA_CDLDRAGONFLYDOJI"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlengulfing =
    foreign "TA_CDLENGULFING" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdleveningdojistar =
    foreign "TA_CDLEVENINGDOJISTAR"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdleveningstar =
    foreign "TA_CDLEVENINGSTAR"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlgapsidesidewhite =
    foreign "TA_CDLGAPSIDESIDEWHITE"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlgravestonedoji =
    foreign "TA_CDLGRAVESTONEDOJI"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlhammer =
    foreign "TA_CDLHAMMER" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlhangingman =
    foreign "TA_CDLHANGINGMAN" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlharami =
    foreign "TA_CDLHARAMI" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlharamicross =
    foreign "TA_CDLHARAMICROSS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlhighwave =
    foreign "TA_CDLHIGHWAVE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlhikkake =
    foreign "TA_CDLHIKKAKE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlhikkakemod =
    foreign "TA_CDLHIKKAKEMOD" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlhomingpigeon =
    foreign "TA_CDLHOMINGPIGEON"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlidentical3crows =
    foreign "TA_CDLIDENTICAL3CROWS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlinneck =
    foreign "TA_CDLINNECK" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlinvertedhammer =
    foreign "TA_CDLINVERTEDHAMMER"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlkicking =
    foreign "TA_CDLKICKING" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlkickingbylength =
    foreign "TA_CDLKICKINGBYLENGTH"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlladderbottom =
    foreign "TA_CDLLADDERBOTTOM"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdllongleggeddoji =
    foreign "TA_CDLLONGLEGGEDDOJI"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdllongline =
    foreign "TA_CDLLONGLINE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlmarubozu =
    foreign "TA_CDLMARUBOZU" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlmatchinglow =
    foreign "TA_CDLMATCHINGLOW"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlmathold =
    foreign "TA_CDLMATHOLD" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlmorningdojistar =
    foreign "TA_CDLMORNINGDOJISTAR"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlmorningstar =
    foreign "TA_CDLMORNINGSTAR"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> double @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlonneck =
    foreign "TA_CDLONNECK" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlpiercing =
    foreign "TA_CDLPIERCING" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlrickshawman =
    foreign "TA_CDLRICKSHAWMAN"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlrisefall3methods =
    foreign "TA_CDLRISEFALL3METHODS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlseparatinglines =
    foreign "TA_CDLSEPARATINGLINES"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlshootingstar =
    foreign "TA_CDLSHOOTINGSTAR"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlshortline =
    foreign "TA_CDLSHORTLINE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlspinningtop =
    foreign "TA_CDLSPINNINGTOP"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlstalledpattern =
    foreign "TA_CDLSTALLEDPATTERN"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlsticksandwich =
    foreign "TA_CDLSTICKSANDWICH"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdltakuri =
    foreign "TA_CDLTAKURI" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdltasukigap =
    foreign "TA_CDLTASUKIGAP" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlthrusting =
    foreign "TA_CDLTHRUSTING" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdltristar =
    foreign "TA_CDLTRISTAR" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr double @-> ptr int @-> ptr int @-> ptr int
    @-> returning int

  let ta_cdlunique3river =
    foreign "TA_CDLUNIQUE3RIVER"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlupsidegap2crows =
    foreign "TA_CDLUPSIDEGAP2CROWS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_cdlxsidegap3methods =
    foreign "TA_CDLXSIDEGAP3METHODS"
    @@ int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_ceil =
    foreign "TA_CEIL" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_cmo =
    foreign "TA_CMO" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_correl =
    foreign "TA_CORREL" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_cos =
    foreign "TA_COS" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_cosh =
    foreign "TA_COSH" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_dema =
    foreign "TA_DEMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_div =
    foreign "TA_DIV" @@ int @-> int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_dx =
    foreign "TA_DX" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_ema =
    foreign "TA_EMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_exp =
    foreign "TA_EXP" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_floor =
    foreign "TA_FLOOR" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_ht_dcperiod =
    foreign "TA_HT_DCPERIOD" @@ int @-> int @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_ht_dcphase =
    foreign "TA_HT_DCPHASE" @@ int @-> int @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_ht_phasor =
    foreign "TA_HT_PHASOR" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> ptr double @-> returning int

  let ta_ht_sine =
    foreign "TA_HT_SINE" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> ptr double @-> returning int

  let ta_ht_trendline =
    foreign "TA_HT_TRENDLINE" @@ int @-> int @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_ht_trendmode =
    foreign "TA_HT_TRENDMODE" @@ int @-> int @-> ptr double @-> ptr int
    @-> ptr int @-> ptr int @-> returning int

  let ta_imi =
    foreign "TA_IMI" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_kama =
    foreign "TA_KAMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_linearreg =
    foreign "TA_LINEARREG" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_linearreg_angle =
    foreign "TA_LINEARREG_ANGLE"
    @@ int @-> int @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_linearreg_intercept =
    foreign "TA_LINEARREG_INTERCEPT"
    @@ int @-> int @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_linearreg_slope =
    foreign "TA_LINEARREG_SLOPE"
    @@ int @-> int @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_ln =
    foreign "TA_LN" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_log10 =
    foreign "TA_LOG10" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_ma =
    foreign "TA_MA" @@ int @-> int @-> ptr double @-> int @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_macd =
    foreign "TA_MACD" @@ int @-> int @-> ptr double @-> int @-> int @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> ptr double @-> ptr double
    @-> returning int

  let ta_macdext =
    foreign "TA_MACDEXT" @@ int @-> int @-> ptr double @-> int @-> int @-> int
    @-> int @-> int @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> ptr double @-> ptr double @-> returning int

  let ta_macdfix =
    foreign "TA_MACDFIX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> ptr double @-> ptr double @-> returning int

  let ta_mama =
    foreign "TA_MAMA" @@ int @-> int @-> ptr double @-> double @-> double
    @-> ptr int @-> ptr int @-> ptr double @-> ptr double @-> returning int

  let ta_mavp =
    foreign "TA_MAVP" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> int @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_max =
    foreign "TA_MAX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_maxindex =
    foreign "TA_MAXINDEX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr int @-> returning int

  let ta_medprice =
    foreign "TA_MEDPRICE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_mfi =
    foreign "TA_MFI" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_midpoint =
    foreign "TA_MIDPOINT" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_midprice =
    foreign "TA_MIDPRICE" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_min =
    foreign "TA_MIN" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_minindex =
    foreign "TA_MININDEX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr int @-> returning int

  let ta_minmax =
    foreign "TA_MINMAX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> ptr double @-> returning int

  let ta_minmaxindex =
    foreign "TA_MINMAXINDEX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr int @-> ptr int @-> returning int

  let ta_minus_di =
    foreign "TA_MINUS_DI" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_minus_dm =
    foreign "TA_MINUS_DM" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_mom =
    foreign "TA_MOM" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_mult =
    foreign "TA_MULT" @@ int @-> int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_natr =
    foreign "TA_NATR" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_obv =
    foreign "TA_OBV" @@ int @-> int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_plus_di =
    foreign "TA_PLUS_DI" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_plus_dm =
    foreign "TA_PLUS_DM" @@ int @-> int @-> ptr double @-> ptr double @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_ppo =
    foreign "TA_PPO" @@ int @-> int @-> ptr double @-> int @-> int @-> int
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_roc =
    foreign "TA_ROC" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_rocp =
    foreign "TA_ROCP" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_rocr =
    foreign "TA_ROCR" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_rocr100 =
    foreign "TA_ROCR100" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_sar =
    foreign "TA_SAR" @@ int @-> int @-> ptr double @-> ptr double @-> double
    @-> double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_sarext =
    foreign "TA_SAREXT" @@ int @-> int @-> ptr double @-> ptr double @-> double
    @-> double @-> double @-> double @-> double @-> double @-> double @-> double
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_sin =
    foreign "TA_SIN" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_sinh =
    foreign "TA_SINH" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_sqrt =
    foreign "TA_SQRT" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_stddev =
    foreign "TA_STDDEV" @@ int @-> int @-> ptr double @-> int @-> double
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_stochf =
    foreign "TA_STOCHF" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> int @-> int @-> ptr int @-> ptr int
    @-> ptr double @-> ptr double @-> returning int

  let ta_stochrsi =
    foreign "TA_STOCHRSI" @@ int @-> int @-> ptr double @-> int @-> int @-> int
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> ptr double
    @-> returning int

  let ta_sub =
    foreign "TA_SUB" @@ int @-> int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_sum =
    foreign "TA_SUM" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_t3 =
    foreign "TA_T3" @@ int @-> int @-> ptr double @-> int @-> double @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_tan =
    foreign "TA_TAN" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_tanh =
    foreign "TA_TANH" @@ int @-> int @-> ptr double @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_tema =
    foreign "TA_TEMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_trange =
    foreign "TA_TRANGE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_trima =
    foreign "TA_TRIMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_trix =
    foreign "TA_TRIX" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_tsf =
    foreign "TA_TSF" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  let ta_typprice =
    foreign "TA_TYPPRICE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_ultosc =
    foreign "TA_ULTOSC" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> int @-> int @-> ptr int @-> ptr int
    @-> ptr double @-> returning int

  let ta_var =
    foreign "TA_VAR" @@ int @-> int @-> ptr double @-> int @-> double
    @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_wclprice =
    foreign "TA_WCLPRICE" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr int @-> ptr int @-> ptr double @-> returning int

  let ta_willr =
    foreign "TA_WILLR" @@ int @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
    @-> returning int

  let ta_wma =
    foreign "TA_WMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
    @-> ptr int @-> ptr double @-> returning int

  module Lookback = struct
    let accbands = foreign "TA_ACCBANDS_Lookback" (int @-> returning int)
    let acos = foreign "TA_ACOS_Lookback" (void @-> returning int)
    let ad = foreign "TA_AD_Lookback" (void @-> returning int)
    let add = foreign "TA_ADD_Lookback" (void @-> returning int)
    let adosc = foreign "TA_ADOSC_Lookback" (int @-> int @-> returning int)
    let adx = foreign "TA_ADX_Lookback" (int @-> returning int)
    let adxr = foreign "TA_ADXR_Lookback" (int @-> returning int)
    let apo = foreign "TA_APO_Lookback" (int @-> int @-> int @-> returning int)
    let aroon = foreign "TA_AROON_Lookback" (int @-> returning int)
    let aroonosc = foreign "TA_AROONOSC_Lookback" (int @-> returning int)
    let asin = foreign "TA_ASIN_Lookback" (void @-> returning int)
    let atan = foreign "TA_ATAN_Lookback" (void @-> returning int)
    let atr = foreign "TA_ATR_Lookback" (int @-> returning int)
    let avgprice = foreign "TA_AVGPRICE_Lookback" (void @-> returning int)
    let avgdev = foreign "TA_AVGDEV_Lookback" (int @-> returning int)

    let bbands =
      foreign "TA_BBANDS_Lookback"
        (int @-> double @-> double @-> int @-> returning int)

    let beta = foreign "TA_BETA_Lookback" (int @-> returning int)
    let bop = foreign "TA_BOP_Lookback" (void @-> returning int)
    let cci = foreign "TA_CCI_Lookback" (int @-> returning int)
    let cdl2crows = foreign "TA_CDL2CROWS_Lookback" (void @-> returning int)

    let cdl3blackcrows =
      foreign "TA_CDL3BLACKCROWS_Lookback" (void @-> returning int)

    let cdl3inside = foreign "TA_CDL3INSIDE_Lookback" (void @-> returning int)

    let cdl3linestrike =
      foreign "TA_CDL3LINESTRIKE_Lookback" (void @-> returning int)

    let cdl3outside = foreign "TA_CDL3OUTSIDE_Lookback" (void @-> returning int)

    let cdl3starsinsouth =
      foreign "TA_CDL3STARSINSOUTH_Lookback" (void @-> returning int)

    let cdl3whitesoldiers =
      foreign "TA_CDL3WHITESOLDIERS_Lookback" (void @-> returning int)

    let cdlabandonedbaby =
      foreign "TA_CDLABANDONEDBABY_Lookback" (double @-> returning int)

    let cdladvanceblock =
      foreign "TA_CDLADVANCEBLOCK_Lookback" (void @-> returning int)

    let cdlbelthold = foreign "TA_CDLBELTHOLD_Lookback" (void @-> returning int)

    let cdlbreakaway =
      foreign "TA_CDLBREAKAWAY_Lookback" (void @-> returning int)

    let cdlclosingmarubozu =
      foreign "TA_CDLCLOSINGMARUBOZU_Lookback" (void @-> returning int)

    let cdlconcealbabyswall =
      foreign "TA_CDLCONCEALBABYSWALL_Lookback" (void @-> returning int)

    let cdlcounterattack =
      foreign "TA_CDLCOUNTERATTACK_Lookback" (void @-> returning int)

    let cdldarkcloudcover =
      foreign "TA_CDLDARKCLOUDCOVER_Lookback" (double @-> returning int)

    let cdldoji = foreign "TA_CDLDOJI_Lookback" (void @-> returning int)
    let cdldojistar = foreign "TA_CDLDOJISTAR_Lookback" (void @-> returning int)

    let cdldragonflydoji =
      foreign "TA_CDLDRAGONFLYDOJI_Lookback" (void @-> returning int)

    let cdlengulfing =
      foreign "TA_CDLENGULFING_Lookback" (void @-> returning int)

    let cdleveningdojistar =
      foreign "TA_CDLEVENINGDOJISTAR_Lookback" (double @-> returning int)

    let cdleveningstar =
      foreign "TA_CDLEVENINGSTAR_Lookback" (double @-> returning int)

    let cdlgapsidesidewhite =
      foreign "TA_CDLGAPSIDESIDEWHITE_Lookback" (void @-> returning int)

    let cdlgravestonedoji =
      foreign "TA_CDLGRAVESTONEDOJI_Lookback" (void @-> returning int)

    let cdlhammer = foreign "TA_CDLHAMMER_Lookback" (void @-> returning int)

    let cdlhangingman =
      foreign "TA_CDLHANGINGMAN_Lookback" (void @-> returning int)

    let cdlharami = foreign "TA_CDLHARAMI_Lookback" (void @-> returning int)

    let cdlharamicross =
      foreign "TA_CDLHARAMICROSS_Lookback" (void @-> returning int)

    let cdlhighwave = foreign "TA_CDLHIGHWAVE_Lookback" (void @-> returning int)
    let cdlhikkake = foreign "TA_CDLHIKKAKE_Lookback" (void @-> returning int)

    let cdlhikkakemod =
      foreign "TA_CDLHIKKAKEMOD_Lookback" (void @-> returning int)

    let cdlhomingpigeon =
      foreign "TA_CDLHOMINGPIGEON_Lookback" (void @-> returning int)

    let cdlidentical3crows =
      foreign "TA_CDLIDENTICAL3CROWS_Lookback" (void @-> returning int)

    let cdlinneck = foreign "TA_CDLINNECK_Lookback" (void @-> returning int)

    let cdlinvertedhammer =
      foreign "TA_CDLINVERTEDHAMMER_Lookback" (void @-> returning int)

    let cdlkicking = foreign "TA_CDLKICKING_Lookback" (void @-> returning int)

    let cdlkickingbylength =
      foreign "TA_CDLKICKINGBYLENGTH_Lookback" (void @-> returning int)

    let cdlladderbottom =
      foreign "TA_CDLLADDERBOTTOM_Lookback" (void @-> returning int)

    let cdllongleggeddoji =
      foreign "TA_CDLLONGLEGGEDDOJI_Lookback" (void @-> returning int)

    let cdllongline = foreign "TA_CDLLONGLINE_Lookback" (void @-> returning int)
    let cdlmarubozu = foreign "TA_CDLMARUBOZU_Lookback" (void @-> returning int)

    let cdlmatchinglow =
      foreign "TA_CDLMATCHINGLOW_Lookback" (void @-> returning int)

    let cdlmathold = foreign "TA_CDLMATHOLD_Lookback" (double @-> returning int)

    let cdlmorningdojistar =
      foreign "TA_CDLMORNINGDOJISTAR_Lookback" (double @-> returning int)

    let cdlmorningstar =
      foreign "TA_CDLMORNINGSTAR_Lookback" (double @-> returning int)

    let cdlonneck = foreign "TA_CDLONNECK_Lookback" (void @-> returning int)
    let cdlpiercing = foreign "TA_CDLPIERCING_Lookback" (void @-> returning int)

    let cdlrickshawman =
      foreign "TA_CDLRICKSHAWMAN_Lookback" (void @-> returning int)

    let cdlrisefall3methods =
      foreign "TA_CDLRISEFALL3METHODS_Lookback" (void @-> returning int)

    let cdlseparatinglines =
      foreign "TA_CDLSEPARATINGLINES_Lookback" (void @-> returning int)

    let cdlshootingstar =
      foreign "TA_CDLSHOOTINGSTAR_Lookback" (void @-> returning int)

    let cdlshortline =
      foreign "TA_CDLSHORTLINE_Lookback" (void @-> returning int)

    let cdlspinningtop =
      foreign "TA_CDLSPINNINGTOP_Lookback" (void @-> returning int)

    let cdlstalledpattern =
      foreign "TA_CDLSTALLEDPATTERN_Lookback" (void @-> returning int)

    let cdlsticksandwich =
      foreign "TA_CDLSTICKSANDWICH_Lookback" (void @-> returning int)

    let cdltakuri = foreign "TA_CDLTAKURI_Lookback" (void @-> returning int)

    let cdltasukigap =
      foreign "TA_CDLTASUKIGAP_Lookback" (void @-> returning int)

    let cdlthrusting =
      foreign "TA_CDLTHRUSTING_Lookback" (void @-> returning int)

    let cdltristar = foreign "TA_CDLTRISTAR_Lookback" (void @-> returning int)

    let cdlunique3river =
      foreign "TA_CDLUNIQUE3RIVER_Lookback" (void @-> returning int)

    let cdlupsidegap2crows =
      foreign "TA_CDLUPSIDEGAP2CROWS_Lookback" (void @-> returning int)

    let cdlxsidegap3methods =
      foreign "TA_CDLXSIDEGAP3METHODS_Lookback" (void @-> returning int)

    let ceil = foreign "TA_CEIL_Lookback" (void @-> returning int)
    let cmo = foreign "TA_CMO_Lookback" (int @-> returning int)
    let correl = foreign "TA_CORREL_Lookback" (int @-> returning int)
    let cos = foreign "TA_COS_Lookback" (void @-> returning int)
    let cosh = foreign "TA_COSH_Lookback" (void @-> returning int)
    let dema = foreign "TA_DEMA_Lookback" (int @-> returning int)
    let div = foreign "TA_DIV_Lookback" (void @-> returning int)
    let dx = foreign "TA_DX_Lookback" (int @-> returning int)
    let ema = foreign "TA_EMA_Lookback" (int @-> returning int)
    let exp = foreign "TA_EXP_Lookback" (void @-> returning int)
    let floor = foreign "TA_FLOOR_Lookback" (void @-> returning int)
    let ht_dcperiod = foreign "TA_HT_DCPERIOD_Lookback" (void @-> returning int)
    let ht_dcphase = foreign "TA_HT_DCPHASE_Lookback" (void @-> returning int)
    let ht_phasor = foreign "TA_HT_PHASOR_Lookback" (void @-> returning int)
    let ht_sine = foreign "TA_HT_SINE_Lookback" (void @-> returning int)

    let ht_trendline =
      foreign "TA_HT_TRENDLINE_Lookback" (void @-> returning int)

    let ht_trendmode =
      foreign "TA_HT_TRENDMODE_Lookback" (void @-> returning int)

    let kama = foreign "TA_KAMA_Lookback" (int @-> returning int)
    let linearreg = foreign "TA_LINEARREG_Lookback" (int @-> returning int)

    let linearreg_angle =
      foreign "TA_LINEARREG_ANGLE_Lookback" (int @-> returning int)

    let linearreg_intercept =
      foreign "TA_LINEARREG_INTERCEPT_Lookback" (int @-> returning int)

    let linearreg_slope =
      foreign "TA_LINEARREG_SLOPE_Lookback" (int @-> returning int)

    let ln = foreign "TA_LN_Lookback" (void @-> returning int)
    let log10 = foreign "TA_LOG10_Lookback" (void @-> returning int)
    let ma = foreign "TA_MA_Lookback" (int @-> int @-> returning int)
    let macd = foreign "TA_MACD_Lookback" (int @-> int @-> int @-> returning int)

    let macdext =
      foreign "TA_MACDEXT_Lookback"
        (int @-> int @-> int @-> int @-> int @-> int @-> returning int)

    let macdfix = foreign "TA_MACDFIX_Lookback" (int @-> returning int)
    let mama = foreign "TA_MAMA_Lookback" (double @-> double @-> returning int)
    let mavp = foreign "TA_MAVP_Lookback" (int @-> int @-> int @-> returning int)
    let max = foreign "TA_MAX_Lookback" (int @-> returning int)
    let maxindex = foreign "TA_MAXINDEX_Lookback" (int @-> returning int)
    let medprice = foreign "TA_MEDPRICE_Lookback" (void @-> returning int)
    let mfi = foreign "TA_MFI_Lookback" (int @-> returning int)
    let midpoint = foreign "TA_MIDPOINT_Lookback" (int @-> returning int)
    let midprice = foreign "TA_MIDPRICE_Lookback" (int @-> returning int)
    let min = foreign "TA_MIN_Lookback" (int @-> returning int)
    let minindex = foreign "TA_MININDEX_Lookback" (int @-> returning int)
    let minmax = foreign "TA_MINMAX_Lookback" (int @-> returning int)
    let minmaxindex = foreign "TA_MINMAXINDEX_Lookback" (int @-> returning int)
    let minus_di = foreign "TA_MINUS_DI_Lookback" (int @-> returning int)
    let minus_dm = foreign "TA_MINUS_DM_Lookback" (int @-> returning int)
    let mom = foreign "TA_MOM_Lookback" (int @-> returning int)
    let mult = foreign "TA_MULT_Lookback" (void @-> returning int)
    let natr = foreign "TA_NATR_Lookback" (int @-> returning int)
    let obv = foreign "TA_OBV_Lookback" (void @-> returning int)
    let plus_di = foreign "TA_PLUS_DI_Lookback" (int @-> returning int)
    let plus_dm = foreign "TA_PLUS_DM_Lookback" (int @-> returning int)
    let ppo = foreign "TA_PPO_Lookback" (int @-> int @-> int @-> returning int)
    let roc = foreign "TA_ROC_Lookback" (int @-> returning int)
    let rocp = foreign "TA_ROCP_Lookback" (int @-> returning int)
    let rocr = foreign "TA_ROCR_Lookback" (int @-> returning int)
    let rocr100 = foreign "TA_ROCR100_Lookback" (int @-> returning int)
    let rsi = foreign "TA_RSI_Lookback" (int @-> returning int)
    let sar = foreign "TA_SAR_Lookback" (double @-> double @-> returning int)

    let sarext =
      foreign "TA_SAREXT_Lookback"
        (double @-> double @-> double @-> double @-> double @-> double
       @-> double @-> double @-> returning int)

    let sin = foreign "TA_SIN_Lookback" (void @-> returning int)
    let sinh = foreign "TA_SINH_Lookback" (void @-> returning int)
    let sma = foreign "TA_SMA_Lookback" (int @-> returning int)
    let sqrt = foreign "TA_SQRT_Lookback" (void @-> returning int)
    let stddev = foreign "TA_STDDEV_Lookback" (int @-> double @-> returning int)

    let stoch =
      foreign "TA_STOCH_Lookback"
        (int @-> int @-> int @-> int @-> int @-> returning int)

    let stochf =
      foreign "TA_STOCHF_Lookback" (int @-> int @-> int @-> returning int)

    let stochrsi =
      foreign "TA_STOCHRSI_Lookback"
        (int @-> int @-> int @-> int @-> returning int)

    let sub = foreign "TA_SUB_Lookback" (void @-> returning int)
    let sum = foreign "TA_SUM_Lookback" (int @-> returning int)
    let t3 = foreign "TA_T3_Lookback" (int @-> double @-> returning int)
    let tan = foreign "TA_TAN_Lookback" (void @-> returning int)
    let tanh = foreign "TA_TANH_Lookback" (void @-> returning int)
    let tema = foreign "TA_TEMA_Lookback" (int @-> returning int)
    let trange = foreign "TA_TRANGE_Lookback" (void @-> returning int)
    let trima = foreign "TA_TRIMA_Lookback" (int @-> returning int)
    let trix = foreign "TA_TRIX_Lookback" (int @-> returning int)
    let tsf = foreign "TA_TSF_Lookback" (int @-> returning int)
    let typprice = foreign "TA_TYPPRICE_Lookback" (void @-> returning int)

    let ultosc =
      foreign "TA_ULTOSC_Lookback" (int @-> int @-> int @-> returning int)

    let var = foreign "TA_VAR_Lookback" (int @-> double @-> returning int)
    let wclprice = foreign "TA_WCLPRICE_Lookback" (void @-> returning int)
    let willr = foreign "TA_WILLR_Lookback" (int @-> returning int)
    let wma = foreign "TA_WMA_Lookback" (int @-> returning int)
  end
end
