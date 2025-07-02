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
end
