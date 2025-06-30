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
end
