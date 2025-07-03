module F = C.Functions
module C = Ctypes
module T = Type_description.Types

let ta_initialize () =
  let ret_code = F.initialize () in
  if ret_code = 0 then Ok () else Error (string_of_int ret_code)

let wrap f =
  let open Ctypes in
  let outBegIdx = C.allocate C.int 0 in
  let outNBElement = C.allocate C.int 0 in
  let res = f outBegIdx outNBElement in
  match res with
  | 0 -> Ok (!@outBegIdx, !@outNBElement)
  | err -> Error (`TALibCode err)

let ba = C.bigarray_start C.array1

let iba :
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int Ctypes.ptr =
  C.bigarray_start C.array1

let ta_rsi (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_rsi startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_accbands (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod
    outRealUpperBand outRealMiddleBand outRealLowerBand =
  wrap (fun outBegIdx outNBElement ->
      F.ta_accbands startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod outBegIdx outNBElement
        (ba outRealUpperBand) (ba outRealMiddleBand) (ba outRealLowerBand))

let ta_acos (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_acos startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_ad (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ad startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) outBegIdx outNBElement (ba outReal))

let ta_add (startIdx, endIdx) inReal0 inReal1 outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_add startIdx endIdx (ba inReal0) (ba inReal1) outBegIdx outNBElement
        (ba outReal))

let ta_adosc (startIdx, endIdx) (ohlcv : Ohlcv.t) optInFastPeriod
    optInSlowPeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_adosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) optInFastPeriod optInSlowPeriod outBegIdx outNBElement
        (ba outReal))

let ta_adx (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_adx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_adxr (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_adxr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_apo (startIdx, endIdx) inReal optInFastPeriod optInSlowPeriod
    ?(optInMAType = Ma_type.EMA) outReal =
  let optInMAType = Ma_type.to_int optInMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_apo startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInMAType outBegIdx outNBElement (ba outReal))

let ta_aroon (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outAroonDown
    outAroonUp =
  wrap (fun outBegIdx outNBElement ->
      F.ta_aroon startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInTimePeriod
        outBegIdx outNBElement (ba outAroonDown) (ba outAroonUp))

let ta_aroonosc (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_aroonosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_asin (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_asin startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_atan (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_atan startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_avgprice (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_avgprice startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (ba outReal))

let ta_avgdev (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_avgdev startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_bbands (startIdx, endIdx) inReal optInTimePeriod optInNbDevUp
    optInNbDevDn ?(optInMAType = Ma_type.EMA) outRealUpperBand outRealMiddleBand
    outRealLowerBand =
  let optInMAType = Ma_type.to_int optInMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_bbands startIdx endIdx (ba inReal) optInTimePeriod optInNbDevUp
        optInNbDevDn optInMAType outBegIdx outNBElement (ba outRealUpperBand)
        (ba outRealMiddleBand) (ba outRealLowerBand))

let ta_beta (startIdx, endIdx) inReal0 inReal1 optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_beta startIdx endIdx (ba inReal0) (ba inReal1) optInTimePeriod
        outBegIdx outNBElement (ba outReal))

let ta_bop (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_bop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) outBegIdx outNBElement (ba outReal))

let ta_cci (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cci startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_cdl2crows (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3blackcrows (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3blackcrows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3inside (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3inside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3linestrike (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3linestrike startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3outside (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3outside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3starsinsouth (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3starsinsouth startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdl3whitesoldiers (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdl3whitesoldiers startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlabandonedbaby (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlabandonedbaby startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdladvanceblock (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdladvanceblock startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlbelthold (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlbelthold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlbreakaway (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlbreakaway startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlclosingmarubozu (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlclosingmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlconcealbabyswall (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlconcealbabyswall startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlcounterattack (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlcounterattack startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdldarkcloudcover (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdldarkcloudcover startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdldoji (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdldoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdldojistar (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdldojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdldragonflydoji (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdldragonflydoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlengulfing (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlengulfing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdleveningdojistar (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdleveningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdleveningstar (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdleveningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdlgapsidesidewhite (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlgapsidesidewhite startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlgravestonedoji (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlgravestonedoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhammer (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhangingman (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhangingman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlharami (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlharami startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlharamicross (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlharamicross startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhighwave (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhighwave startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhikkake (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhikkake startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhikkakemod (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhikkakemod startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlhomingpigeon (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlhomingpigeon startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlidentical3crows (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlidentical3crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlinneck (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlinneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlinvertedhammer (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlinvertedhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlkicking (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlkicking startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlkickingbylength (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlkickingbylength startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlladderbottom (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlladderbottom startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdllongleggeddoji (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdllongleggeddoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdllongline (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdllongline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlmarubozu (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlmatchinglow (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlmatchinglow startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlmathold (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlmathold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdlmorningdojistar (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlmorningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdlmorningstar (startIdx, endIdx) (ohlcv : Ohlcv.t) optInPenetration
    outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlmorningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration outBegIdx outNBElement
        (iba outInteger))

let ta_cdlonneck (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlonneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlpiercing (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlpiercing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlrickshawman (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlrickshawman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlrisefall3methods (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlrisefall3methods startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlseparatinglines (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlseparatinglines startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlshootingstar (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlshootingstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlshortline (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlshortline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlspinningtop (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlspinningtop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlstalledpattern (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlstalledpattern startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlsticksandwich (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlsticksandwich startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdltakuri (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdltakuri startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdltasukigap (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdltasukigap startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlthrusting (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlthrusting startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdltristar (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdltristar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlunique3river (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlunique3river startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlupsidegap2crows (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlupsidegap2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_cdlxsidegap3methods (startIdx, endIdx) (ohlcv : Ohlcv.t) outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cdlxsidegap3methods startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement (iba outInteger))

let ta_ceil (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ceil startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_cmo (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cmo startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_correl (startIdx, endIdx) inReal0 inReal1 optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_correl startIdx endIdx (ba inReal0) (ba inReal1) optInTimePeriod
        outBegIdx outNBElement (ba outReal))

let ta_cos (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cos startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_cosh (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_cosh startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_dema (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_dema startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_div (startIdx, endIdx) inReal0 inReal1 outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_div startIdx endIdx (ba inReal0) (ba inReal1) outBegIdx outNBElement
        (ba outReal))

let ta_dx (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_dx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_ema (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ema startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_exp (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_exp startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_floor (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_floor startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_ht_dcperiod (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_dcperiod startIdx endIdx (ba inReal) outBegIdx outNBElement
        (ba outReal))

let ta_ht_dcphase (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_dcphase startIdx endIdx (ba inReal) outBegIdx outNBElement
        (ba outReal))

let ta_ht_phasor (startIdx, endIdx) inReal outInPhase outQuadrature =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_phasor startIdx endIdx (ba inReal) outBegIdx outNBElement
        (ba outInPhase) (ba outQuadrature))

let ta_ht_sine (startIdx, endIdx) inReal outSine outLeadSine =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_sine startIdx endIdx (ba inReal) outBegIdx outNBElement
        (ba outSine) (ba outLeadSine))

let ta_ht_trendline (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_trendline startIdx endIdx (ba inReal) outBegIdx outNBElement
        (ba outReal))

let ta_ht_trendmode (startIdx, endIdx) inReal outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ht_trendmode startIdx endIdx (ba inReal) outBegIdx outNBElement
        (iba outInteger))

let ta_imi (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_imi startIdx endIdx (ba ohlcv.open_) (ba ohlcv.close) optInTimePeriod
        outBegIdx outNBElement (ba outReal))

let ta_kama (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_kama startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_linearreg (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_linearreg startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_linearreg_angle (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_linearreg_angle startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_linearreg_intercept (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_linearreg_intercept startIdx endIdx (ba inReal) optInTimePeriod
        outBegIdx outNBElement (ba outReal))

let ta_linearreg_slope (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_linearreg_slope startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_ln (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ln startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_log10 (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_log10 startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_ma (startIdx, endIdx) inReal optInTimePeriod ?(optInMAType = Ma_type.EMA)
    outReal =
  let optInMAType = Ma_type.to_int optInMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_ma startIdx endIdx (ba inReal) optInTimePeriod optInMAType outBegIdx
        outNBElement (ba outReal))

let ta_macd (startIdx, endIdx) inReal optInFastPeriod optInSlowPeriod
    optInSignalPeriod outMACD outMACDSignal outMACDHist =
  wrap (fun outBegIdx outNBElement ->
      F.ta_macd startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInSignalPeriod outBegIdx outNBElement (ba outMACD) (ba outMACDSignal)
        (ba outMACDHist))

let ta_macdext (startIdx, endIdx) inReal optInFastPeriod
    ?(optInFastMAType = Ma_type.EMA) optInSlowPeriod
    ?(optInSlowMAType = Ma_type.EMA) optInSignalPeriod
    ?(optInSignalMAType = Ma_type.EMA) outMACD outMACDSignal outMACDHist =
  let optInFastMAType = Ma_type.to_int optInFastMAType in
  let optInSlowMAType = Ma_type.to_int optInSlowMAType in
  let optInSignalMAType = Ma_type.to_int optInSignalMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_macdext startIdx endIdx (ba inReal) optInFastPeriod optInFastMAType
        optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
        outBegIdx outNBElement (ba outMACD) (ba outMACDSignal) (ba outMACDHist))

let ta_macdfix (startIdx, endIdx) inReal optInSignalPeriod outMACD outMACDSignal
    outMACDHist =
  wrap (fun outBegIdx outNBElement ->
      F.ta_macdfix startIdx endIdx (ba inReal) optInSignalPeriod outBegIdx
        outNBElement (ba outMACD) (ba outMACDSignal) (ba outMACDHist))

let ta_mama (startIdx, endIdx) inReal optInFastLimit optInSlowLimit outMAMA
    outFAMA =
  wrap (fun outBegIdx outNBElement ->
      F.ta_mama startIdx endIdx (ba inReal) optInFastLimit optInSlowLimit
        outBegIdx outNBElement (ba outMAMA) (ba outFAMA))

let ta_mavp (startIdx, endIdx) inReal inPeriods optInMinPeriod optInMaxPeriod
    ?(optInMAType = Ma_type.EMA) outReal =
  let optInMAType = Ma_type.to_int optInMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_mavp startIdx endIdx (ba inReal) (ba inPeriods) optInMinPeriod
        optInMaxPeriod optInMAType outBegIdx outNBElement (ba outReal))

let ta_max (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_max startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_maxindex (startIdx, endIdx) inReal optInTimePeriod outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_maxindex startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (iba outInteger))

let ta_medprice (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_medprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) outBegIdx
        outNBElement (ba outReal))

let ta_mfi (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_mfi startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_midpoint (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_midpoint startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_midprice (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_midprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_min (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_min startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_minindex (startIdx, endIdx) inReal optInTimePeriod outInteger =
  wrap (fun outBegIdx outNBElement ->
      F.ta_minindex startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (iba outInteger))

let ta_minmax (startIdx, endIdx) inReal optInTimePeriod outMin outMax =
  wrap (fun outBegIdx outNBElement ->
      F.ta_minmax startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outMin) (ba outMax))

let ta_minmaxindex (startIdx, endIdx) inReal optInTimePeriod outMinIdx outMaxIdx
    =
  wrap (fun outBegIdx outNBElement ->
      F.ta_minmaxindex startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (iba outMinIdx) (iba outMaxIdx))

let ta_minus_di (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_minus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_minus_dm (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_minus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_mom (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_mom startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_mult (startIdx, endIdx) inReal0 inReal1 outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_mult startIdx endIdx (ba inReal0) (ba inReal1) outBegIdx outNBElement
        (ba outReal))

let ta_natr (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_natr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_obv (startIdx, endIdx) inReal inVolume outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_obv startIdx endIdx (ba inReal) (ba inVolume) outBegIdx outNBElement
        (ba outReal))

let ta_plus_di (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_plus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_plus_dm (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_plus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_ppo (startIdx, endIdx) inReal optInFastPeriod optInSlowPeriod
    ?(optInMAType = Ma_type.EMA) outReal =
  let optInMAType = Ma_type.to_int optInMAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_ppo startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInMAType outBegIdx outNBElement (ba outReal))

let ta_roc (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_roc startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_rocp (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_rocp startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_rocr (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_rocr startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_rocr100 (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_rocr100 startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_sar (startIdx, endIdx) (ohlcv : Ohlcv.t) optInAcceleration optInMaximum
    outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sar startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInAcceleration
        optInMaximum outBegIdx outNBElement (ba outReal))

let ta_sarext (startIdx, endIdx) (ohlcv : Ohlcv.t) optInStartValue
    optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
    optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort
    optInAccelerationMaxShort outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sarext startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInStartValue
        optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
        optInAccelerationMaxLong optInAccelerationInitShort
        optInAccelerationShort optInAccelerationMaxShort outBegIdx outNBElement
        (ba outReal))

let ta_sin (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sin startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_sinh (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sinh startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_sma (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sma startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_sqrt (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sqrt startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_stddev (startIdx, endIdx) inReal optInTimePeriod optInNbDev outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_stddev startIdx endIdx (ba inReal) optInTimePeriod optInNbDev
        outBegIdx outNBElement (ba outReal))

let ta_stoch (startIdx, endIdx) (ohlcv : Ohlcv.t) optInFastK_Period
    optInSlowK_Period ?(optInSlowK_MAType = Ma_type.EMA) optInSlowD_Period
    ?(optInSlowD_MAType = Ma_type.EMA) outSlowK outSlowD =
  let optInSlowK_MAType = Ma_type.to_int optInSlowK_MAType in
  let optInSlowD_MAType = Ma_type.to_int optInSlowD_MAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_stoch startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInFastK_Period optInSlowK_Period optInSlowK_MAType optInSlowD_Period
        optInSlowD_MAType outBegIdx outNBElement (ba outSlowK) (ba outSlowD))

let ta_stochf (startIdx, endIdx) (ohlcv : Ohlcv.t) optInFastK_Period
    optInFastD_Period ?(optInFastD_MAType = Ma_type.EMA) outFastK outFastD =
  let optInFastD_MAType = Ma_type.to_int optInFastD_MAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_stochf startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInFastK_Period optInFastD_Period optInFastD_MAType
        outBegIdx outNBElement (ba outFastK) (ba outFastD))

let ta_stochrsi (startIdx, endIdx) inReal optInTimePeriod optInFastK_Period
    optInFastD_Period ?(optInFastD_MAType = Ma_type.EMA) outFastK outFastD =
  let optInFastD_MAType = Ma_type.to_int optInFastD_MAType in
  wrap (fun outBegIdx outNBElement ->
      F.ta_stochrsi startIdx endIdx (ba inReal) optInTimePeriod
        optInFastK_Period optInFastD_Period optInFastD_MAType outBegIdx
        outNBElement (ba outFastK) (ba outFastD))

let ta_sub (startIdx, endIdx) inReal0 inReal1 outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sub startIdx endIdx (ba inReal0) (ba inReal1) outBegIdx outNBElement
        (ba outReal))

let ta_sum (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_sum startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_t3 (startIdx, endIdx) inReal optInTimePeriod optInVFactor outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_t3 startIdx endIdx (ba inReal) optInTimePeriod optInVFactor outBegIdx
        outNBElement (ba outReal))

let ta_tan (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_tan startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_tanh (startIdx, endIdx) inReal outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_tanh startIdx endIdx (ba inReal) outBegIdx outNBElement (ba outReal))

let ta_tema (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_tema startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_trange (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_trange startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) outBegIdx outNBElement (ba outReal))

let ta_trima (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_trima startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_trix (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_trix startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_tsf (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_tsf startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))

let ta_typprice (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_typprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) outBegIdx outNBElement (ba outReal))

let ta_ultosc (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod1
    optInTimePeriod2 optInTimePeriod3 outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_ultosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod1 optInTimePeriod2 optInTimePeriod3
        outBegIdx outNBElement (ba outReal))

let ta_var (startIdx, endIdx) inReal optInTimePeriod optInNbDev outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_var startIdx endIdx (ba inReal) optInTimePeriod optInNbDev outBegIdx
        outNBElement (ba outReal))

let ta_wclprice (startIdx, endIdx) (ohlcv : Ohlcv.t) outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_wclprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) outBegIdx outNBElement (ba outReal))

let ta_willr (startIdx, endIdx) (ohlcv : Ohlcv.t) optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_willr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod outBegIdx outNBElement (ba outReal))

let ta_wma (startIdx, endIdx) inReal optInTimePeriod outReal =
  wrap (fun outBegIdx outNBElement ->
      F.ta_wma startIdx endIdx (ba inReal) optInTimePeriod outBegIdx
        outNBElement (ba outReal))
