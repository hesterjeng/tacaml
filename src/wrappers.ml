module F = C.Functions
module C = Ctypes
module T = Type_description.Types

type ohlcv = {
  open_ : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  high : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  low : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  close : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  volume : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let ta_initialize () =
  let ret_code = F.initialize () in
  if ret_code = 0 then Ok () else Error (string_of_int ret_code)

let wrap f =
  let res = f () in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ba = C.bigarray_start C.array1

let iba :
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int Ctypes.ptr =
  C.bigarray_start C.array1

let p v = C.allocate C.int v

let ta_rsi startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_rsi startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_accbands startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outRealUpperBand outRealMiddleBand outRealLowerBand =
  wrap (fun () ->
      F.ta_accbands startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod (p outBegIdx) (p outNBElement)
        (ba outRealUpperBand) (ba outRealMiddleBand) (ba outRealLowerBand))

let ta_acos startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_acos startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_ad startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ad startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) (p outBegIdx) (p outNBElement) (ba outReal))

let ta_add startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_add startIdx endIdx (ba inReal0) (ba inReal1) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_adosc startIdx endIdx (ohlcv : ohlcv) optInFastPeriod optInSlowPeriod
    outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_adosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) optInFastPeriod optInSlowPeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_adx startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_adx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_adxr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_adxr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_apo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
    outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_apo startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInMAType (p outBegIdx) (p outNBElement) (ba outReal))

let ta_aroon startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outAroonDown outAroonUp =
  wrap (fun () ->
      F.ta_aroon startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outAroonDown) (ba outAroonUp))

let ta_aroonosc startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_aroonosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_asin startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_asin startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_atan startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_atan startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_avgprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_avgprice startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_avgdev startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_avgdev startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_bbands startIdx endIdx inReal optInTimePeriod optInNbDevUp optInNbDevDn
    optInMAType outBegIdx outNBElement outRealUpperBand outRealMiddleBand
    outRealLowerBand =
  wrap (fun () ->
      F.ta_bbands startIdx endIdx (ba inReal) optInTimePeriod optInNbDevUp
        optInNbDevDn optInMAType (p outBegIdx) (p outNBElement)
        (ba outRealUpperBand) (ba outRealMiddleBand) (ba outRealLowerBand))

let ta_beta startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_beta startIdx endIdx (ba inReal0) (ba inReal1) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_bop startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_bop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) (p outBegIdx) (p outNBElement) (ba outReal))

let ta_cci startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_cci startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_cdl2crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3blackcrows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3blackcrows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3inside startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3inside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3linestrike startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3linestrike startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3outside startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3outside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3starsinsouth startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3starsinsouth startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdl3whitesoldiers startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdl3whitesoldiers startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlabandonedbaby startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlabandonedbaby startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdladvanceblock startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdladvanceblock startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlbelthold startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlbelthold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlbreakaway startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlbreakaway startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlclosingmarubozu startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlclosingmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlconcealbabyswall startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlconcealbabyswall startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlcounterattack startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlcounterattack startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdldarkcloudcover startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdldarkcloudcover startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdldoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outInteger
    =
  wrap (fun () ->
      F.ta_cdldoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdldojistar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdldojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdldragonflydoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdldragonflydoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlengulfing startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlengulfing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdleveningdojistar startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdleveningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdleveningstar startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdleveningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdlgapsidesidewhite startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlgapsidesidewhite startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlgravestonedoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlgravestonedoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhammer startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhangingman startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhangingman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlharami startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlharami startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlharamicross startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlharamicross startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhighwave startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhighwave startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhikkake startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhikkake startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhikkakemod startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhikkakemod startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlhomingpigeon startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlhomingpigeon startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlidentical3crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlidentical3crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlinneck startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlinneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlinvertedhammer startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlinvertedhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlkicking startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlkicking startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlkickingbylength startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlkickingbylength startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlladderbottom startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlladderbottom startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdllongleggeddoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdllongleggeddoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdllongline startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdllongline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlmarubozu startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlmatchinglow startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlmatchinglow startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlmathold startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlmathold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdlmorningdojistar startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlmorningdojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdlmorningstar startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlmorningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) optInPenetration (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_cdlonneck startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlonneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlpiercing startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlpiercing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlrickshawman startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlrickshawman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlrisefall3methods startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlrisefall3methods startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlseparatinglines startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlseparatinglines startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlshootingstar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlshootingstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlshortline startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlshortline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlspinningtop startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlspinningtop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlstalledpattern startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlstalledpattern startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlsticksandwich startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlsticksandwich startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdltakuri startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdltakuri startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdltasukigap startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdltasukigap startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlthrusting startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlthrusting startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdltristar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdltristar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlunique3river startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlunique3river startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlupsidegap2crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_cdlupsidegap2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_cdlxsidegap3methods startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  wrap (fun () ->
      F.ta_cdlxsidegap3methods startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
        (ba ohlcv.low) (ba ohlcv.close) (p outBegIdx) (p outNBElement)
        (iba outInteger))

let ta_ceil startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ceil startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_cmo startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_cmo startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_correl startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_correl startIdx endIdx (ba inReal0) (ba inReal1) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_cos startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_cos startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_cosh startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_cosh startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_dema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_dema startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_div startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_div startIdx endIdx (ba inReal0) (ba inReal1) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_dx startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_dx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_ema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_ema startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_exp startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_exp startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_floor startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_floor startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_ht_dcperiod startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ht_dcperiod startIdx endIdx (ba inReal) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_ht_dcphase startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ht_dcphase startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_ht_phasor startIdx endIdx inReal outBegIdx outNBElement outInPhase
    outQuadrature =
  wrap (fun () ->
      F.ta_ht_phasor startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outInPhase) (ba outQuadrature))

let ta_ht_sine startIdx endIdx inReal outBegIdx outNBElement outSine outLeadSine
    =
  wrap (fun () ->
      F.ta_ht_sine startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outSine) (ba outLeadSine))

let ta_ht_trendline startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ht_trendline startIdx endIdx (ba inReal) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_ht_trendmode startIdx endIdx inReal outBegIdx outNBElement outInteger =
  wrap (fun () ->
      F.ta_ht_trendmode startIdx endIdx (ba inReal) (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_imi startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_imi startIdx endIdx (ba ohlcv.open_) (ba ohlcv.close) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_kama startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_kama startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_linearreg startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_linearreg startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_linearreg_angle startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_linearreg_angle startIdx endIdx (ba inReal) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_linearreg_intercept startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_linearreg_intercept startIdx endIdx (ba inReal) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_linearreg_slope startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_linearreg_slope startIdx endIdx (ba inReal) optInTimePeriod
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_ln startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ln startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_log10 startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_log10 startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_ma startIdx endIdx inReal optInTimePeriod optInMAType outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_ma startIdx endIdx (ba inReal) optInTimePeriod optInMAType
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_macd startIdx endIdx inReal optInFastPeriod optInSlowPeriod
    optInSignalPeriod outBegIdx outNBElement outMACD outMACDSignal outMACDHist =
  wrap (fun () ->
      F.ta_macd startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInSignalPeriod (p outBegIdx) (p outNBElement) (ba outMACD)
        (ba outMACDSignal) (ba outMACDHist))

let ta_macdext startIdx endIdx inReal optInFastPeriod optInFastMAType
    optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
    outBegIdx outNBElement outMACD outMACDSignal outMACDHist =
  wrap (fun () ->
      F.ta_macdext startIdx endIdx (ba inReal) optInFastPeriod optInFastMAType
        optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
        (p outBegIdx) (p outNBElement) (ba outMACD) (ba outMACDSignal)
        (ba outMACDHist))

let ta_macdfix startIdx endIdx inReal optInSignalPeriod outBegIdx outNBElement
    outMACD outMACDSignal outMACDHist =
  wrap (fun () ->
      F.ta_macdfix startIdx endIdx (ba inReal) optInSignalPeriod (p outBegIdx)
        (p outNBElement) (ba outMACD) (ba outMACDSignal) (ba outMACDHist))

let ta_mama startIdx endIdx inReal optInFastLimit optInSlowLimit outBegIdx
    outNBElement outMAMA outFAMA =
  wrap (fun () ->
      F.ta_mama startIdx endIdx (ba inReal) optInFastLimit optInSlowLimit
        (p outBegIdx) (p outNBElement) (ba outMAMA) (ba outFAMA))

let ta_mavp startIdx endIdx inReal inPeriods optInMinPeriod optInMaxPeriod
    optInMAType outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_mavp startIdx endIdx (ba inReal) (ba inPeriods) optInMinPeriod
        optInMaxPeriod optInMAType (p outBegIdx) (p outNBElement) (ba outReal))

let ta_max startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_max startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_maxindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_maxindex startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_medprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_medprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_mfi startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_mfi startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        (ba ohlcv.volume) optInTimePeriod (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_midpoint startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_midpoint startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_midprice startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_midprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_min startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_min startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_minindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outInteger =
  wrap (fun () ->
      F.ta_minindex startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (iba outInteger))

let ta_minmax startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outMin outMax =
  wrap (fun () ->
      F.ta_minmax startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outMin) (ba outMax))

let ta_minmaxindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outMinIdx outMaxIdx =
  wrap (fun () ->
      F.ta_minmaxindex startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (iba outMinIdx) (iba outMaxIdx))

let ta_minus_di startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_minus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_minus_dm startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_minus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_mom startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_mom startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_mult startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_mult startIdx endIdx (ba inReal0) (ba inReal1) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_natr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_natr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_obv startIdx endIdx inReal inVolume outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_obv startIdx endIdx (ba inReal) (ba inVolume) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_plus_di startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_plus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_plus_dm startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_plus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_ppo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
    outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ppo startIdx endIdx (ba inReal) optInFastPeriod optInSlowPeriod
        optInMAType (p outBegIdx) (p outNBElement) (ba outReal))

let ta_roc startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_roc startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_rocp startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_rocp startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_rocr startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_rocr startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_rocr100 startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_rocr100 startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_sar startIdx endIdx (ohlcv : ohlcv) optInAcceleration optInMaximum
    outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sar startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInAcceleration
        optInMaximum (p outBegIdx) (p outNBElement) (ba outReal))

let ta_sarext startIdx endIdx (ohlcv : ohlcv) optInStartValue
    optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
    optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort
    optInAccelerationMaxShort outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sarext startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) optInStartValue
        optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
        optInAccelerationMaxLong optInAccelerationInitShort
        optInAccelerationShort optInAccelerationMaxShort (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_sin startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sin startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_sinh startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sinh startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_sma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_sma startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_sqrt startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sqrt startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_stddev startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_stddev startIdx endIdx (ba inReal) optInTimePeriod optInNbDev
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_stoch startIdx endIdx (ohlcv : ohlcv) optInFastK_Period optInSlowK_Period
    optInSlowK_MAType optInSlowD_Period optInSlowD_MAType outBegIdx outNBElement
    outSlowK outSlowD =
  wrap (fun () ->
      F.ta_stoch startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInFastK_Period optInSlowK_Period optInSlowK_MAType optInSlowD_Period
        optInSlowD_MAType (p outBegIdx) (p outNBElement) (ba outSlowK)
        (ba outSlowD))

let ta_stochf startIdx endIdx (ohlcv : ohlcv) optInFastK_Period
    optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK outFastD
    =
  wrap (fun () ->
      F.ta_stochf startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInFastK_Period optInFastD_Period optInFastD_MAType
        (p outBegIdx) (p outNBElement) (ba outFastK) (ba outFastD))

let ta_stochrsi startIdx endIdx inReal optInTimePeriod optInFastK_Period
    optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK outFastD
    =
  wrap (fun () ->
      F.ta_stochrsi startIdx endIdx (ba inReal) optInTimePeriod
        optInFastK_Period optInFastD_Period optInFastD_MAType (p outBegIdx)
        (p outNBElement) (ba outFastK) (ba outFastD))

let ta_sub startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_sub startIdx endIdx (ba inReal0) (ba inReal1) (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_sum startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_sum startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_t3 startIdx endIdx inReal optInTimePeriod optInVFactor outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_t3 startIdx endIdx (ba inReal) optInTimePeriod optInVFactor
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_tan startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_tan startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_tanh startIdx endIdx inReal outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_tanh startIdx endIdx (ba inReal) (p outBegIdx) (p outNBElement)
        (ba outReal))

let ta_tema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_tema startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_trange startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_trange startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) (p outBegIdx) (p outNBElement) (ba outReal))

let ta_trima startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_trima startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_trix startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  wrap (fun () ->
      F.ta_trix startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_tsf startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_tsf startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))

let ta_typprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_typprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) (p outBegIdx) (p outNBElement) (ba outReal))

let ta_ultosc startIdx endIdx (ohlcv : ohlcv) optInTimePeriod1 optInTimePeriod2
    optInTimePeriod3 outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_ultosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) optInTimePeriod1 optInTimePeriod2 optInTimePeriod3
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_var startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_var startIdx endIdx (ba inReal) optInTimePeriod optInNbDev
        (p outBegIdx) (p outNBElement) (ba outReal))

let ta_wclprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  wrap (fun () ->
      F.ta_wclprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
        (ba ohlcv.close) (p outBegIdx) (p outNBElement) (ba outReal))

let ta_willr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  wrap (fun () ->
      F.ta_willr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close)
        optInTimePeriod (p outBegIdx) (p outNBElement) (ba outReal))

let ta_wma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  wrap (fun () ->
      F.ta_wma startIdx endIdx (ba inReal) optInTimePeriod (p outBegIdx)
        (p outNBElement) (ba outReal))
