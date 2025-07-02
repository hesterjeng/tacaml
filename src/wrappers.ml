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

let ta_rsi startIdx endIdx inReal optInTimePeriod outBegIdx outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outReal = C.bigarray_start C.array1 outReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int (-1) in
  let res =
    F.ta_rsi startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_accbands startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outRealUpperBand outRealMiddleBand outRealLowerBand =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outRealUpperBand = C.bigarray_start C.array1 outRealUpperBand in
  let outRealMiddleBand = C.bigarray_start C.array1 outRealMiddleBand in
  let outRealLowerBand = C.bigarray_start C.array1 outRealLowerBand in
  let res =
    F.ta_accbands startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outRealUpperBand outRealMiddleBand outRealLowerBand
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_acos startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_acos startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ad startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let inVolume = C.bigarray_start C.array1 ohlcv.volume in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ad startIdx endIdx inHigh inLow inClose inVolume outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_add startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_add startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_adosc startIdx endIdx (ohlcv : ohlcv) optInFastPeriod optInSlowPeriod
    outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let inVolume = C.bigarray_start C.array1 ohlcv.volume in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_adosc startIdx endIdx inHigh inLow inClose inVolume optInFastPeriod
      optInSlowPeriod outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_adx startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_adx startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_adxr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_adxr startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_apo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
    outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_apo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
      outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_aroon startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outAroonDown outAroonUp =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outAroonDown = C.bigarray_start C.array1 outAroonDown in
  let outAroonUp = C.bigarray_start C.array1 outAroonUp in
  let res =
    F.ta_aroon startIdx endIdx inHigh inLow optInTimePeriod outBegIdx
      outNBElement outAroonDown outAroonUp
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_aroonosc startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_aroonosc startIdx endIdx inHigh inLow optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_asin startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_asin startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_atan startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_atan startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_avgprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_avgprice startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_avgdev startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_avgdev startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_bbands startIdx endIdx inReal optInTimePeriod optInNbDevUp optInNbDevDn
    optInMAType outBegIdx outNBElement outRealUpperBand outRealMiddleBand
    outRealLowerBand =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outRealUpperBand = C.bigarray_start C.array1 outRealUpperBand in
  let outRealMiddleBand = C.bigarray_start C.array1 outRealMiddleBand in
  let outRealLowerBand = C.bigarray_start C.array1 outRealLowerBand in
  let res =
    F.ta_bbands startIdx endIdx inReal optInTimePeriod optInNbDevUp optInNbDevDn
      optInMAType outBegIdx outNBElement outRealUpperBand outRealMiddleBand
      outRealLowerBand
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_beta startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
    outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_beta startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_bop startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_bop startIdx endIdx inOpen inHigh inLow inClose outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cci startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_cci startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl2crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl2crows startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3blackcrows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3blackcrows startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3inside startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3inside startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3linestrike startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3linestrike startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3outside startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3outside startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3starsinsouth startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3starsinsouth startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdl3whitesoldiers startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdl3whitesoldiers startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlabandonedbaby startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlabandonedbaby startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdladvanceblock startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdladvanceblock startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlbelthold startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlbelthold startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlbreakaway startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlbreakaway startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlclosingmarubozu startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlclosingmarubozu startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlconcealbabyswall startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlconcealbabyswall startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlcounterattack startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlcounterattack startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdldarkcloudcover startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdldarkcloudcover startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdldoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outInteger
    =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdldoji startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdldojistar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdldojistar startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdldragonflydoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdldragonflydoji startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlengulfing startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlengulfing startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdleveningdojistar startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdleveningdojistar startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdleveningstar startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdleveningstar startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlgapsidesidewhite startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlgapsidesidewhite startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlgravestonedoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlgravestonedoji startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhammer startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhammer startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhangingman startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhangingman startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlharami startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlharami startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlharamicross startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlharamicross startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhighwave startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhighwave startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhikkake startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhikkake startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhikkakemod startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhikkakemod startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlhomingpigeon startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlhomingpigeon startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlidentical3crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlidentical3crows startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlinneck startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlinneck startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlinvertedhammer startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlinvertedhammer startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlkicking startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlkicking startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlkickingbylength startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlkickingbylength startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlladderbottom startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlladderbottom startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdllongleggeddoji startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdllongleggeddoji startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdllongline startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdllongline startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlmarubozu startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlmarubozu startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlmatchinglow startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlmatchinglow startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlmathold startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlmathold startIdx endIdx inOpen inHigh inLow inClose optInPenetration
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlmorningdojistar startIdx endIdx (ohlcv : ohlcv) optInPenetration
    outBegIdx outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlmorningdojistar startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlmorningstar startIdx endIdx (ohlcv : ohlcv) optInPenetration outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlmorningstar startIdx endIdx inOpen inHigh inLow inClose
      optInPenetration outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlonneck startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlonneck startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlpiercing startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlpiercing startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlrickshawman startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlrickshawman startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlrisefall3methods startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlrisefall3methods startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlseparatinglines startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlseparatinglines startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlshootingstar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlshootingstar startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlshortline startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlshortline startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlspinningtop startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlspinningtop startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlstalledpattern startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlstalledpattern startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlsticksandwich startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlsticksandwich startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdltakuri startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdltakuri startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdltasukigap startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdltasukigap startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlthrusting startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlthrusting startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdltristar startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdltristar startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlunique3river startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlunique3river startIdx endIdx inOpen inHigh inLow inClose outBegIdx
      outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlupsidegap2crows startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement
    outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlupsidegap2crows startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cdlxsidegap3methods startIdx endIdx (ohlcv : ohlcv) outBegIdx
    outNBElement outInteger =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_cdlxsidegap3methods startIdx endIdx inOpen inHigh inLow inClose
      outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ceil startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_ceil startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cmo startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_cmo startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_correl startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
    outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_correl startIdx endIdx inReal0 inReal1 optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cos startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_cos startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_cosh startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_cosh startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_dema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_dema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_div startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_div startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_dx startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx outNBElement
    outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_dx startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_exp startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_exp startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_floor startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_floor startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_dcperiod startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ht_dcperiod startIdx endIdx inReal outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_dcphase startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ht_dcphase startIdx endIdx inReal outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_phasor startIdx endIdx inReal outBegIdx outNBElement outInPhase
    outQuadrature =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInPhase = C.bigarray_start C.array1 outInPhase in
  let outQuadrature = C.bigarray_start C.array1 outQuadrature in
  let res =
    F.ta_ht_phasor startIdx endIdx inReal outBegIdx outNBElement outInPhase
      outQuadrature
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_sine startIdx endIdx inReal outBegIdx outNBElement outSine outLeadSine
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outSine = C.bigarray_start C.array1 outSine in
  let outLeadSine = C.bigarray_start C.array1 outLeadSine in
  let res =
    F.ta_ht_sine startIdx endIdx inReal outBegIdx outNBElement outSine
      outLeadSine
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_trendline startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ht_trendline startIdx endIdx inReal outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ht_trendmode startIdx endIdx inReal outBegIdx outNBElement outInteger =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_ht_trendmode startIdx endIdx inReal outBegIdx outNBElement outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_imi startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inOpen = C.bigarray_start C.array1 ohlcv.open_ in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_imi startIdx endIdx inOpen inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_kama startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_kama startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_linearreg startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_linearreg startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_linearreg_angle startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_linearreg_angle startIdx endIdx inReal optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_linearreg_intercept startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_linearreg_intercept startIdx endIdx inReal optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_linearreg_slope startIdx endIdx inReal optInTimePeriod outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_linearreg_slope startIdx endIdx inReal optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ln startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_ln startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_log10 startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_log10 startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ma startIdx endIdx inReal optInTimePeriod optInMAType outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ma startIdx endIdx inReal optInTimePeriod optInMAType outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_macd startIdx endIdx inReal optInFastPeriod optInSlowPeriod
    optInSignalPeriod outBegIdx outNBElement outMACD outMACDSignal outMACDHist =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMACD = C.bigarray_start C.array1 outMACD in
  let outMACDSignal = C.bigarray_start C.array1 outMACDSignal in
  let outMACDHist = C.bigarray_start C.array1 outMACDHist in
  let res =
    F.ta_macd startIdx endIdx inReal optInFastPeriod optInSlowPeriod
      optInSignalPeriod outBegIdx outNBElement outMACD outMACDSignal outMACDHist
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_macdext startIdx endIdx inReal optInFastPeriod optInFastMAType
    optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
    outBegIdx outNBElement outMACD outMACDSignal outMACDHist =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMACD = C.bigarray_start C.array1 outMACD in
  let outMACDSignal = C.bigarray_start C.array1 outMACDSignal in
  let outMACDHist = C.bigarray_start C.array1 outMACDHist in
  let res =
    F.ta_macdext startIdx endIdx inReal optInFastPeriod optInFastMAType
      optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
      outBegIdx outNBElement outMACD outMACDSignal outMACDHist
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_macdfix startIdx endIdx inReal optInSignalPeriod outBegIdx outNBElement
    outMACD outMACDSignal outMACDHist =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMACD = C.bigarray_start C.array1 outMACD in
  let outMACDSignal = C.bigarray_start C.array1 outMACDSignal in
  let outMACDHist = C.bigarray_start C.array1 outMACDHist in
  let res =
    F.ta_macdfix startIdx endIdx inReal optInSignalPeriod outBegIdx outNBElement
      outMACD outMACDSignal outMACDHist
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_mama startIdx endIdx inReal optInFastLimit optInSlowLimit outBegIdx
    outNBElement outMAMA outFAMA =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMAMA = C.bigarray_start C.array1 outMAMA in
  let outFAMA = C.bigarray_start C.array1 outFAMA in
  let res =
    F.ta_mama startIdx endIdx inReal optInFastLimit optInSlowLimit outBegIdx
      outNBElement outMAMA outFAMA
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_mavp startIdx endIdx inReal inPeriods optInMinPeriod optInMaxPeriod
    optInMAType outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let inPeriods = C.bigarray_start C.array1 inPeriods in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_mavp startIdx endIdx inReal inPeriods optInMinPeriod optInMaxPeriod
      optInMAType outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_max startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_max startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_maxindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outInteger =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_maxindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_medprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_medprice startIdx endIdx inHigh inLow outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_mfi startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let inVolume = C.bigarray_start C.array1 ohlcv.volume in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_mfi startIdx endIdx inHigh inLow inClose inVolume optInTimePeriod
      outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_midpoint startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_midpoint startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_midprice startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_midprice startIdx endIdx inHigh inLow optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_min startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_min startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_minindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outInteger =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outInteger = C.bigarray_start C.array1 outInteger in
  let res =
    F.ta_minindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outInteger
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_minmax startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outMin outMax =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMin = C.bigarray_start C.array1 outMin in
  let outMax = C.bigarray_start C.array1 outMax in
  let res =
    F.ta_minmax startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outMin outMax
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_minmaxindex startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outMinIdx outMaxIdx =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outMinIdx = C.bigarray_start C.array1 outMinIdx in
  let outMaxIdx = C.bigarray_start C.array1 outMaxIdx in
  let res =
    F.ta_minmaxindex startIdx endIdx inReal optInTimePeriod outBegIdx
      outNBElement outMinIdx outMaxIdx
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_minus_di startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_minus_di startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_minus_dm startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_minus_dm startIdx endIdx inHigh inLow optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_mom startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_mom startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_mult startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_mult startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_natr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_natr startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_obv startIdx endIdx inReal inVolume outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let inVolume = C.bigarray_start C.array1 inVolume in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_obv startIdx endIdx inReal inVolume outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_plus_di startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_plus_di startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_plus_dm startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_plus_dm startIdx endIdx inHigh inLow optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ppo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
    outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ppo startIdx endIdx inReal optInFastPeriod optInSlowPeriod optInMAType
      outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_roc startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_roc startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_rocp startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_rocp startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_rocr startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_rocr startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_rocr100 startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_rocr100 startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sar startIdx endIdx (ohlcv : ohlcv) optInAcceleration optInMaximum
    outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_sar startIdx endIdx inHigh inLow optInAcceleration optInMaximum
      outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sarext startIdx endIdx (ohlcv : ohlcv) optInStartValue
    optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
    optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort
    optInAccelerationMaxShort outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_sarext startIdx endIdx inHigh inLow optInStartValue
      optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong
      optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort
      optInAccelerationMaxShort outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sin startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_sin startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sinh startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_sinh startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_sma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sqrt startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_sqrt startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_stddev startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_stddev startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_stoch startIdx endIdx (ohlcv : ohlcv) optInFastK_Period optInSlowK_Period
    optInSlowK_MAType optInSlowD_Period optInSlowD_MAType outBegIdx outNBElement
    outSlowK outSlowD =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outSlowK = C.bigarray_start C.array1 outSlowK in
  let outSlowD = C.bigarray_start C.array1 outSlowD in
  let res =
    F.ta_stoch startIdx endIdx inHigh inLow inClose optInFastK_Period
      optInSlowK_Period optInSlowK_MAType optInSlowD_Period optInSlowD_MAType
      outBegIdx outNBElement outSlowK outSlowD
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_stochf startIdx endIdx (ohlcv : ohlcv) optInFastK_Period
    optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK outFastD
    =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outFastK = C.bigarray_start C.array1 outFastK in
  let outFastD = C.bigarray_start C.array1 outFastD in
  let res =
    F.ta_stochf startIdx endIdx inHigh inLow inClose optInFastK_Period
      optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK
      outFastD
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_stochrsi startIdx endIdx inReal optInTimePeriod optInFastK_Period
    optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK outFastD
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outFastK = C.bigarray_start C.array1 outFastK in
  let outFastD = C.bigarray_start C.array1 outFastD in
  let res =
    F.ta_stochrsi startIdx endIdx inReal optInTimePeriod optInFastK_Period
      optInFastD_Period optInFastD_MAType outBegIdx outNBElement outFastK
      outFastD
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sub startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal =
  let inReal0 = C.bigarray_start C.array1 inReal0 in
  let inReal1 = C.bigarray_start C.array1 inReal1 in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_sub startIdx endIdx inReal0 inReal1 outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_sum startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_sum startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_t3 startIdx endIdx inReal optInTimePeriod optInVFactor outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_t3 startIdx endIdx inReal optInTimePeriod optInVFactor outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_tan startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_tan startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_tanh startIdx endIdx inReal outBegIdx outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res = F.ta_tanh startIdx endIdx inReal outBegIdx outNBElement outReal in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_tema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_tema startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_trange startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_trange startIdx endIdx inHigh inLow inClose outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_trima startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_trima startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_trix startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
    outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_trix startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_tsf startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_tsf startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_typprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_typprice startIdx endIdx inHigh inLow inClose outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_ultosc startIdx endIdx (ohlcv : ohlcv) optInTimePeriod1 optInTimePeriod2
    optInTimePeriod3 outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_ultosc startIdx endIdx inHigh inLow inClose optInTimePeriod1
      optInTimePeriod2 optInTimePeriod3 outBegIdx outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_var startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
    outNBElement outReal =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_var startIdx endIdx inReal optInTimePeriod optInNbDev outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_wclprice startIdx endIdx (ohlcv : ohlcv) outBegIdx outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_wclprice startIdx endIdx inHigh inLow inClose outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_willr startIdx endIdx (ohlcv : ohlcv) optInTimePeriod outBegIdx
    outNBElement outReal =
  let inHigh = C.bigarray_start C.array1 ohlcv.high in
  let inLow = C.bigarray_start C.array1 ohlcv.low in
  let inClose = C.bigarray_start C.array1 ohlcv.close in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_willr startIdx endIdx inHigh inLow inClose optInTimePeriod outBegIdx
      outNBElement outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err

let ta_wma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
    =
  let inReal = C.bigarray_start C.array1 inReal in
  let outBegIdx = C.allocate C.int outBegIdx in
  let outNBElement = C.allocate C.int outNBElement in
  let outReal = C.bigarray_start C.array1 outReal in
  let res =
    F.ta_wma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement
      outReal
  in
  match res with
  | 0 -> Ok ()
  | err -> Error err
