open Safe
module C = Ctypes

let ba = C.bigarray_start C.array1

let range ?i x =
  match i with
  | Some i -> i
  | None -> Bigarray.Array1.dim x - 1

exception BadInput

let iba :
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int Ctypes.ptr =
 fun arr ->
  (* Cast the int32 array to int array for C compatibility *)
  let int_arr = Obj.magic arr in
  C.bigarray_start C.array1 int_arr

let calculate : type a b.
    (a, b) t -> ?i:int -> a -> b -> (int * int, [> `TALibCode of int ]) result =
 fun params ?i source output ->
  (* let source = source data in *)
  (* let output = output data in *)
  try
    let startIdx =
      match i with
      | Some i -> i
      | None -> 0
    in
    let offset = lookback params in
    let slice arr =
      match i with
      | Some i -> Bigarray.Array1.sub arr i 1
      | None ->
        let len = Bigarray.Array1.dim arr in
        if offset >= len then raise BadInput
        else Bigarray.Array1.sub arr offset (len - offset)
    in
    match (params, source, output) with
    | Accbands { timeperiod }, ohlcv, (out1, out2, out3) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_accbands startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Acos (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_acos startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ad (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ad startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) outBegIdx outNBElement
            (ba (slice out)))
    | Add (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_add startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Adosc { fast_period; slow_period }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) fast_period slow_period outBegIdx
            outNBElement
            (ba (slice out)))
    | Adx { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Adxr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_adxr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Apo { fast_period; slow_period; ma_type }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_apo startIdx endIdx (ba f) fast_period slow_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Aroon { timeperiod }, ohlcv, (out1, out2) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_aroon startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Aroonosc { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_aroonosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Asin (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_asin startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Atan (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_atan startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Atr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_atr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Avgprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_avgprice startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Avgdev { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_avgdev startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | ( Bbands { timeperiod; nb_dev_up; nb_dev_dn; ma_type },
        ohlcv,
        (out1, out2, out3) ) ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_bbands startIdx endIdx (ba f) timeperiod nb_dev_up nb_dev_dn
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Beta { timeperiod }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_beta startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Bop (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_bop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Cci { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cci startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Cdl2crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl2crows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3blackcrows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3blackcrows startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3inside (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3inside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3linestrike (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3linestrike startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3outside (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3outside startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3starsinsouth (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3starsinsouth startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdl3whitesoldiers (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdl3whitesoldiers startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlabandonedbaby { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlabandonedbaby startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdladvanceblock (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdladvanceblock startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlbelthold (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlbelthold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlbreakaway (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlbreakaway startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlclosingmarubozu (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlclosingmarubozu startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlconcealbabyswall (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlconcealbabyswall startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlcounterattack (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlcounterattack startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldarkcloudcover { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldarkcloudcover startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdldoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldojistar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldojistar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdldragonflydoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdldragonflydoji startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlengulfing (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlengulfing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdleveningdojistar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdleveningdojistar startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdleveningstar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdleveningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlgapsidesidewhite (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlgapsidesidewhite startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlgravestonedoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlgravestonedoji startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlhammer (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhammer startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhangingman (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhangingman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlharami (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlharami startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlharamicross (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlharamicross startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhighwave (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhighwave startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhikkake (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhikkake startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhikkakemod (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhikkakemod startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlhomingpigeon (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlhomingpigeon startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlidentical3crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlidentical3crows startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlinneck (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlinneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlinvertedhammer (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlinvertedhammer startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlkicking (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlkicking startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlkickingbylength (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlkickingbylength startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlladderbottom (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlladderbottom startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdllongleggeddoji (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdllongleggeddoji startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdllongline (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdllongline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmarubozu (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmarubozu startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmatchinglow (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmatchinglow startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmathold { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmathold startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmorningdojistar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmorningdojistar startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) penetration
            outBegIdx outNBElement
            (iba (slice out)))
    | Cdlmorningstar { penetration }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlmorningstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) penetration outBegIdx outNBElement
            (iba (slice out)))
    | Cdlonneck (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlonneck startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlpiercing (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlpiercing startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlrickshawman (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlrickshawman startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlrisefall3methods (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlrisefall3methods startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlseparatinglines (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlseparatinglines startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlshootingstar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlshootingstar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlshortline (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlshortline startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlspinningtop (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlspinningtop startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlstalledpattern (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlstalledpattern startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlsticksandwich (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlsticksandwich startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltakuri (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltakuri startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltasukigap (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltasukigap startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlthrusting (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlthrusting startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdltristar (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdltristar startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlunique3river (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlunique3river startIdx endIdx (ba ohlcv.open_) (ba ohlcv.high)
            (ba ohlcv.low) (ba ohlcv.close) outBegIdx outNBElement
            (iba (slice out)))
    | Cdlupsidegap2crows (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlupsidegap2crows startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Cdlxsidegap3methods (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cdlxsidegap3methods startIdx endIdx (ba ohlcv.open_)
            (ba ohlcv.high) (ba ohlcv.low) (ba ohlcv.close) outBegIdx
            outNBElement
            (iba (slice out)))
    | Ceil (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ceil startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Cmo { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cmo startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Correl { timeperiod }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_correl startIdx endIdx (ba f1) (ba f2) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Cos (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cos startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Cosh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_cosh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Dema { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_dema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Div (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_div startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Dx { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_dx startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Ema { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ema startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Exp (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_exp startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Floor (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_floor startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_dcperiod (), ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_dcperiod startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_dcphase (), ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_dcphase startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_phasor (), ohlcv, (out1, out2) ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_phasor startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Ht_sine (), ohlcv, (out1, out2) ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_sine startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Ht_trendline (), ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_trendline startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ht_trendmode (), ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ht_trendmode startIdx endIdx (ba f) outBegIdx outNBElement
            (iba (slice out)))
    | Imi { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_imi startIdx endIdx (ba ohlcv.open_) (ba ohlcv.close) timeperiod
            outBegIdx outNBElement
            (ba (slice out)))
    | Kama { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_kama startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Linearreg { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_angle { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_angle startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_intercept { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_intercept startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Linearreg_slope { timeperiod }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_linearreg_slope startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Ln (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ln startIdx endIdx (ba f) outBegIdx outNBElement (ba (slice out)))
    | Log10 (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_log10 startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Ma { timeperiod; ma_type }, ohlcv, out ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ma startIdx endIdx (ba f) timeperiod (Ma_type.to_int ma_type)
            outBegIdx outNBElement
            (ba (slice out)))
    | Macd { fast_period; slow_period; signal_period }, ohlcv, (out1, out2, out3)
      ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macd startIdx endIdx (ba f) fast_period slow_period signal_period
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | ( Macdext
          {
            fast_period;
            fast_ma_type;
            slow_period;
            slow_ma_type;
            signal_period;
            signal_ma_type;
          },
        ohlcv,
        (out1, out2, out3) ) ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macdext startIdx endIdx (ba f) fast_period
            (Ma_type.to_int fast_ma_type)
            slow_period
            (Ma_type.to_int slow_ma_type)
            signal_period
            (Ma_type.to_int signal_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Macdfix { signal_period }, ohlcv, (out1, out2, out3) ->
      let f = ohlcv.close in
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_macdfix startIdx endIdx (ba f) signal_period outBegIdx
            outNBElement
            (ba (slice out1))
            (ba (slice out2))
            (ba (slice out3)))
    | Mama { fast_limit; slow_limit }, f, (out1, out2) ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mama startIdx endIdx (ba f.close) fast_limit slow_limit outBegIdx
            outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Mavp { min_period; max_period; ma_type }, (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mavp startIdx endIdx (ba f1) (ba f2) min_period max_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Max { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_max startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Maxindex { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_maxindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (iba (slice out)))
    | Medprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_medprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) outBegIdx
            outNBElement
            (ba (slice out)))
    | Mfi { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mfi startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) (ba ohlcv.volume) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Midpoint { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_midpoint startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Midprice { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_midprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Min { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_min startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Minindex { timeperiod }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minindex startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (iba (slice out)))
    | Minmax { timeperiod }, f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minmax startIdx endIdx (ba f) timeperiod outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Minmaxindex { timeperiod }, f, (out1, out2) ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minmaxindex startIdx endIdx (ba f) timeperiod outBegIdx
            outNBElement
            (iba (slice out1))
            (iba (slice out2)))
    | Minus_di { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Minus_dm { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_minus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Mom { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mom startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Mult (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_mult startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Natr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_natr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Obv (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_obv startIdx endIdx (ba ohlcv.close) (ba ohlcv.volume) outBegIdx
            outNBElement
            (ba (slice out)))
    | Plus_di { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_plus_di startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Plus_dm { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_plus_dm startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) timeperiod
            outBegIdx outNBElement
            (ba (slice out)))
    | Ppo { fast_period; slow_period; ma_type }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ppo startIdx endIdx (ba f.close) fast_period slow_period
            (Ma_type.to_int ma_type) outBegIdx outNBElement
            (ba (slice out)))
    | Roc { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_roc startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Rocp { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocp startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Rocr { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocr startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Rocr100 { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rocr100 startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Rsi { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_rsi startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Sar { acceleration; maximum }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sar startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) acceleration
            maximum outBegIdx outNBElement
            (ba (slice out)))
    | ( Sarext
          {
            start_value;
            offset_on_reverse;
            acceleration_init_long;
            acceleration_long;
            acceleration_max_long;
            acceleration_init_short;
            acceleration_short;
            acceleration_max_short;
          },
        ohlcv,
        out ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sarext startIdx endIdx (ba ohlcv.high) (ba ohlcv.low) start_value
            offset_on_reverse acceleration_init_long acceleration_long
            acceleration_max_long acceleration_init_short acceleration_short
            acceleration_max_short outBegIdx outNBElement
            (ba (slice out)))
    | Sin (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sin startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Sinh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sinh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Sma { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sma startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Sqrt (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sqrt startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Stddev { timeperiod; nb_dev }, f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stddev startIdx endIdx (ba f) timeperiod nb_dev outBegIdx
            outNBElement
            (ba (slice out)))
    | ( Stoch
          {
            fast_k_period;
            slow_k_period;
            slow_k_ma_type;
            slow_d_period;
            slow_d_ma_type;
          },
        ohlcv,
        (out1, out2) ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stoch startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) fast_k_period slow_k_period
            (Ma_type.to_int slow_k_ma_type)
            slow_d_period
            (Ma_type.to_int slow_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | ( Stochf { fast_k_period; fast_d_period; fast_d_ma_type },
        ohlcv,
        (out1, out2) ) ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stochf startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) fast_k_period fast_d_period
            (Ma_type.to_int fast_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | ( Stochrsi { timeperiod; fast_k_period; fast_d_period; fast_d_ma_type },
        f,
        (out1, out2) ) ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_stochrsi startIdx endIdx (ba f.close) timeperiod fast_k_period
            fast_d_period
            (Ma_type.to_int fast_d_ma_type)
            outBegIdx outNBElement
            (ba (slice out1))
            (ba (slice out2)))
    | Sub (), (f1, f2), out ->
      let endIdx = range ?i f1 in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sub startIdx endIdx (ba f1) (ba f2) outBegIdx outNBElement
            (ba (slice out)))
    | Sum { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_sum startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | T3 { timeperiod; v_factor }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_t3 startIdx endIdx (ba f.close) timeperiod v_factor outBegIdx
            outNBElement
            (ba (slice out)))
    | Tan (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tan startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Tanh (), f, out ->
      let endIdx = range ?i f in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tanh startIdx endIdx (ba f) outBegIdx outNBElement
            (ba (slice out)))
    | Tema { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tema startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Trange (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trange startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Trima { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trima startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Trix { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_trix startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Tsf { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_tsf startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
    | Typprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_typprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Ultosc { timeperiod1; timeperiod2; timeperiod3 }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_ultosc startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod1 timeperiod2 timeperiod3 outBegIdx
            outNBElement
            (ba (slice out)))
    | Var { timeperiod; nb_dev }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_var startIdx endIdx (ba f.close) timeperiod nb_dev outBegIdx
            outNBElement
            (ba (slice out)))
    | Wclprice (), ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_wclprice startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) outBegIdx outNBElement
            (ba (slice out)))
    | Willr { timeperiod }, ohlcv, out ->
      let endIdx = range ?i ohlcv.high in
      wrap (fun outBegIdx outNBElement ->
          F.ta_willr startIdx endIdx (ba ohlcv.high) (ba ohlcv.low)
            (ba ohlcv.close) timeperiod outBegIdx outNBElement
            (ba (slice out)))
    | Wma { timeperiod }, f, out ->
      let endIdx = range ?i f.close in
      wrap (fun outBegIdx outNBElement ->
          F.ta_wma startIdx endIdx (ba f.close) timeperiod outBegIdx
            outNBElement
            (ba (slice out)))
  with
  | BadInput -> Error (`TALibCode (-2))
