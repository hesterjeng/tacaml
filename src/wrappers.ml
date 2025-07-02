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
