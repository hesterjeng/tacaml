module T = C.Type
module F = C.Functions
module C = Ctypes

let integer_default = T.ta_integer_default

let result_of_int x =
  match x with
  | 0 -> Ok ()
  | n -> Error n

let initialize () = result_of_int @@ F.initialize ()

module Func = struct
  let sma startIdx endIdx inReal optInTimePeriod outBegIdx outNBElement outReal
      =
    assert (startIdx <= endIdx);
    assert (startIdx >= 0);
    let arr = Ctypes.bigarray_start Ctypes.array1 inReal in
    let out_arr = Ctypes.bigarray_start Ctypes.array1 outReal in
    let out_beg_idx = C.allocate C.int outBegIdx in
    let nbelement = C.allocate C.int outNBElement in
    let res =
      F.ta_sma startIdx endIdx arr optInTimePeriod out_beg_idx nbelement out_arr
    in
    result_of_int res
end
