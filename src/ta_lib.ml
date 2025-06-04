module T = C.Type
module F = C.Functions
module C = Ctypes

let integer_default = T.ta_integer_default

let result_of_int x =
  match x with
  | 0 -> Ok ()
  | n -> Error n

let initialize () = result_of_int @@ F.initialize ()

let sma start_idx end_idx lookback input output =
  assert (start_idx <= end_idx);
  assert (start_idx >= 0);
  let arr = Ctypes.bigarray_start Ctypes.array1 input in
  let out_arr = Ctypes.bigarray_start Ctypes.array1 output in
  let out_beg_idx = C.allocate C.int integer_default in
  let nbelement = C.allocate C.int integer_default in
  let res =
    F.ta_sma start_idx end_idx arr lookback out_beg_idx nbelement out_arr
  in
  result_of_int res

let sma_single idx lookback input =
  let ( let* ) = Result.( let* ) in
  let output = Arr.create 1 in
  let* () = sma idx idx lookback input output in
  let res = Arr.get output 0 in
  Result.return res

let sma_top lookback input =
  let l = Arr.dim input in
  sma_single (l - 1) lookback input
