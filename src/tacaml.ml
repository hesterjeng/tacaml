module Ohlcv = Ohlcv
module C = C.Functions
module Safe = Safe
module Input = Input
module Output = Output
module Defaults = Defaults
module Indicator = Indicator

type t = Pack.t

let input x = Pack.input_flag_from_wrapper x
let output x = Pack.output_flag_from_wrapper x
let initialize = Safe.ta_initialize
let calculate = Pack.calculate
let all = Defaults.all
let to_string = Pack.to_string
let pp = Pack.pp
