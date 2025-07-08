module Ohlcv = Ohlcv
module C = C.Functions
module Safe = Wrappers
module Input = Input_source
module Output = Output_destination
module Defaults = Defaults
module Indicator = Type

type t = Pack.t

let input x = Pack.input_flag_from_wrapper x
let output x = Pack.output_flag_from_wrapper x
let initialize = Wrappers.ta_initialize
let calculate = Pack.calculate
let all = Defaults.all
