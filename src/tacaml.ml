module Ohlcv = Ohlcv
module C = C.Functions
module Safe = Wrappers
module Input = Input_source
module Output = Output_destination
module Defaults = Defaults
module Indicator = Type

type t = Pack.t

let initialize = Wrappers.ta_initialize
let calculate = Pack.calculate
let all = Defaults.all
