open Wrappers

type t =
  | Ohlcv of Ohlcv.t
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba

module Flag = struct
  type t = OhlcvFlag | FloatBAFlag | FloatBA2Flag
end

let to_string = function
  | Ohlcv _ -> "Ohlcv"
  | FloatBA _ -> "FloatBA"
  | FloatBA2 _ -> "FloatBA2"
