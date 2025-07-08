open Wrappers

type t =
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba
  | FloatBA3 of float_ba * float_ba * float_ba
  | IntBA of int_ba
  | IntBA2 of int_ba * int_ba
