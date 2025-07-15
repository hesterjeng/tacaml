open Safe

type t =
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba
  | FloatBA3 of float_ba * float_ba * float_ba
  | IntBA of int_ba
  | IntBA2 of int_ba * int_ba

module Flag = struct
  type t =
    | FloatBAFlag of Indicator.t
    | FloatBA2Flag of Indicator.t * Indicator.t
    | FloatBA3Flag of Indicator.t * Indicator.t * Indicator.t
    | IntBAFlag of Indicator.t
    | IntBA2Flag of Indicator.t * Indicator.t
end

let to_string = function
  | FloatBA _ -> "FloatBA"
  | FloatBA2 _ -> "FloatBA2"
  | FloatBA3 _ -> "FloatBA3"
  | IntBA _ -> "IntBA"
  | IntBA2 _ -> "IntBA2"
