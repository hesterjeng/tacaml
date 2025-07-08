open Wrappers

type t =
  | FloatBA of float_ba
  | FloatBA2 of float_ba * float_ba
  | FloatBA3 of float_ba * float_ba * float_ba
  | IntBA of int_ba
  | IntBA2 of int_ba * int_ba

module Flag = struct
  type t =
    | FloatBAFlag of Type.t
    | FloatBA2Flag of Type.t * Type.t
    | FloatBA3Flag of Type.t * Type.t * Type.t
    | IntBAFlag of Type.t
    | IntBA2Flag of Type.t * Type.t
end

let to_string = function
  | FloatBA _ -> "FloatBA"
  | FloatBA2 _ -> "FloatBA2"
  | FloatBA3 _ -> "FloatBA3"
  | IntBA _ -> "IntBA"
  | IntBA2 _ -> "IntBA2"
