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
