[@@@warning "-33"]

(* open Ctypes *)

(* let initialize = foreign "TA_Initialize" (void @-> returning void) *)

let sealed = ref false

module Types (F : Ctypes.TYPE) = struct
  (* open F *)
  include F

  let ta_integer_default = constant "TA_INTEGER_DEFAULT" F.int
end
