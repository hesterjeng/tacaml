[@@@warning "-33"]

open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type ta_string_table

  let ta_string_table : ta_string_table structure typ =
    structure "TA_StringTable"
end
