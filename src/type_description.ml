[@@@warning "-33"]

open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type ta_string_table

  let ta_string_table : ta_string_table structure typ =
    structure "TA_StringTable"

  let ta_func_handle = F.uint
  let ta_func_flags = F.int

  type ta_func_info

  let ta_func_info : ta_func_info structure typ =
    let structure = structure "TA_FuncInfo" in
    let _ = field structure "name" (const @@ ptr @@ char) in
    let _ = field structure "group" (const @@ ptr @@ char) in
    let _ = field structure "hint" (const @@ ptr @@ char) in
    let _ = field structure "camelCaseName" (const @@ ptr @@ char) in
    let _ = field structure "flags" ta_func_flags in
    let _ = field structure "nbInput" F.uint in
    let _ = field structure "nbOptInput" F.uint in
    let _ = field structure "nbOutput" F.uint in
    let _ = field structure "handle" @@ const @@ ptr @@ ta_func_handle in
    seal structure;
    structure
end
