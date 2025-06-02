[@@@warning "-33"]

open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let ta_group_table_alloc =
    foreign "TA_GroupTableAlloc"
      ((ptr @@ ptr @@ Types.ta_string_table) @-> returning int)

  let ta_group_table_free =
    foreign "TA_GroupTableFree" @@ ptr Types.ta_string_table @-> returning int

  let ta_func_table_alloc =
    foreign "TA_FuncTableAlloc"
    @@ ptr char
    @-> (ptr @@ ptr @@ Types.ta_string_table)
    @-> returning int

  let ta_func_table_free =
    foreign "TA_FuncTableFree"
    @@ (ptr @@ Types.ta_string_table)
    @-> returning int

  let ta_get_func_handle =
    foreign "TA_GetFuncHandle"
    @@ (const @@ ptr @@ char)
    @-> (const @@ ptr @@ ptr @@ Types.ta_func_handle)
    @-> returning int

  (* let ta_func_flag_overlap = *)
  (*   foreign "TA_FUNC_FLAG_OVERLAP" *)
end
