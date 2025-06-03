[@@@warning "-33"]

open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let initialize = foreign "TA_Initialize" (void @-> returning void)

  let ta_group_table_alloc =
    foreign "TA_GroupTableAlloc"
      ((ptr @@ ptr @@ Type_description.StringTable.t) @-> returning int)
      (* ((ptr @@ ptr @@ Types.StringTable.t) @-> returning int) *)

  (* let ta_group_table_free = *)
  (*   foreign "TA_GroupTableFree" @@ ptr Types.StringTable.t @-> returning int *)

  (* let ta_func_table_alloc = *)
  (*   foreign "TA_FuncTableAlloc" *)
  (*   @@ (ptr @@ const char) *)
  (*   @-> (ptr @@ ptr @@ Types.StringTable.t) *)
  (*   @-> returning int *)

  (* let ta_func_table_free = *)
  (*   foreign "TA_FuncTableFree" @@ (ptr @@ Types.StringTable.t) @-> returning int *)

  (* let ta_get_func_handle = *)
  (*   foreign "TA_GetFuncHandle" *)
  (*   @@ (ptr @@ const char) *)
  (*   @-> (ptr @@ ptr @@ const Types.ta_func_handle) *)
  (*   @-> returning int *)

  (* let ta_get_func_info = *)
  (*   foreign "TA_GetFuncInfo" *)
  (*   @@ (ptr @@ const Types.ta_func_handle) *)
  (*   @-> (ptr @@ ptr @@ const Types.ta_func_info) *)
  (*   @-> returning int *)
end
