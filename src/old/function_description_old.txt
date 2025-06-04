[@@@warning "-33"]

open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  module StringTable = struct
    let ta_group_table_alloc =
      foreign "TA_GroupTableAlloc"
        ((ptr @@ ptr @@ Types_generated.StringTable.stringtable)
        @-> returning int)
  end

  let initialize = foreign "TA_Initialize" (void @-> returning void)

  (* let ta_group_table_alloc = *)
  (*   foreign "TA_GroupTableAlloc" *)
  (*     ((ptr @@ ptr @@ Type_description.StringTable.t) @-> returning int) *)
  (* (\* ((ptr @@ ptr @@ Types.StringTable.t) @-> returning int) *\) *)

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

  module MAType = struct end

  module Func = struct
    let ta_atr =
      foreign "TA_ATR" @@ int @-> int @-> ptr double @-> ptr double
      @-> ptr double @-> int @-> ptr int @-> ptr int @-> ptr double
      @-> returning int

    (* TA_LIB_API TA_RetCode TA_STOCH( int    startIdx, *)
    (*                                 int    endIdx, *)
    (*                                            const double inHigh[], *)
    (*                                            const double inLow[], *)
    (*                                            const double inClose[], *)
    (*                                            int           optInFastK_Period, /* From 1 to 100000 */ *)
    (*                                            int           optInSlowK_Period, /* From 1 to 100000 */ *)
    (*                                            TA_MAType     optInSlowK_MAType, *)
    (*                                            int           optInSlowD_Period, /* From 1 to 100000 */ *)
    (*                                            TA_MAType     optInSlowD_MAType, *)
    (*                                            int          *outBegIdx, *)
    (*                                            int          *outNBElement, *)
    (*                                            double        outSlowK[], *)
    (*                                            double        outSlowD[] ); *)
    let ta_stoch =
      foreign "TA_STOCH" @@ int @-> int @-> ptr double @-> ptr double
      @-> ptr double @-> int @-> int @-> int @-> int @-> int @-> ptr int
      @-> ptr int @-> ptr double @-> ptr double @-> returning int

    (* TA_LIB_API TA_RetCode TA_SMA( int    startIdx, *)
    (*                               int    endIdx, *)
    (*                                          const double inReal[], *)
    (*                                          int           optInTimePeriod, /* From 2 to 100000 */ *)
    (*                                          int          *outBegIdx, *)
    (*                                          int          *outNBElement, *)
    (*                                          double        outReal[] ); *)

    let ta_sma =
      foreign "TA_SMA" @@ int @-> int @-> ptr double @-> int @-> ptr int
      @-> ptr int @-> ptr double @-> returning int
  end
end
