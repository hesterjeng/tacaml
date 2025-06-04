[@@@warning "-33"]

open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let initialize = foreign "TA_Initialize" (void @-> returning int)

  let ta_atr =
    foreign "TA_ATR" @@ int @-> int @-> ptr double @-> ptr double @-> ptr double
    @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int

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
