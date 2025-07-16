(* ENUM_BEGIN( MAType ) *)
(*    ENUM_DEFINE( TA_MAType_SMA,   Sma   ) =0, *)
(*    ENUM_DEFINE( TA_MAType_EMA,   Ema   ) =1, *)
(*    ENUM_DEFINE( TA_MAType_WMA,   Wma   ) =2, *)
(*    ENUM_DEFINE( TA_MAType_DEMA,  Dema  ) =3, *)
(*    ENUM_DEFINE( TA_MAType_TEMA,  Tema  ) =4, *)
(*    ENUM_DEFINE( TA_MAType_TRIMA, Trima ) =5, *)
(*    ENUM_DEFINE( TA_MAType_KAMA,  Kama  ) =6, *)
(*    ENUM_DEFINE( TA_MAType_MAMA,  Mama  ) =7, *)
(*    ENUM_DEFINE( TA_MAType_T3,    T3    ) =8 *)
(* ENUM_END( MAType ) *)

type t = SMA | EMA | WMA | DEMA | TEMA | TRIMA | KAMA | MAMA | T3

let to_int = function
  | SMA -> 0
  | EMA -> 1
  | WMA -> 2
  | DEMA -> 3
  | TEMA -> 4
  | TRIMA -> 5
  | KAMA -> 6
  | MAMA -> 7
  | T3 -> 8

let of_int = function
  | 0 -> Ok SMA
  | 1 -> Ok EMA
  | 2 -> Ok WMA
  | 3 -> Ok DEMA
  | 4 -> Ok TEMA
  | 5 -> Ok TRIMA
  | 6 -> Ok KAMA
  | 7 -> Ok MAMA
  | 8 -> Ok T3
  | i -> Error (`BadMAType i)

(* Parse Ma_type from string *)
let parse s =
  match String.uppercase_ascii (String.trim s) with
  | "SMA" -> Ok SMA
  | "EMA" -> Ok EMA
  | "WMA" -> Ok WMA
  | "DEMA" -> Ok DEMA
  | "TEMA" -> Ok TEMA
  | "TRIMA" -> Ok TRIMA
  | "KAMA" -> Ok KAMA
  | "MAMA" -> Ok MAMA
  | "T3" -> Ok T3
  | _ -> Error ("Invalid Ma_type: " ^ s)
