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

type t = Sma | Ema | Wma | Dema | Tema | Trima | Kama | Mama | T3
[@@deriving hash, compare, show]

let to_int = function
  | Sma -> 0
  | Ema -> 1
  | Wma -> 2
  | Dema -> 3
  | Tema -> 4
  | Trima -> 5
  | Kama -> 6
  | Mama -> 7
  | T3 -> 8

let of_int = function
  | 0 -> Ok Sma
  | 1 -> Ok Ema
  | 2 -> Ok Wma
  | 3 -> Ok Dema
  | 4 -> Ok Tema
  | 5 -> Ok Trima
  | 6 -> Ok Kama
  | 7 -> Ok Mama
  | 8 -> Ok T3
  | i -> Error (`BadMAType i)

(* Parse Ma_type from string *)
let parse s =
  match String.uppercase_ascii (String.trim s) with
  | "SMA" -> Ok Sma
  | "EMA" -> Ok Ema
  | "WMA" -> Ok Wma
  | "DEMA" -> Ok Dema
  | "TEMA" -> Ok Tema
  | "TRIMA" -> Ok Trima
  | "KAMA" -> Ok Kama
  | "MAMA" -> Ok Mama
  | "T3" -> Ok T3
  | _ -> Error ("Invalid Ma_type: " ^ s)
