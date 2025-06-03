module TA = struct
  module GroupTable = struct
    type t

    (* external alloc : unit -> t = "TA_GroupTableAlloc" *)
  end

  (* external add : int -> int -> int = "add" *)
  let add _ _ =
    let market_strength =
      Ctypes.CArray.of_string "Market Strength" |> Ctypes.CArray.start
    in
    let ta_string_table =
      (* Ctypes.make C.Type.ta_string_table |> Ctypes.addr |> *)
      (* Ctypes.reference_type *)
    in
    let x = C.Functions.ta_func_table_alloc market_strength @@ ta_string_table in
    x
end

(* module C = Func *)
