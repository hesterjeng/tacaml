module TA = struct
  module CT = C.Type

  let ptr_of_string x = Ctypes.CArray.of_string x |> Ctypes.CArray.start
  let ptr_ptr ty dec = Ctypes.allocate (CT.ptr ty) dec

  module Common = struct
    let initialize () = C.Functions.initialize ()
  end

  module StringTable = struct
    include C.Type.StringTable
    include C.Functions.StringTable

    let size x =
      (* let open CT in *)
      try
        let res = Ctypes.getf Ctypes.(!@x) Types_generated.StringTable.size in
        Unsigned.UInt.to_int res
      with
      | e ->
        let typ = Ctypes.string_of_typ stringtable in
        Format.printf "@[problem getting string table size. %s sealed: %b@]@."
          typ !Type_description.sealed;
        raise e

    let string_field x =
      (* let open CT in *)
      Ctypes.getf Ctypes.(!@x) Types_generated.StringTable.string

    let declare () =
      let x = CT.structure "TA_StringTable" in
      let res = Ctypes.from_voidp x Ctypes.null in
      res

    let to_list table_ptr : string list =
      let count = size table_ptr in
      Format.printf "@[count: %d@]@." count;
      let strings_ptr = string_field table_ptr in
      let rec extract acc i =
        if i < 0 then acc
        else
          let cstr_ptr = Ctypes.(!@(strings_ptr +@ i)) in
          let ocaml_str =
            Ctypes.string_from_ptr cstr_ptr
              ~length:
                (String.length (Ctypes.string_from_ptr cstr_ptr ~length:1024))
          in
          extract (ocaml_str :: acc) (i - 1)
      in
      extract [] (count - 1)
  end

  module GroupTable = struct
    type t = StringTable.stringtable Ctypes.structure Ctypes.ptr

    let pp : t Format.printer =
     fun fmt x ->
      let count = StringTable.size x in
      let l = StringTable.to_list x in
      Format.fprintf fmt "@[%d %a@]@." count (List.pp String.pp) l

    let allocate () : t =
      let ta_string_table = StringTable.declare () in
      let gt_ptr = ptr_ptr StringTable.stringtable ta_string_table in
      let res = StringTable.ta_group_table_alloc gt_ptr in
      match res with
      | 0 ->
        Format.printf "@[Group table allocated@]@.";
        ta_string_table
      | x ->
        Format.printf "@[%d@]@." x;
        invalid_arg "Bad response allocating GroupTable"
  end

  module FuncTable = struct
    (* include C.Type.StringTable *)

    (* let allocate x = *)
    (*   let market_strength = ptr_of_string x in *)
    (*   let ta_string_table = StringTable.declare () in *)
    (*   let address_ta_string_table = ptr_ptr t ta_string_table in *)
    (*   let res = *)
    (*     C.Functions.ta_func_table_alloc market_strength *)
    (*     @@ address_ta_string_table *)
    (*   in *)
    (*   match res with *)
    (*   | 0 -> ta_string_table *)
    (*   | x -> *)
    (*     Format.printf "@[%d@]@." x; *)
    (*     invalid_arg "Bad response allocating FuncTable" *)
  end
end

module Func = struct
  (* let sma start_idx end_idx values = *)
  (*   C.Functions.Func.ta_sma *)
end

(* module C = Func *)
