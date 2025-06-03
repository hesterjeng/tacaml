module TA = struct
  module CT = Ctypes

  let ptr_of_string x = CT.CArray.of_string x |> CT.CArray.start
  let ptr_ptr ty dec = CT.allocate (CT.ptr ty) dec

  module Common = struct
    let initialize () = C.Functions.initialize ()
  end

  module StringTable = struct
    (* include C.Type.StringTable *)
    module Self = Type_description.StringTable
      (* C.Type.StringTable *)

    type t = Self.t

    let string_field = Self.string_field

    let declare () : t =
      let x = CT.structure "TA_StringTable" in
      let res = CT.from_voidp x CT.null in
      res

    (* let size (x : t) = *)
    (*   let open Ctypes in *)
    (*   Format.printf "@[getting size@]@."; *)
    (*   let res = CT.getf !@x C.Type.StringTable.size_field in *)
    (*   Unsigned.UInt.to_int res *)

    let to_list (table_ptr : t) : string list =
      let open Ctypes in
      let count = size table_ptr in
      Format.printf "@[count: %d@]@." count;
      let table = !@table_ptr in
      let strings_ptr = getf table string_field in
      let rec extract acc i =
        if i < 0 then acc
        else
          let cstr_ptr = !@(strings_ptr +@ i) in
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
    (* include C.Type.StringTable *)
    module Self = C.Type.StringTable

    type t = Self.t

    let t = Self.t

    let allocate () =
      let ta_string_table = StringTable.declare () in
      let gt_ptr = ptr_ptr t ta_string_table in
      let res = C.Functions.ta_group_table_alloc gt_ptr in
      match res with
      | 0 -> ta_string_table
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

(* module C = Func *)
