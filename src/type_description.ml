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

  type ta_real_range

  let ta_real_range : ta_real_range structure typ =
    let structure = structure "TA_RealRange" in
    let _ = field structure "min" float in
    let _ = field structure "max" float in
    let _ = field structure "precision" int in
    seal structure;
    structure

  type ta_integer_range

  let ta_integer_range : ta_integer_range structure typ =
    let structure = structure "TA_IntegerRange" in
    let _ = field structure "min" int in
    let _ = field structure "max" int in
    seal structure;
    structure

  type ta_real_data_pair

  let ta_real_data_pair : ta_real_data_pair structure typ =
    let structure = structure "TA_RealDataPair" in
    let _ = field structure "value" float in
    let _ = field structure "string" (ptr @@ const char) in
    seal structure;
    structure

  type ta_integer_data_pair

  let ta_integer_data_pair : ta_integer_data_pair structure typ =
    let structure = structure "TA_IntegerDataPair" in
    let _ = field structure "value" int in
    let _ = field structure "string" (ptr @@ const char) in
    seal structure;
    structure

  type ta_real_list

  let ta_real_list : ta_real_list structure typ =
    let structure = structure "TA_RealList" in
    let _ = field structure "data" @@ ptr @@ const ta_real_data_pair in
    let _ = field structure "nbElement" (const uint) in
    seal structure;
    structure

  type ta_integer_list

  let ta_integer_list : ta_integer_list structure typ =
    let structure = structure "TA_IntegerList" in
    let _ = field structure "data" @@ ptr @@ const ta_integer_data_pair in
    let _ = field structure "nbElement" (const uint) in
    seal structure;
    structure

  type ta_input_parameter_type =
    | TA_Input_Price
    | TA_Input_Real
    | TA_Input_Integer

  let ta_input_parameter_type =
    let input_price = constant "TA_Input_Price" int64_t in
    let input_real = constant "TA_Input_Real" int64_t in
    let input_integer = constant "TA_Input_Integer" int64_t in
    let res =
      enum "TA_InputParameterType" ~typedef:true
        [
          (TA_Input_Price, input_price);
          (TA_Input_Real, input_real);
          (TA_Input_Integer, input_integer);
        ]
    in
    res

  type ta_opt_input_parameter_type =
    | TA_OptInput_RealRange
    | TA_OptInput_RealList
    | TA_OptInput_IntegerRange
    | TA_OptInput_IntegerList

  let ta_opt_input_parameter_type =
    let realrange = constant "TA_OptInput_RealRange" int64_t in
    let reallist = constant "TA_OptInput_RealList" int64_t in
    let integerrange = constant "TA_OptInput_IntegerRange" int64_t in
    let integerlist = constant "TA_OptInput_IntegerList" int64_t in
    let res =
      enum "TA_OptInputParameterType" ~typedef:true
        [
          (TA_OptInput_RealRange, realrange);
          (TA_OptInput_RealList, reallist);
          (TA_OptInput_IntegerRange, integerrange);
          (TA_OptInput_IntegerList, integerlist);
        ]
    in
    res
end
