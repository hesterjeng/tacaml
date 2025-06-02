[@@@warning "-33"]

open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type ta_string_table

  let ta_string_table : ta_string_table structure typ =
    structure "TA_StringTable"

  let ta_func_handle = typedef F.uint "TA_FuncHandle"
  let ta_func_flags = typedef F.int "TA_FuncFlags"

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

  type ta_output_parameter_type = TA_Output_Real | TA_Output_Integer

  let ta_output_parameter_type =
    let outputreal = constant "TA_Output_Real" int64_t in
    let outputinteger = constant "TA_Output_Integer" int64_t in
    let res =
      enum "TA_OutputParameterType" ~typedef:true
        [ (TA_Output_Real, outputreal); (TA_Output_Integer, outputinteger) ]
    in
    res

  let ta_input_flags = typedef F.int "TA_InputFlags"
  let ta_opt_input_flags = typedef F.int "TA_OptInputFlags"
  let ta_output_flags = typedef F.int "TA_OutputFlags"

  type ta_input_parameter_info

  let ta_input_parameter_info : ta_input_parameter_info structure typ =
    let structure = structure "TA_InputParameterInfo" in
    let _ = field structure "type" @@ ta_input_parameter_type in
    let _ = field structure "paramName" @@ ptr @@ const char in
    let _ = field structure "flags" @@ ta_input_flags in
    seal structure;
    structure

  type ta_opt_input_parameter_info

  let ta_opt_input_parameter_info : ta_opt_input_parameter_info structure typ =
    let structure = structure "TA_OptInputParameterInfo" in
    let _ = field structure "type" @@ ta_opt_input_parameter_type in
    let _ = field structure "paramName" @@ ptr @@ const char in
    let _ = field structure "flags" @@ ta_opt_input_flags in
    let _ = field structure "displayName" @@ ptr @@ const char in
    let _ = field structure "dataSet" @@ ptr @@ const void in
    let _ = field structure "defaultValue" @@ double in
    let _ = field structure "hint" @@ ptr @@ const char in
    let _ = field structure "helpFile" @@ ptr @@ const char in
    seal structure;
    structure

  type ta_output_parameter_info

  let ta_output_parameter_info : ta_output_parameter_info structure typ =
    let structure = structure "TA_OutputParameterInfo" in
    let _ = field structure "type" @@ ta_output_parameter_type in
    let _ = field structure "paramName" @@ ptr @@ const char in
    let _ = field structure "flags" @@ ta_output_flags in
    seal structure;
    structure

  type ta_param_holder

  let ta_param_holder : ta_param_holder structure typ =
    let structure = structure "TA_ParamHolder" in
    let _ = field structure "hiddenData" @@ ptr void in
    seal structure;
    structure
end
