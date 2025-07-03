type t = {
  open_ : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  high : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  low : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  close : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  volume : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
