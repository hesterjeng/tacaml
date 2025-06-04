[@@@warning "-32"]

include Bigarray.Array1

type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

let create x = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout x
let init x f = Bigarray.Array1.init Bigarray.float64 Bigarray.c_layout x f

let pp : t Format.printer =
 fun fmt x ->
  let dim = dim x in
  let rec collect i acc =
    try
      let elt = get x i in
      collect (i + 1) (elt :: acc)
    with
    | _ -> acc
  in
  let l = collect 0 [] |> List.rev in
  Format.fprintf fmt "@[dim: %d@]@.@[%a@]@." dim (List.pp Float.pp) l
