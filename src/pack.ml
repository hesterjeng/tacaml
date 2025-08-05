module S = Safe

type t = Pack : ('a, 'b) S.t -> t

let pack x = Pack x

let to_string (Pack x : t) =
  let str = Safe.to_string x in
  str

let pp = fun fmt (Pack x : t) -> Format.fprintf fmt "%a" Safe.pp x
let hash (Pack x : t) = Safe.hash x
let equal (Pack x : t) (Pack y : t) = Safe.equal x y

