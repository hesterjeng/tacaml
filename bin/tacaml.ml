module T = Tacaml__Ta_lib

let () =
  let res = T.TA.add 4 8 in
  Format.printf "%d" res;
  ()
