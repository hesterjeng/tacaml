module T = Tacaml__Ta_lib

let () =
  T.TA.Common.initialize ();
  let _ = T.TA.FuncTable.allocate () in
  ()
