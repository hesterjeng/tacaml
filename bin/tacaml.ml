module T = Tacaml__Ta_lib

let () =
  T.TA.Common.initialize ();
  let x = T.TA.GroupTable.allocate () in
  Format.printf "@[%a@]@." T.TA.GroupTable.pp x;
  ()
