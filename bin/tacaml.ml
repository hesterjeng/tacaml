module T = Tacaml__Ta_lib

let () =
  T.TA.Common.initialize ();
  let x = T.TA.GroupTable.allocate () in
  let l = T.TA.StringTable.to_list x in
  Format.printf "@[%a@]@." (List.pp String.pp) l;
  ()
