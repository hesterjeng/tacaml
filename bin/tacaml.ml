module TA = Tacaml__Ta_lib
module Arr = Tacaml__Arr

let top () =
  let ( let* ) = Result.( let* ) in
  let data = Arr.init 1000 @@ fun i -> Float.of_int i in
  (* let output = Arr.create 1000 in *)
  let* () = TA.initialize () in
  (* let* () = TA.sma 999 999 2 data output in *)
  let* res = TA.sma_top 2 data in
  Format.printf "%f" res;
  (* Format.printf "%a" Arr.pp output; *)
  Result.return @@ ()

let () =
  match top () with
  | Ok () ->
    Format.printf "@[OK@]@.";
    ()
  | Error e -> Format.printf "@[retcode %d@]@." e
