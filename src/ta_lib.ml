module TA = struct
  module GroupTable = struct
    type t

    (* external alloc : unit -> t = "TA_GroupTableAlloc" *)
  end

  (* external add : int -> int -> int = "add" *)
  let add _ _ = 0
end
