structure temp :> temp = struct
(* label is some machine-language location whose exact address is yet to be
 determined -just like a label in assembly language. *)
type label = string
(* temp is a value that is temporarily held in a register. *)
type temp = string

local
  val tempi = ref 0
  val labeli = ref 0
in
  fun newTemp() =
    let val s = "T"^Int.toString(!tempi)
    in tempi := !tempi+1; s end

  fun newLabel() =
    let val s = "L"^Int.toString(!labeli)
    in labeli := !labeli+1; s end
end

end
