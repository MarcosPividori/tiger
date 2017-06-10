structure regalloc = struct

open assem
open color
open frame
open graph
open dict
open liveness
open temp

fun addSpill instrLst spillLst = instrLst (* TODO: do actual spilling *)

fun spillCost nod = 1 (* TODO: improve spillCost *)

(* given a list of assem instructions and the associated frame, returns a
 * register allocation with a new list of instructions including some
 * modifications for spilling.
 * alloc: instr list -> frame -> instr list * allocation *)

fun alloc instrLst frame = let
      val (fgraph, nodeLst) = instrs2graph instrLst
      val (igraph, liveOut) = interferenceGraph fgraph nodeLst
      val (allocFn, spillLst) = color {interference=igraph, initial=tempMap,
                                     spillCost=spillCost, registers=machineRegs}
    in case spillLst of
         [] => (instrLst, allocFn) (* nothing to spill => done *)
       | _ => alloc (addSpill instrLst spillLst) frame
    end

end
