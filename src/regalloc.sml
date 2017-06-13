structure regalloc = struct

open assem
open codegen
open tree
open color
open frame
open graph
open dict
open liveness
open temp

fun addSpill frame instrLst spillLst = let
      fun newTemps lst = foldl (fn (tmp, d) => dictInsert d tmp (newTemp()))
                               (dictNewStr()) lst

      val acc = foldl (fn (tmp, d) => dictInsert d tmp (allocLocal frame true))
                      (dictNewStr()) spillLst

      fun intersec [] _ = []
        | intersec _ [] = []
        | intersec [x] (y::ys) = if x = y then [x] else intersec [x] ys
        | intersec (x::xs) ys = intersec [x] ys @ intersec xs ys

      fun replace d [] = []
        | replace d (x::xs) = case dictSearch d x of
            NONE => x :: replace d xs
          | SOME t => t :: replace d xs

      fun store [] _ = []
        | store (d::dd) tmpsDst = case (dictGet tmpsDst d, dictGet acc d) of
            (t, InFrame off) => codegen frame
                  (MOVE (MEM (BINOP (PLUS, CONST off, TEMP FP)), TEMP t))
                @ store dd tmpsDst
          | _ => raise Fail "Never happen"

      fun load [] _ = []
        | load (s::ss) tmpsSrc = case (dictGet tmpsSrc s, dictGet acc s) of
            (t, InFrame off) => codegen frame
                  (MOVE (TEMP t, MEM (BINOP (PLUS, CONST off, TEMP FP))))
                @ load ss tmpsSrc
          | _ => raise Fail "Never happen"

      fun update [] = []
        | update (AOPER {assem, src, dst, jump} ::xs) = let
            val srcInt = intersec spillLst src
            val tmpsSrc = newTemps srcInt
            val dstInt = intersec spillLst dst
            val tmpsDst = newTemps dstInt
          in load srcInt tmpsSrc @
             [AOPER {assem=assem, src=replace tmpsSrc src,
                     dst=replace tmpsDst dst, jump=jump}] @
             store dstInt tmpsDst @ update xs
          end
        | update (AMOVE {assem, src, dst} ::xs) = let
            val srcInt = intersec spillLst [src]
            val tmpsSrc = newTemps srcInt
            val dstInt = intersec spillLst [dst]
            val tmpsDst = newTemps dstInt
          in load srcInt tmpsSrc @
             [AMOVE {assem=assem, src=hd (replace tmpsSrc [src]),
                     dst=hd (replace tmpsDst [dst])}] @
             store dstInt tmpsDst @ update xs
          end
        | update ((l as ALABEL {assem, lab}) ::xs) = l :: update xs
    in
      update instrLst
    end

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
       | _ => alloc (addSpill frame instrLst spillLst) frame
    end

end
