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
      fun newTemps lst = let fun f (tmp, d) = if dictExists d tmp then d
                                              else dictInsert d tmp (newTemp())
                          in foldl f (dictNewStr()) lst end

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
        | store (d::dd) tmps = case (dictGet tmps d, dictGet acc d) of
            (t, InFrame off) => codegen frame
                  (MOVE (MEM (BINOP (PLUS, CONST off, TEMP FP)), TEMP t))
                @ store dd tmps
          | _ => raise Fail "Never happen"

      fun load [] _ = []
        | load (s::ss) tmps = case (dictGet tmps s, dictGet acc s) of
            (t, InFrame off) => codegen frame
                  (MOVE (TEMP t, MEM (BINOP (PLUS, CONST off, TEMP FP))))
                @ load ss tmps
          | _ => raise Fail "Never happen"

      fun update [] = []
        | update (AOPER {assem, src, dst, jump} ::xs) = let
            val srcInt = intersec spillLst src
            val dstInt = intersec spillLst dst
            val tmps = newTemps (dstInt @ srcInt)
          in load srcInt tmps @
             [AOPER {assem=assem, src=replace tmps src,
                     dst=replace tmps dst, jump=jump}] @
             store dstInt tmps @ update xs
          end
        | update (AMOVE {assem, src, dst} ::xs) = let
            val srcInt = intersec spillLst [src]
            val dstInt = intersec spillLst [dst]
            val tmps = newTemps (dstInt @ srcInt)
          in load srcInt tmps @
             [AMOVE {assem=assem, src=hd (replace tmps [src]),
                     dst=hd (replace tmps [dst])}] @
             store dstInt tmps @ update xs
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
