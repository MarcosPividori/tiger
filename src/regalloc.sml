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
      fun acc lst = foldl (fn (tmp, d) => dictInsert d tmp
                                          (newTemp(), allocLocal frame true))
                          (dictNewStr()) lst

      fun intersec [] _ = []
        | intersec _ [] = []
        | intersec [x] (y::ys) = if x = y then [x] else intersec [x] ys
        | intersec (x::xs) ys = intersec [x] ys @ intersec xs ys

      fun replace d [] = []
        | replace d (x::xs) = case dictSearch d x of
            NONE => x :: replace d xs
          | SOME (t,_) => t :: replace d xs

      fun store [] _ = []
        | store (d::dd) accDst = case dictGet accDst d of
            (t, InFrame off) => codegen frame
                  (MOVE (MEM (BINOP (PLUS, CONST off, TEMP FP)), TEMP t))
                @ store dd accDst
          | _ => raise Fail "Never happen"

      fun load [] _ = []
        | load (s::ss) accSrc = case dictGet accSrc s of
            (t, InFrame off) => codegen frame
                  (MOVE (TEMP t, MEM (BINOP (PLUS, CONST off, TEMP FP))))
                @ load ss accSrc
          | _ => raise Fail "Never happen"

      fun update [] = []
        | update (AOPER {assem, src, dst, jump} ::xs) = let
            val srcInt = intersec spillLst src
            val accSrc = acc srcInt
            val dstInt = intersec spillLst dst
            val accDst = acc dstInt
          in load srcInt accSrc @
             [AOPER {assem=assem, src=replace accSrc src,
                    dst=replace accDst dst, jump=jump}] @
             store dstInt accDst @ update xs
          end
        | update (AMOVE {assem, src, dst} ::xs) = let
            val srcInt = intersec spillLst [src]
            val accSrc = acc srcInt
            val dstInt = intersec spillLst [dst]
            val accDst = acc dstInt
          in load srcInt accSrc @
             [AMOVE {assem=assem, src=hd (replace accSrc [src]),
                    dst=hd (replace accDst [dst])}] @
             store dstInt accDst @ update xs
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
