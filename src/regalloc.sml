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

infixr 0 $
fun x $ y = x y

fun addSpill frame auxTmps instrLst spillLst = let
      fun newTemps lst = let fun f (tmp, d) = if dictExists d tmp then d
                                              else dictInsert d tmp $ newTemp()
                          in foldl f (dictNewStr()) lst end

      val acc = foldl (fn (tmp, d) => dictInsert d tmp $ allocLocal frame true)
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

      fun update updated auxTmps [] = (rev updated, auxTmps)
        | update updated auxTmps (AOPER {assem, src, dst, jump} ::xs) = let
            val srcInt = intersec spillLst src
            val dstInt = intersec spillLst dst
            val tmps = newTemps $ dstInt @ srcInt
            val auxTmps1 = Splayset.addList (auxTmps, map #2 $ dictToList tmps)
          in update ((store dstInt tmps) @
                     [AOPER {assem=assem, src=replace tmps src,
                             dst=replace tmps dst, jump=jump}] @
                     (load srcInt tmps) @
                     updated) auxTmps1 xs
          end
        | update updated auxTmps (AMOVE {assem, src, dst} ::xs) = let
            val srcInt = intersec spillLst [src]
            val dstInt = intersec spillLst [dst]
            val tmps = newTemps (dstInt @ srcInt)
            val auxTmps1 = Splayset.addList (auxTmps, map #2 $ dictToList tmps)
          in update ((store dstInt tmps) @
                     [AMOVE {assem=assem, src=hd $ replace tmps [src],
                             dst=hd $ replace tmps [dst]}] @
                     (load srcInt tmps) @
                     updated) auxTmps1 xs
          end
        | update updated auxTmps ((l as ALABEL {assem, lab}) ::xs) =
            update (l :: updated) auxTmps xs

    in
      update [] auxTmps instrLst
    end

fun spillCost auxTmps gtemp tbound igraph nod =
      if Splayset.member (auxTmps,dictGet gtemp nod)
        then tbound + 10
        else tbound - (degree igraph nod)

(* given a list of assem instructions and the associated frame, returns a
 * register allocation with a new list of instructions including some
 * modifications for spilling.
 * alloc: instr list -> frame -> instr list * allocation *)

fun alloc instrLst frame = let
    (* allocAux keeps track of the new temps used for spilled temps. We don't
     * want to spill these auxiliary temps. *)
    fun allocAux instrLst frame auxTmps = let
        val (fgraph, nodeLst) = instrs2graph instrLst
        val (igraph, liveOut) = interferenceGraph fgraph nodeLst
        val (allocFn, spillLst) = color {
                      interference=igraph,
                      initial=tempMap,
                      spillCost=spillCost auxTmps (#gtemp igraph)
                      (*top bound for degree*) (length $ nodes $ #graph igraph),
                      registers=machineRegs}
      in case spillLst of
           [] => (instrLst, allocFn) (* nothing to spill => done *)
         | _ => let val (instrSpilled, auxTmps1) =
                      addSpill frame auxTmps instrLst spillLst
                in allocAux instrSpilled frame auxTmps1 end
      end
 in allocAux instrLst frame $ Splayset.empty String.compare end

end
