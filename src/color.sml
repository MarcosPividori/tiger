structure color :> color = struct

open assem
open frame
open graph
open liveness
open temp
open dict
open Splayset
open List

infixr 0 $
fun x $ y = x y

type allocation = (temp.temp, frame.register) dict.Dict

(* Produces an extension of the initial allocation, that assigns all temps used
 * in the flow graph, making use of registers from the registers list, along
 * with a list of spills. *)
fun color {interference= ig as {graph, tnode, gtemp, moves},
           initial, spillCost, registers} =
    let
      (* max number of colors *)
      val colorNum = length registers

      val registersSet = addList (empty compareRegister, registers)

      fun simplify graph nodeStack moveNodSet =
           let fun validNod n = not (member (moveNodSet, n))
                        andalso (degree graph n) < colorNum
           in case filter validNod $ nodes graph of
                [] => (graph, nodeStack)
              | candidates => simplify (foldr (fn (n, g) => rmNode g n)
                                           graph candidates)
                                    (candidates @ nodeStack) moveNodSet end

      fun coalesce graph moveGraph = (* Briggs *)
        let fun coalesceMoveGraph g mg (a, b) =
              let val newNeigh = List.filter (fn x => not $ isEdge g (a, x))
                                             (listItems $ delete (succ mg b, a))
                  val mg1 = rmNode mg b
                  val mg2 = foldl (fn (x, mmg) => addUndEdge mmg (a, x))
                                  mg1 newNeigh
               in mg2 end
            fun considerMove _ [] st = st
              | considerMove a (b::xs) (lst, g, mg) =
              let val neighA = succ g a
                  val neighBNoA = difference (succ g b, neighA)
                  val aAndB = union (neighA, neighBNoA)
                  val nIt = length (List.filter (fn a => degree g a >= colorNum)
                                                (listItems aAndB))
               in if nIt < colorNum
                   then ((a,b)::lst, coalesceUndEdge g (b, a),
                         coalesceMoveGraph g mg (b, a))
                   else considerMove a xs (lst, g, mg) end
            fun considerMoveNode (n, st as (lst, g, mg)) =
                  if degree mg n = 0
                    then (lst, g, rmNode mg n)
                    else considerMove n (listItems (succ mg n)) st
        in foldl considerMoveNode ([], graph, moveGraph) (nodes moveGraph) end

      fun freeze graph moveGraph =
        case List.filter (fn n => degree graph n < colorNum)
                         $ nodes moveGraph of
          [] => ([], graph, moveGraph)
        | (x::_) => let
             val succX = succ moveGraph x
             val (g, mg) = (rmNode graph x, rmNode moveGraph x)
             val mg1 = Splayset.foldl (fn (y, mgg) => if degree mgg y = 0
                                                       then rmNode mgg y
                                                       else mgg) mg succX
           in ([x], g, mg1) end

      fun posSpill graph moveGraph = case nodes graph of
            [] => ([], graph, moveGraph)
          | (x::_) => ([x], rmNode graph x, if isNode moveGraph x
                                              then rmNode moveGraph x
                                              else moveGraph)

      datatype ST = Simplify | Coalesce | Freeze | PosSpill

      fun loop Simplify graph moveGraph nodeStack aliases =
            let val moveNodSet = addList (empty compareNode, nodes moveGraph)
              val (graph1, simLst) = simplify graph [] moveNodSet
            in loop Coalesce graph1 moveGraph (simLst @ nodeStack) aliases end
        | loop Coalesce graph moveGraph nodeStack aliases =
            let val (coalLst, graph1, moveGraph1) = coalesce graph moveGraph
              val aliases1 = foldl (fn ((a,b), al) => dictInsert al a b)
                                   aliases coalLst
            in case coalLst of
                 [] => loop Freeze graph1 moveGraph1 nodeStack aliases
               | _ => loop Simplify graph1 moveGraph1 nodeStack aliases1
            end
        | loop Freeze graph moveGraph nodeStack aliases =
            let val (frozenLst, graph1, moveGraph1) = freeze graph moveGraph
            in case frozenLst of
                 [] => loop PosSpill graph1 moveGraph1 nodeStack aliases
               | x::_ => loop Simplify graph1 moveGraph1 (x::nodeStack) aliases
            end
        | loop PosSpill graph moveGraph nodeStack aliases =
            let val (spillLst, graph1, moveGraph1) = posSpill graph moveGraph
            in case spillLst of
                 [] => (nodeStack, aliases) (* end *)
               | x::_ => loop Simplify graph1 moveGraph1 (x::nodeStack) aliases
            end

      fun tryDelete s e = if member (s, e) then delete (s, e) else s

      fun assignColor graph node2reg [] aliases spilled =
            let fun recIns [] [] all = all
                  | recIns [] x all = recIns x [] all
                  | recIns ((a, b)::xs) l all = case dictSearch all b of
                      NONE => recIns xs ((a, b)::l) all
                    | SOME r => recIns xs l $ dictInsert all a r
            in (recIns (dictToList aliases) [] node2reg, spilled) end
        | assignColor graph node2reg (x::xs) aliases spilled =
            let fun considerNeigh (nod, candidates) =
                  case dictSearch aliases nod of
                    SOME al => (case dictSearch node2reg al of
                          SOME reg => tryDelete candidates reg
                        | NONE => candidates)
                  | NONE => (case dictSearch node2reg nod of
                          SOME reg => tryDelete candidates reg
                        | NONE => candidates)
             val cand = Splayset.foldl considerNeigh registersSet $ succ graph x
            in if isEmpty cand
                then assignColor graph node2reg xs aliases (x::spilled)
                else assignColor graph (dictInsert node2reg x
                                       $ hd $ listItems cand) xs aliases spilled
            end

      (* moves that could be coalesced *)
      val candMoves = List.filter (fn (a, b) => not (isEdge graph (a, b))
                                                andalso a <> b) moves
      (* graph of the candidate moves *)
      val moveGraph = undGraphFromList candMoves

      val (nodeStack, aliases) =
            loop Simplify graph moveGraph [] $ dictNew compareNode

      val n2reg = foldl (fn ((a, b), d) => dictInsert d (dictGet tnode a) b)
                        (dictNew compareNode) $ dictToList initial
      val (node2reg, spilled) = assignColor graph n2reg nodeStack aliases []

      val tmp2reg = foldl (fn ((a, b), d) => dictInsert d (dictGet gtemp a) b)
                        (dictNewStr()) $ dictToList node2reg

(*
      val _ = showigraph {graph=moveGraph, tnode=tnode, gtemp=gtemp, moves=moves}
      val _ = showigraph {graph=g1, tnode=tnode, gtemp=gtemp, moves=moves}
*)

  in (tmp2reg, map (dictGet gtemp) spilled) end
end
