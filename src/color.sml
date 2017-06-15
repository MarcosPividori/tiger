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

      fun precoloredNode n = isSome $ dictSearch initial $ dictGet gtemp n

      fun rmNodeMG mg n = Splayset.foldl (fn (x, mmg) => if degree mmg x = 0
                                                     then rmNode mmg x else mmg)
                                         (rmNode mg n) $ succ mg n

      fun rmEdgeMG mg (a, b) =
            let val mg1 = rmUndEdge mg (a, b)
              val mg2 = if degree mg1 a = 0 then rmNode mg1 a else mg1
              val mg3 = if degree mg2 b = 0 then rmNode mg2 b else mg2
            in mg3 end

      fun simplify graph nodeStack moveNodSet =
           let fun validNod n = not (member (moveNodSet, n))
                        andalso not (precoloredNode n)
                        andalso (degree graph n) < colorNum
           in case filter validNod $ nodes graph of
                [] => (graph, nodeStack)
              | candidates => simplify (foldr (fn (n, g) => rmNode g n)
                                           graph candidates)
                                    (candidates @ nodeStack) moveNodSet end

      fun coalesce graph moveGraph = (* Briggs *)
        let fun coalesceGraph g mg (a, b) =
              let val g1 = coalesceUndEdge g (a, b)
                val mg1 = coalesceUndEdge mg (a, b)
                val mg2 = Splayset.foldl (fn (x, mmg) => if isEdge g1 (a, x)
                                              then rmEdgeMG mmg (a, x) else mmg)
                                         mg1 $ succ mg1 a
                val mg3 = if isNode mg2 a andalso degree mg2 a = 0
                            then rmNode mg2 a else mg2
               in (g1, mg3) end
            fun considerMove st _ []= st
              | considerMove (lst, g, mg) a (b::xs) =
              let val aAndB = union (succ g a, succ g b)
                  val nIt = length $ List.filter (fn a => degree g a >=colorNum
                                                        orelse precoloredNode a)
                                                 $ listItems aAndB
               in if nIt < colorNum
                   then let val (g1, mg1) = coalesceGraph g mg (b, a)
                         in ((a, b)::lst, g1, mg1) end
                   else considerMove (lst, g, mg) a xs end
            fun considerMoveNode (n, st as (lst, g, mg)) = if isNode mg n
                    then considerMove st n $ listItems $ succ mg n
                    else st
        in foldl considerMoveNode ([], graph, moveGraph)
                      $ List.filter (not o precoloredNode) $ nodes moveGraph end

      fun freeze graph moveGraph =
        case List.filter (fn n => degree graph n < colorNum
                                  andalso not $ precoloredNode n)
                         $ nodes moveGraph of
          [] => ([], graph, moveGraph)
        | (x::_) => let val (g, mg) = (rmNode graph x, rmNodeMG moveGraph x)
                     in ([x], g, mg) end

      fun posSpill graph moveGraph = case List.filter (not o precoloredNode)
                                                      $ nodes graph of
            [] => ([], graph, moveGraph)
          | lst => let
              val scLst = map (fn x => (x, spillCost graph x)) lst
              val (minNod,_) = foldl
                  (fn ((a, c), (mn, mc)) => if c < mc then (a, c) else (mn, mc))
                  (hd scLst) $ tl scLst
            in ([minNod], rmNode graph minNod, if isNode moveGraph minNod
                                                 then rmNodeMG moveGraph minNod
                                                 else moveGraph) end

      datatype ST = Simplify | Coalesce | Freeze | PosSpill

      fun loop Simplify graph moveGraph nodeStack aliases =
            let val moveNodSet = addList (empty compareNode, nodes moveGraph)
              val (graph1, simLst) = simplify graph [] moveNodSet
            in loop Coalesce graph1 moveGraph (simLst @ nodeStack) aliases end
        | loop Coalesce graph moveGraph nodeStack aliases =
            let val (coalLst, graph1, moveGraph1) = coalesce graph moveGraph
              val aliases1 = coalLst @ aliases
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

      fun assignColor graph node2reg aliases spilled [] =
            let fun addAlias ((a, b), n2r) = dictInsert n2r a $ dictGet n2r b
            in (foldl addAlias node2reg aliases, spilled) end
        | assignColor graph node2reg aliases spilled (x::xs) =
            let fun considerNeigh (nod, candidates) =
                  case dictSearch node2reg nod of
                    SOME reg => tryDelete candidates reg
                  | NONE => candidates
             val cand = Splayset.foldl considerNeigh registersSet $ succ graph x
            in if isEmpty cand
                then assignColor graph node2reg aliases (x::spilled) xs
                else assignColor graph (dictInsert node2reg x
                                       $ hd $ listItems cand) aliases spilled xs
            end

      (* moves that could be coalesced *)
      val candMoves = List.filter (fn (a, b) => not (isEdge graph (a, b))
                                                andalso a <> b) moves
      (* graph of the candidate moves *)
      val moveGraph = undGraphFromList candMoves

      (* get order for coloring *)
      val (nodeStack, aliases) = loop Simplify graph moveGraph [] []

      (* apply all the coalesces *)
      val coalGraph = foldr (fn ((a,b),g) => coalesceUndEdge g (b, a))
                            graph aliases

      (* convert temp2reg to node2reg *)
      val n2reg = foldl (fn ((t, reg), d) => dictInsert d (dictGet tnode t) reg)
                        (dictNew compareNode) $ dictToList initial

      (* assign colors following the nodeStack order *)
      val (node2reg, spilled) = assignColor coalGraph n2reg aliases [] nodeStack

      (* convert node2reg map to temp2reg *)
      val tmp2reg = foldl (fn ((n, r), d) => dictInsert d (dictGet gtemp n) r)
                          (dictNewStr()) $ dictToList node2reg

  in (tmp2reg, map (dictGet gtemp) spilled) end
end
