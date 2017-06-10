structure liveness :> liveness = struct

open assem
open graph
open dict
open temp
open Splayset
open List

infixr 0 $
fun x $ y = x y

(* type to represent flow graphs *)
type flowgraph = {
    control: graph,
    def: temp list GraphTable,
    use: temp list GraphTable,
    ismove: bool GraphTable}

(* The function instrs2graph takes a list of instructions and returns a
 * flow graph, along with a list of nodes that corresponds exactly to the
 * instructions. *)
fun instrs2graph instrLst =
      let
        fun addJump graph labToNod node [] = (graph, labToNod)
          | addJump graph labToNod node (x::xs) = case dictSearch labToNod x of
                SOME labNod =>
                    addJump (addEdge graph (node, labNod)) labToNod node xs
              | NONE => let
                  val (graph1, labNod) = addNode graph
                  val labToNod1 = dictInsert labToNod x labNod
                in addJump (addEdge graph1 (node, labNod)) labToNod1 node xs end

        fun addInst (AOPER {assem, src, dst, jump},
                     ({control, def, use, ismove}, labToNod, nodes)) =
              let val (graph, nod) = addNode control
                  val def1 = dictInsert def nod dst
                  val use1 = dictInsert use nod src
                  val ismove1 = dictInsert ismove nod false
                  val (control1, labToNod1) = case (jump, nodes) of
                          ([], []) => (graph, labToNod) (* last instr *)
                        | ([], next::_) => (addEdge graph (nod, next), labToNod)
                        | _ => addJump graph labToNod nod jump
               in ({control=control1, def=def1, use=use1, ismove=ismove1},
                   labToNod1, nod::nodes) end
          | addInst (ALABEL {assem, lab},
                     (fg as {control, def, use, ismove}, labToNod, nodes)) =
              let val (graph, nod) = case dictSearch labToNod lab of
                          SOME nod => (control, nod)
                        | NONE => addNode control
                  val def1 = dictInsert def nod []
                  val use1 = dictInsert use nod []
                  val ismove1 = dictInsert ismove nod false
                  val labToNod1 = dictInsert labToNod lab nod
                  val control1 = case nodes of
                          [] => graph (* last instr *)
                        | next::_ => addEdge graph (nod, next)
               in ({control=control1, def=def1, use=use1, ismove=ismove1},
                   labToNod1, nod::nodes) end
          | addInst (AMOVE {assem, src, dst},
                     ({control, def, use, ismove}, labToNod, nodes)) =
              let val (graph, nod) = addNode control
                  val def1 = dictInsert def nod [dst]
                  val use1 = dictInsert use nod [src]
                  val ismove1 = dictInsert ismove nod true
                  val control1 = case nodes of
                          [] => graph (* last instr *)
                        | next::_ => addEdge graph (nod, next)
               in ({control=control1, def=def1, use=use1, ismove=ismove1},
                   labToNod, nod::nodes) end

        val (fgraph, _, nodeLst) = let
              val initFg = {control=newGraph(), def=newGraphTable(),
                               use=newGraphTable(), ismove=newGraphTable()}
               in foldr addInst (initFg, dictNewStr(), []) instrLst end
      in
        (fgraph, nodeLst)
      end

(* showfgraph just prints out, for debugging purposes, a list of nodes in the
 * flow graph, and for each node, a list of nodes adjacent to it. *)
fun showfgraph ({control, def, use, ismove}:flowgraph) instrList nodeLst = let
      fun printList [] = ()
        | printList [x] = print x
        | printList (x::xs) = (print x; print ", "; printList xs)
      val listInstrNode = ListPair.zip (instrList, nodeLst)
      fun printInstr (inst, nod) = let
            val suc = map nodeToString $ listItems $ succ control nod
             in (print $ nodeToString nod; print ": ";
                 print $ assem.format (fn n => n) inst;
                 print "  => "; printList suc;
                 print "  (use: "; printList $ dictGet use nod;
                 print " def: "; printList $ dictGet def nod;
                 print " ismove: ";
                 print $ (Bool.toString $ dictGet ismove nod) ^ ") \n") end
      val _ = print "Flow graph:\n"
    in app printInstr listInstrNode end

(* type to represent interference graphs *)
type igraph = {
    graph: graph,
    tnode: (temp, node) Dict,
    gtemp: temp GraphTable,
    moves: (node * node) list}

(* interferenceGraph takes a flow graph and returns two kinds of information:
 * an interference graph (igraph) and a table mapping each flow-graph node to
 * the set of temps that are live-out at that node. *)
fun interferenceGraph {control, def, use, ismove} nodeLst =
      let val emptySet = empty String.compare

        val innInit = foldr (fn (nod, dic) => dictInsert dic nod emptySet)
                        (newGraphTable ()) nodeLst

        val outInit = innInit

        fun updateInOut (nod, (inn, out, flag)) = let
              val innNod = dictGet inn nod
              val outNod = dictGet out nod
              val useNod = addList (emptySet, dictGet use nod)
              val defNod = addList (emptySet, dictGet def nod)
              val succNod = listItems $ succ control nod
              val outNod1 = foldr union emptySet $ map (dictGet inn) succNod
              val innNod1 = union (useNod, difference (outNod1, defNod))
              val inn1 = dictRInsert inn nod innNod1
              val out1 = dictRInsert out nod outNod1
              val change = (not $ equal (outNod, outNod1))
                    orelse (not $ equal (innNod, innNod1))
            in (inn1, out1, change orelse flag) end

        fun getFixedPoint (inn, out, false) = out
          | getFixedPoint (inn, out, true) =
              getFixedPoint $ foldr updateInOut (inn, out, false) nodeLst

        val out = getFixedPoint (innInit, outInit, true)

        val listTemps = concat (map #2 (dictToList def) @
                                map #2 (dictToList use))
                      @ dictKeys frame.tempMap
                      (* machine registers are always present in igraphs *)

        fun addTemp (tmp, (g, tnode, gtemp)) = case dictSearch tnode tmp of
                SOME nod => (g, tnode, gtemp)
              | NONE => let val (g1, nod) = addNode g
                in (g1, dictInsert tnode tmp nod, dictInsert gtemp nod tmp) end

        val (initGraph, tnode, gtemp) = foldl addTemp
                           (newGraph(), dictNewStr(), newGraphTable()) listTemps

        fun addInter (nod, graph) = let
              val defTmp = dictGet def nod
              val useTmp = dictGet use nod
              val defNod = map (dictGet tnode) defTmp
              fun getEdges a = map (fn n => (a, n)) defNod @
                               map (fn n => (n, a)) defNod
              val outNod = map (dictGet tnode) $ listItems $ difference
                ((if dictGet ismove nod
                   then difference (dictGet out nod, addList (emptySet, useTmp))
                   else dictGet out nod), addList (emptySet, defTmp))
              val edgeLst = concat $ map getEdges outNod
            in foldl (fn (e, g) => addEdge g e) graph edgeLst end

        val interfGraph = foldl addInter initGraph nodeLst

        val moves = map (fn nod => let
                            val useNod = dictGet tnode $ hd $ dictGet use nod
                            val defNod = dictGet tnode $ hd $ dictGet def nod
                           in (useNod, defNod) end)
              $ filter (dictGet ismove) nodeLst
      in
        ({graph=interfGraph, tnode=tnode, gtemp=gtemp, moves=moves},
         dictApp listItems out)
      end

(* showigraph just prints out, for debugging purposes, a list of nodes in the
 * interference graph, and for each node, a list of nodes adjacent to it. *)
fun showigraph ({graph, gtemp, tnode, moves}:igraph) = let
      val tmpLst = map (dictGet gtemp) $ nodes graph
      fun succTmp t = map (dictGet gtemp) $ listItems
                        $ succ graph $ dictGet tnode t
      fun printList [] = ()
        | printList [x] = print x
        | printList (x::xs) = (print x; print ", "; printList xs)
      fun printTmp t = (print t; print " => ";
                        printList $ succTmp t; print "\n")
      val _ = print "Interference graph:\n"
    in app printTmp tmpLst end
end
