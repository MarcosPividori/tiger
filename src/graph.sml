structure graph :> graph = struct

open dict
open Splayset

infixr 0 $
fun x $ y = x y

type node = int
type 'data GraphTable = (node, 'data) Dict

type edge = node * node

fun newGraphTable () = dictNewInt()

type graph = {maxNodeNum: int,
              succ: node set GraphTable,
              pred: node set GraphTable}

fun nodes ({succ,...}:graph) = dictKeys succ

fun succ ({succ,...}:graph) nod = dictGet succ nod

fun pred ({pred,...}:graph) nod = dictGet pred nod

fun comparePair ((n11, n12), (n21, n22)) = case Int.compare (n11, n21) of
        EQUAL => Int.compare (n12, n22)
      | r => r

fun newGraph () = {maxNodeNum=0,
                   succ=newGraphTable(),
                   pred=newGraphTable()}

fun addNode {maxNodeNum, pred, succ} =
     ({maxNodeNum = maxNodeNum+1,
       pred = dictInsert pred maxNodeNum $ empty Int.compare,
       succ = dictInsert succ maxNodeNum $ empty Int.compare}, maxNodeNum)

fun isNode ({succ,...}:graph) n = case dictSearch succ n of
        SOME _ => true
      | NONE => false

fun isEdge ({succ,...}:graph) (a, b) = case dictSearch succ a of
        SOME set => member (set, b)
      | NONE => false

fun addEdge (g as {maxNodeNum, pred, succ}) (a, b) =
      if isEdge g (a, b)
        then g
        else {maxNodeNum = maxNodeNum,
              pred = dictRInsert pred b $ add (dictGet pred b, a),
              succ = dictRInsert succ a $ add (dictGet succ a, b)}

fun addUndEdge g (a, b) = addEdge (addEdge g (a, b)) (b, a)

fun rmEdge (g as {maxNodeNum, pred, succ}) (a, b) =
        if not $ isEdge g (a, b)
          then g
          else {maxNodeNum = maxNodeNum,
                pred = dictRInsert pred b $ delete (dictGet pred b, a),
                succ = dictRInsert succ a $ delete (dictGet succ a, b)}

fun rmNode (g as {maxNodeNum, pred, succ}) nod =
      let val pre = listItems $ dictGet pred nod
        val suc = listItems $ dictGet succ nod
        val edges = map (fn a => (a, nod)) pre @ map (fn a => (nod, a)) suc
        val {maxNodeNum=maxNodeNum1, pred=pred1, succ=succ1} =
              List.foldl (fn (e, graph) => rmEdge graph e) g edges
      in {maxNodeNum=maxNodeNum1,
          pred=dictRemove pred1 nod,
          succ=dictRemove succ1 nod}
      end

fun listEdges ({succ, pred, ...}:graph) = List.concat $
      map (fn (a, neigh) => map (fn b => (a, b)) $ listItems neigh) $
      dictToList succ @ dictToList pred

fun rmUndEdge g (a, b) = rmEdge (rmEdge g (a, b)) (b, a)

fun degree ({succ,...}:graph) nod = numItems $ dictGet succ nod

val nodeToString = Int.toString

val compareNode = Int.compare

fun undGraphFromList edgeLst = let
      fun addNod (nod, g as {maxNodeNum, pred, succ}) =
        case dictSearch pred nod of
          SOME _ => g
        | NONE => {maxNodeNum = if nod > maxNodeNum then nod else maxNodeNum,
                   pred = dictInsert pred nod $ empty Int.compare,
                   succ = dictInsert succ nod $ empty Int.compare}
      val g1 = List.foldl addNod (newGraph()) $ map #1 edgeLst @ map #2 edgeLst
    in List.foldl (fn (e, g) => addUndEdge g e) g1 edgeLst end

fun coalesceUndEdge g (a, b) = let
        val succB = succ g b
        val succB1 = if member (succB, a) then delete (succB, a) else succB
        val g1 = rmNode g b
      in foldl (fn (n, gg) => addUndEdge gg (a, n)) g1 succB1 end

end
