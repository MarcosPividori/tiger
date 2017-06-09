structure graph :> graph = struct

open dict
open Splayset

type node = int
type 'data GraphTable = (node, 'data) Dict

type edge = node * node

fun newGraphTable () = dictNewInt()

type graph = {maxNodeNum: int,
              succ: node list GraphTable,
              pred: node list GraphTable,
              matrix: edge set}

fun nodes ({succ,...}:graph) = dictKeys succ

fun succ ({succ,...}:graph) nod = dictGet succ nod

fun pred ({pred,...}:graph) nod = dictGet pred nod

fun comparePair ((n11, n12), (n21, n22)) = case Int.compare (n11, n21) of
        EQUAL => Int.compare (n12, n22)
      | r => r

fun newGraph () = {maxNodeNum=0,
                   succ=newGraphTable(),
                   pred=newGraphTable(),
                   matrix=empty comparePair}

fun addNode {maxNodeNum, pred, succ, matrix} =
     ({maxNodeNum = maxNodeNum+1,
       pred = dictInsert pred maxNodeNum [],
       succ = dictInsert succ maxNodeNum [],
       matrix = matrix}, maxNodeNum)

fun isEdge ({matrix,...}:graph) (a, b) = case peek (matrix, (a, b)) of
        SOME _ => true
      | NONE => false

fun addEdge (g as {maxNodeNum, pred, succ, matrix}) (a, b) =
      if isEdge g (a, b)
        then g
        else {maxNodeNum = maxNodeNum,
              pred = dictRInsert pred b (a::(dictGet pred b)),
              succ = dictRInsert succ a (b::(dictGet succ a)),
              matrix = add (matrix, (a,b))}

fun rmEdge (g as {maxNodeNum, pred, succ, matrix}) (a, b) = let
        fun rmFromList _ [] = []
          | rmFromList a (x::xs) = if a = x then xs else x :: rmFromList a xs
      in
        if not (isEdge g (a, b))
          then g
          else {maxNodeNum = maxNodeNum,
                pred = dictRInsert pred b (rmFromList a (dictGet pred b)),
                succ = dictRInsert succ a (rmFromList b (dictGet succ a)),
                matrix = delete (matrix, (a,b))}
      end

val nodeToString = Int.toString

end
