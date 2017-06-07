structure graph :> graph = struct

open hashtable

type node = int
type 'data GraphTable = (node, 'data) HashT

type graph = {maxNodeNum: int,
              succ: node list GraphTable,
              pred: node list GraphTable,
              matrix: ((node*node), unit) HashT}

fun nodes ({succ,...}:graph) = htKeys succ

fun succ ({succ,...}:graph) nod = htGet succ nod

fun pred ({pred,...}:graph) nod = htGet pred nod

fun newGraph () = {maxNodeNum=0, succ=htNew(), pred=htNew(), matrix=htNew()}

fun addNode {maxNodeNum, pred, succ, matrix} =
     ({maxNodeNum = maxNodeNum+1,
       pred = htInsert pred maxNodeNum [],
       succ = htInsert succ maxNodeNum [],
       matrix = matrix}, maxNodeNum)

fun isEdge ({matrix,...}:graph) a b = case htSearch matrix (a,b) of
        SOME _ => true
      | NONE => false

fun addEdge (g as {maxNodeNum, pred, succ, matrix}) a b =
      if isEdge g a b
        then g
        else {maxNodeNum = maxNodeNum,
              pred = htRInsert pred b (a::(htGet pred b)),
              succ = htRInsert succ a (b::(htGet succ a)),
              matrix = htInsert matrix (a,b) ()}

fun rmEdge (g as {maxNodeNum, pred, succ, matrix}) a b = let
        fun rmFromList _ [] = []
          | rmFromList a (x::xs) = if a = x then xs else x :: rmFromList a xs
      in
        if not (isEdge g a b)
          then g
          else {maxNodeNum = maxNodeNum,
                pred = htRInsert pred b (rmFromList a (htGet pred b)),
                succ = htRInsert succ a (rmFromList b (htGet succ a)),
                matrix = htRemove matrix (a,b)}
      end

val nodeToString = Int.toString

end 
