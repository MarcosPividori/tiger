local
  fun mem x lst = List.exists (fn y => x = y) lst
  fun nexts a lst = List.map #2 (List.filter (fn (x,_) => x = a) lst)
in
  fun topsort graph = let
      fun sort [] path visited = visited
        | sort (x::xs) path visited =
          if mem x path then raise Fail "Cycle in graph!"
          else if mem x visited
            then sort xs path visited
            else sort xs path (x::(sort (nexts x graph) (x::path) visited))
      val (starts, _) = ListPair.unzip graph
    in sort starts [] [] end
end
