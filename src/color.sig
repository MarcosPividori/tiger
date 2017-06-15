signature color =
sig

type allocation = (temp.temp, frame.register) dict.Dict

(* Produces an extension of the initial allocation, that assigns all temps used
 * in the flow graph, making use of registers from the registers list, along
 * with a list of spills. *)
val color: {
      (* an interference graph *)
      interference: liveness.igraph,
      (* initial allocation (precoloring) of some temporaries imposed by calling
       * conventions *)
      initial: allocation,
      (* the spilling cost of each temporary, which can be just the number of
       * uses and defs, or better yet, uses and defs weighted by occurrence
       * in loops and nested loops. *)
      spillCost: graph.graph -> graph.node -> int,
      (* list of colors (registers) *)
      registers: frame.register list}
        ->
          (* extended allocation and list of spills *)
          allocation * temp.temp list
end
