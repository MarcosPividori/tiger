signature liveness = sig

(* type to represent flow graphs *)
type flowgraph = {
    (* control is a directed graph wherein each node represents an instr *)
    control: graph.graph,
    (* def is a table of the temporaries defined at each node. *)
    def: temp.temp list graph.GraphTable,
    (* use is a table of the temporaries used at each node *)
    use: temp.temp list graph.GraphTable,
    (* ismove tells whether each instruction is a MOVE instruction *)
    ismove: bool graph.GraphTable}

(* The function instrs2graph takes a list of instructions and returns a
 * flow graph, along with a list of nodes that corresponds exactly to the
 * instructions. *)
val instrs2graph: assem.instr list -> flowgraph * graph.node list

(* showfgraph just prints out, for debugging purposes, a list of nodes in the
 * flow graph, and for each node, a list of nodes adjacent to it. *)
val showfgraph: flowgraph -> assem.instr list -> graph.node list -> unit

(* type to represent interference graphs *)
type igraph = {
    (* graph is an undirected graph wherein each node represents a temp *)
    graph: graph.graph,
    (* tnode is a mapping from temps of the Assem program to graph nodes *)
    tnode: (temp.temp, graph.node) dict.Dict,
    (* gtemp is the inverse mapping, from graph nodes back to temporaries *)
    gtemp: temp.temp graph.GraphTable,
    (* moves is a list of move instructions *)
    moves: (graph.node * graph.node) list}

(* interferenceGraph takes a flow graph and returns two kinds of information:
 * an interference graph (igraph) and a table mapping each flow-graph node to
 * the set of temps that are live-out at that node. *)
val interferenceGraph: flowgraph
                    -> graph.node list
                    -> igraph * (temp.temp list graph.GraphTable)

(* showigraph just prints out, for debugging purposes, a list of nodes in the
 * interference graph, and for each node, a list of nodes adjacent to it. *)
val showigraph: igraph -> unit

end
