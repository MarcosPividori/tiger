signature graph =
sig

type graph
type node
type edge = node * node

type 'data GraphTable = (node, 'data) dict.Dict
val newGraphTable: unit -> 'a GraphTable

val newGraph: unit -> graph

val nodes: graph -> node list
val succ: graph -> node -> node Splayset.set
val pred: graph -> node -> node Splayset.set

val addNode: graph -> graph * node
val rmNode: graph -> node -> graph
val isEdge: graph -> edge -> bool
val addEdge: graph -> edge -> graph
val rmEdge: graph -> edge -> graph

val listEdges: graph -> edge list

val addUndEdge: graph -> edge -> graph
val rmUndEdge: graph -> edge -> graph

val degree: graph -> node -> int

val nodeToString: node -> string (* for debugging *)
val compareNode: node*node -> order

val undGraphFromList: edge list -> graph

val coalesceUndEdge: graph -> edge -> graph

end
