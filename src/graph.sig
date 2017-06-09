signature graph =
sig

type graph
type node
type edge = node * node

type 'data GraphTable = (node, 'data) dict.Dict
val newGraphTable: unit -> 'a GraphTable

val newGraph: unit -> graph

val nodes: graph -> node list
val succ: graph -> node -> node list
val pred: graph -> node -> node list

val addNode: graph -> graph * node
val isEdge: graph -> edge -> bool
val addEdge: graph -> edge -> graph
val rmEdge: graph -> edge -> graph

val nodeToString: node -> string (* for debugging *)

end
