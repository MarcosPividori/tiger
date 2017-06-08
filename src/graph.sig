signature graph =
sig

type graph
type node

type 'data GraphTable = (node, 'data) dictionary.Dict
val newGraphTable: unit -> 'a GraphTable

val newGraph: unit -> graph

val nodes: graph -> node list
val succ: graph -> node -> node list
val pred: graph -> node -> node list

val addNode: graph -> graph * node
val isEdge: graph -> node -> node -> bool
val addEdge: graph -> node -> node -> graph
val rmEdge: graph -> node -> node -> graph

val nodeToString: node -> string (* for debugging *)

end
