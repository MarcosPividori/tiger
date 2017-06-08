signature dict =
sig

type ('a, 'b) Dict

exception notExists

val dictNew     : ('a * 'a -> order) -> ('a, 'b) Dict

val dictNewStr  : unit -> (string, 'b) Dict

val dictNewInt  : unit -> (int, 'b) Dict

val dictExists  : ('a, 'b) Dict -> 'a -> bool

val dictInsert  : ('a, 'b) Dict -> 'a -> 'b -> ('a, 'b) Dict

val dictInsList : ('a, 'b) Dict -> ('a * 'b) list -> ('a, 'b) Dict

val dictRInsert : ('a, 'b) Dict -> 'a -> 'b -> ('a, 'b) Dict

val dictRemove  : ('a, 'b) Dict -> 'a -> ('a, 'b) Dict

val dictSearch  : ('a, 'b) Dict -> 'a -> 'b option

val dictGet     : ('a, 'b) Dict -> 'a -> 'b

val dictApp     : ('a -> 'b) -> ('c, 'a) Dict -> ('c, 'b) Dict

val dictToList  : ('a, 'b) Dict -> ('a * 'b) list

val dictKeys    : ('a, 'b) Dict -> 'a list

end
