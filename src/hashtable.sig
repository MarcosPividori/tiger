signature hashtable =
sig

type ('a, 'b) HashT

exception notExists

val htNew  : unit -> (''a, 'b) HashT

val htExists  : (''a, 'b) HashT -> ''a -> bool

val htInsert  : (''a, 'b) HashT -> ''a -> 'b -> (''a, 'b) HashT

val htInsList : (''a, 'b) HashT -> (''a * 'b) list -> (''a, 'b) HashT

val htRInsert : (''a, 'b) HashT -> ''a -> 'b -> (''a, 'b) HashT

val htRemove  : (''a, 'b) HashT -> ''a -> (''a, 'b) HashT

val htSearch  : (''a, 'b) HashT -> ''a -> 'b option

val htGet     : (''a, 'b) HashT -> ''a -> 'b

val htFromList : (''a * 'b) list -> (''a, 'b) HashT

val htApp     : (''a -> 'b) -> ('c, ''a) HashT -> ('c, 'b) HashT

val htAApp    : (''a -> ''c) -> ('b -> 'd) -> (''a, 'b) HashT -> (''c, 'd) HashT

val htRAApp   : (''a -> ''c) -> ('b -> 'd) -> (''a, 'b) HashT -> (''c, 'd) HashT

val htToList  : (''a, 'b) HashT -> (''a * 'b) list

val htFilter  : ('b -> bool) -> (''a, 'b) HashT -> (''a, 'b) HashT

val htFirst   : ('b -> bool) -> (''a, 'b) HashT -> (''a * 'b)

val htKeys    : (''a, 'b) HashT -> ''a list

end
