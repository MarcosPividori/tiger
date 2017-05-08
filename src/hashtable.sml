structure hashtable :> hashtable =
struct
open Polyhash

type (''a, 'b) HashT = (''a, 'b) hash_table

exception notExists

fun htNew () = mkPolyTable(100, notExists)

fun htExists ht = isSome o peek ht

fun htInsert ht key value = let val ht2 = copy ht
  in (peekInsert ht2 (key, value); ht2) end

fun htInsList ht lst = let val ht2 = copy ht
  in (List.app (insert ht2) lst; ht2) end

fun htRInsert ht key value = let val ht2 = copy ht
  in (insert ht2 (key, value); ht2) end

val htSearch = peek

fun htGet ht key = case htSearch ht key of
    SOME value => value
  | _ => raise notExists

fun htFromList lst = let val ht2 = htNew ()
  in (List.app (insert ht2) lst; ht2) end

val htToList = listItems

fun htApp f = map (f o #2)

fun htAApp f g = htFromList o List.map (fn (k, e) => (f k, g e)) o htToList

fun htRAApp f g =
  htFromList o List.map (fn (k, e) => (f k, g e)) o rev o htToList

fun htFilter f = htFromList o List.filter (f o #2) o htToList

fun htFirst f = hd o List.filter (f o #2) o htToList

fun htKeys ht = (List.map #1 o htToList) ht

end
