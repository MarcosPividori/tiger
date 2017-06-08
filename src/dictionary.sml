structure dictionary :> dictionary =
struct
open Splaymap

type ('a, 'b) Dict = ('a, 'b) dict

exception notExists

fun dictNew order = mkDict order

fun dictNewStr () = mkDict String.compare

fun dictNewInt () = mkDict Int.compare

fun dictExists dict key = isSome (peek (dict, key))

fun dictInsert dict key value = if dictExists dict key
                                 then dict
                                 else insert (dict, key, value)

fun dictInsList dict lst =
      List.foldl (fn ((a, b), dict2) => insert (dict2, a, b)) dict lst

fun dictRInsert dict key value = insert (dict, key, value)

fun dictRemove dict key = #1 (remove (dict,key)) handle _ => raise notExists

fun dictSearch dict key = peek (dict, key)

fun dictGet dict key = case dictSearch dict key of
    SOME value => value
  | _ => raise notExists

val dictToList = listItems

fun dictApp f = map (f o #2)

fun dictKeys dict = (List.map #1 o dictToList) dict

end
