signature trans = sig

type access
type frag = frame.frag

type level
val getDepth : level -> int
val outermost : level
val newLevel : {parent: level, name: temp.label,
                origName: string, formals: bool list} -> level
val formals : level -> access list
val allocLocal : level -> bool -> access

type exp

val procEntryExit : level -> exp -> unit

val getResult : unit -> frag list

val unitExp : unit -> exp

val nilExp : unit -> exp

val intExp : int -> exp

val stringExp : string -> exp

val simpleVar : int -> access -> int -> exp

val varDec : access -> int -> exp
val fieldVar : exp -> int -> exp
val subscriptVar : exp -> exp -> exp

val recordExp : (exp * int) list -> exp

val callExp : level -> temp.label -> bool -> bool -> level -> exp list -> exp

type looplvl
val nilLoopLevel : looplvl
val newLoopLevel : unit -> looplvl

val whileExp : {test: exp, body: exp, lev:level, looplvl: looplvl} -> exp
val forExp : {lo: exp, hi: exp, var: exp, body: exp, looplvl: looplvl} -> exp

exception breakexc
val breakExp : looplvl -> exp

val seqExp : exp list -> exp

val ifThenExp : {test: exp, then': exp} -> exp
val ifThenElseExp : {test: exp, then': exp, else': exp} -> exp
val ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp

val assignExp : exp -> exp -> exp

val functionDec : exp -> level -> bool -> exp

val binOpIntExp : abs.oper -> exp -> exp -> exp
val binOpIntRelExp: abs.oper -> exp -> exp -> exp
val binOpStrExp : abs.oper -> exp -> exp -> exp

val arrayExp : {size: exp, init: exp} -> exp

end
