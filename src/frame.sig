signature frame =
sig

type frame
type register = string

val rv : temp.temp
val ov : temp.temp
val fp : temp.temp

datatype access = InFrame of int | InReg of temp.label

val fpPrev : int
val fpPrevLev : int
val newFrame : {name: temp.label, formals: bool list} -> frame
val name : frame -> temp.label
val formals : frame -> access list
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val sp : temp.temp
val maxRegFrame : frame -> int
val wSz : int
val log2WSz : int
val calldefs : temp.temp list
val callersaves : temp.temp list
val exp : access -> tree.exp -> tree.exp
val externalCall : string * tree.exp list -> tree.exp

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

end
