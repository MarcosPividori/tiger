signature frame =
sig

type frame

val FP : temp.temp
val SP : temp.temp
val RV : temp.temp
val RAX : temp.temp
val RDX : temp.temp

datatype access = InFrame of int | InReg of temp.label

val WSize : int
val FpPrev : int
val FpPrevLev : int

val argregs : temp.temp list
val calldefs : temp.temp list
val callersaves : temp.temp list

val newFrame : {name: temp.label, formals: bool list} -> frame
val name : frame -> temp.label
val formals : frame -> access list

val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access

val maxRegFrame : frame -> int
val exp : access -> tree.exp -> tree.exp
val externalCall : string * tree.exp list -> tree.exp

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

end
