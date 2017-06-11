signature frame =
sig

type frame
type register

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
val machineRegs : register list

val tempMap : (temp.temp, register) dict.Dict
val regToString: register -> string

val newFrame : {name: temp.label, formals: bool list} -> frame
val name : frame -> temp.label
val formals : frame -> access list

val string: temp.label -> string -> string

val allocLocal : frame -> bool -> access

val exp : access -> tree.exp
val externalCall : string * tree.exp list -> tree.exp

val compareRegister: register * register -> order

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

val procEntryExit1 : frame -> tree.stm -> tree.stm

val procEntryExit2 : assem.instr list -> assem.instr list

val procEntryExit3 : frame -> string -> string

end
