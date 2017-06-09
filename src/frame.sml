structure frame :> frame = struct
(*
  Frames for x64:
   + Pass as many parameters as will fit in registers, in the order:
       rdi, rsi, rdx, rcx, r8, r9. (max 6 params in registers)
   + Additional parameters are pushed on the stack, and are removed by the
       caller after the call.

    |    argn    |  fp+8*(n-5)
    |    ...     |
    |    arg8    |  fp+24
    |    arg7    |  fp+16
    |  retorno   |  fp+8
    |   fp ant   |  fp
    --------------  fp
    |  fp level  |  fp-8
    |   local2   |  fp-16
    |   local3   |  fp-24
    |    ...     |
    |   localn   |  fp-8*n
*)

open tree
open dict

val FP = "rbp"            (* frame pointer register *)
val SP = "rsp"            (* stack pointer register *)
val RV = "rax"            (* return value register *)
val RAX = "rax"
val RDX = "rdx"

val WSize = 8             (* word size in bytes *)
val FpPrev = 0            (* offset (bytes) for prev fp *)
val FpPrevLev = ~WSize    (* offset (bytes) for the static link *)

val argsInitial = 0       (* number of args by default *)
val argsOffInitial = 2    (* offset for the first arg in stack, in words *)
val argsGap = WSize       (* number of bytes for each arg *)
val localsInitial = 0     (* initial number of locals *)
val localsOffInitial = ~1 (* offset for the first local, in words *)

val specialregs = [RV, FP, SP] (* special purpose registers *)
val argregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
              (* registers for passing first args *)
val callersaves = ["r10", "r11"]
                  (* registers that must be preserved by the caller *)
val calleesaves = ["rbx", "r12", "r13", "r14", "r15"]
                  (* registers that must be preserved by the callee *)
val calldefs = [RV] @ argregs @ callersaves
               (* registers possibly written by the callee *)
val machineRegs = argregs @ callersaves @ calleesaves
                  (* all registers available for coloring *)

val tempMap = dictInsList (dictNewStr())
                          (ListPair.zip (machineRegs, machineRegs))

datatype access = InFrame of int
                | InReg of temp.temp

type frame = {
  name: string,
  formals: access list, (* one access per parameter *)
  actualLocal: int ref (* counter of locals in the given frame *)
}

type register = string

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

fun allocLocal ({actualLocal, ...}: frame) true =
      let val ret = (!actualLocal+localsOffInitial) * WSize
        val _ = actualLocal := (!actualLocal-1)
      in InFrame ret end
  | allocLocal _ false = InReg (temp.newTemp())

fun newFrame {name, formals} = let
    val nregs = length argregs
    val f : frame = {name=name, formals=[], actualLocal=ref localsInitial}
    fun allocArg _ [] = []
      | allocArg argn (escaped::xs) =
          (if argn < nregs
            then if escaped
                   then allocLocal f true
                   else InReg (List.nth(argregs, argn))
            else InFrame ((argn - nregs + argsOffInitial) * WSize))
          :: allocArg (argn+1) xs
  in {name=name, formals=allocArg 0 formals, actualLocal= #actualLocal f} end

fun name ({name, ...}: frame) = name

fun formals ({formals, ...}: frame) = formals

fun exp (InFrame k) = MEM (BINOP (PLUS, TEMP FP, CONST k))
  | exp (InReg l) = TEMP l

fun externalCall (s, l) = ESEQ (EXP (CALL (NAME s, l)), TEMP RV)

(* Add statements to save the escaping arguments and save/restore callee save *)
fun procEntryExit1 ({formals, ...}: frame, body) = let
    fun seq [] = EXP (CONST 0)
      | seq [s] = s
      | seq (x::xs) = SEQ (x, seq xs)
    fun moveArgToStack ((InFrame k)::params) (reg::regs) =
          (MOVE (exp (InFrame k), TEMP reg)) :: (moveArgToStack params regs)
      | moveArgToStack (_::params) (_::regs) = moveArgToStack params regs
      | moveArgToStack _ _ = []
  in seq ((moveArgToStack formals argregs) @ [body])
     (* TODO: incomplete, need to save/restore callee saves *)
  end

(* ... *)
fun procEntryExit2 (frame, body) = body @
     [assem.AOPER {assem="", src=[SP, FP, RV] @ calleesaves, dst=[], jump=[]}]

(* Add instructions to update the SP according to the frame size before and
 * after the function's body *)
fun procEntryExit3 ({name, ...}: frame, body) =
      {prolog = "PROCEDURE " ^ name ^ "\n",
       body = body,
       epilog = "END " ^ name ^ "\n"}
      (* TODO: incomplete *)
end
