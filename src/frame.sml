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
val machineRegs = specialregs @ argregs @ callersaves @ calleesaves
                  (* all registers available for coloring *)

val tempMap = dictInsList (dictNewStr())
                          (ListPair.zip (machineRegs, machineRegs))

type register = string

fun regToString reg = reg

datatype access = InFrame of int
                | InReg of temp.temp

type frame = {
  name: temp.label,
  origName: string,
  formals: access list, (* one access per parameter *)
  actualLocal: int ref (* counter of locals in the given frame *)
}

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

fun stringLen s =
    let fun aux[] = 0
          | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
          | aux(_::t) = 1+aux(t)
    in aux(explode s) end

fun string label s = let
      val len = "  .quad " ^ makestring(stringLen s)
      val str = "  .ascii \""^s^"\""
    in label ^ ":\n" ^ len ^ "\n" ^ str end

fun allocLocal ({actualLocal, ...}: frame) true =
      let val ret = (!actualLocal+localsOffInitial) * WSize
        val _ = actualLocal := (!actualLocal-1)
      in InFrame ret end
  | allocLocal _ false = InReg (temp.newTemp())

fun newFrame {name, origName, formals} = let
    val nregs = length argregs
    val f : frame = {name=name, origName=origName,
                     formals=[], actualLocal=ref localsInitial}
    fun allocArg _ [] = []
      | allocArg argn (escaped::xs) =
          (if argn < nregs
            then allocLocal f escaped
            else InFrame ((argn - nregs + argsOffInitial) * WSize))
          :: allocArg (argn+1) xs
  in {name=name, origName=origName, formals=allocArg 0 formals,
      actualLocal= #actualLocal f} end

fun name ({name, ...}: frame) = name

fun formals ({formals, ...}: frame) = formals

fun exp (InFrame k) = MEM (BINOP (PLUS, TEMP FP, CONST k))
  | exp (InReg l) = TEMP l

fun externalCall (s, l) = ESEQ (EXP (CALL (NAME s, l)), TEMP RV)

val compareRegister = String.compare

(* Add statements to save the escaping arguments and save/restore callee save *)
fun procEntryExit1 ({formals, ...}: frame) body = let
    fun seq [] = EXP (CONST 0)
      | seq [s] = s
      | seq (x::xs) = SEQ (x, seq xs)
    fun moveArgToStack ((InFrame k)::params) (reg::regs) =
          (MOVE (exp (InFrame k), TEMP reg)) :: moveArgToStack params regs
      | moveArgToStack ((InReg t)::params) (reg::regs) =
          (MOVE (TEMP t, TEMP reg)) :: moveArgToStack params regs
      | moveArgToStack _ _ = []
    val (preCS,postCS) = foldl
          (fn (t, (a, b)) => let val fresh = temp.newTemp()
                              in (MOVE (TEMP fresh, TEMP t) :: a,
                                  MOVE (TEMP t, TEMP fresh) :: b) end)
          ([], []) calleesaves
  in seq ((moveArgToStack formals argregs) @ preCS @ [body] @ postCS)
  end

(* Set src and dst to ensure calleesaves registers are preserved *)
fun procEntryExit2 body =
     [assem.AOPER {assem="", src=[], dst=[SP, FP] @ calleesaves, jump=[]}] @
     body @
     [assem.AOPER {assem="", src=[SP, FP] @ calleesaves, dst=[], jump=[]}]

(* Add instructions to update the SP according to the frame size before and
 * after the function's body *)
fun procEntryExit3 ({name, origName, actualLocal,...}: frame) body =
     let val size = (!actualLocal) * WSize * ~1
     in concat [
       ".globl "^name^"\n",
       name ^ ": # \"" ^ origName ^ "\"\n",
       "  pushq %rbp\n",
       "  movq %rsp, %rbp\n",
       "  subq $"^Int.toString(size)^", %rsp\n",
       body,
       "  movq %rbp, %rsp\n",
       "  popq %rbp\n",
       "  ret\n",
       "##############\n"]
     end
end
