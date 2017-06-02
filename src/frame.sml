structure frame :> frame = struct
(*
  Frames for x64:
   + Pass as many parameters as will fit in registers, in the order:
       rdi, rsi, rdx, rcx, r8, r9. (max 6 params in registers)
   + Additional parameters are pushed on the stack, and are removed by the
       caller after the call.

    |    argn    |  fp+8*(n-4)
    |    ...     |
    |    arg8    |  fp+32
    |    arg7    |  fp+24
    |  fp level  |  fp+16
    |  retorno   |  fp+8
    |   fp ant   |  fp
    --------------  fp
    |   local1   |  fp-8
    |   local2   |  fp-16
    |    ...     |
    |   localn   |  fp-8*n
*)

open tree

val fp = "rbp"            (* frame pointer register *)
val sp = "rsp"            (* stack pointer register *)
val rv = "rax"            (* return value register *)
val ov = "rdx"            (* overflow value register (edx en el 386) *)

val wSz = 8               (* word size in bytes *)
val log2WSz = 3           (* base two logarithm of word size in bytes *)
val fpPrev = 0            (* offset (bytes) for prev fp *)
val fpPrevLev = 2 * wSz   (* offset (bytes) for the static link *)

val argsInitial = 0       (* number of args by default *)
val argsOffInitial = 3    (* offset for the first arg, in words *)
val argsGap = wSz         (* number of bytes for each arg *)
val regInitial = 1        (* initial value for reg counter *)
val localsInitial = 0     (* initial number of locals *)
val localsOffInitial = ~1 (* offset for the first local, in words *)

val specialregs = [rv, fp, sp] (* special purpose registers *)
val argregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
              (* registers for passing first args *)
val callersaves = ["rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11"]
                  (* registers that must be preserved by the caller *)
val calleesaves = ["rbp", "rsp", "rbx", "r12", "r13", "r14", "r15"]
                  (* registers that must be preserved by the callee *)
val calldefs = [rv] @ callersaves (* registers possibly written by the callee *)
val rax = "rax"
val rdx = "rdx"

type frame = {
  name: string,
  formals: bool list, (* one boolean per parameter, true if not escaped *)
  actualArg: int ref, (* counter of arguments in the given frame *)
  actualLocal: int ref, (* counter of locals in the given frame *)
  actualReg: int ref
}

datatype access = InFrame of int
                | InReg of temp.temp

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

fun newFrame {name, formals} = {
  name=name,
  formals=formals,
  actualArg=ref argsInitial,
  actualLocal=ref localsInitial,
  actualReg=ref regInitial
}

fun name ({name, ...}: frame) = name

fun formals ({formals=f, ...}: frame) =
  let fun aux n [] = []
        | aux n (h::t) = InFrame(n) :: (aux (n+argsGap) t)
  in aux argsInitial f end

fun maxRegFrame ({actualReg, ...}: frame) = !actualReg

fun allocArg ({actualArg, ...}: frame) true =
      let val ret = (!actualArg+argsOffInitial) * wSz
        val _ = actualArg := !actualArg+1
      in InFrame ret end
  | allocArg _ false = InReg (temp.newTemp())

fun allocLocal ({actualLocal, ...}: frame) true =
      let val ret = (!actualLocal+localsOffInitial) * wSz
        val _ = actualLocal := (!actualLocal-1)
      in InFrame ret end
  | allocLocal _ false = InReg (temp.newTemp())

fun exp (InFrame k) _ = MEM (BINOP (PLUS, TEMP fp, CONST k))
  | exp (InReg l) _ = TEMP l

fun externalCall (s, l) = CALL (NAME s, l)

end
