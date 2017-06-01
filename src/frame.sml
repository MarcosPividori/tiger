structure frame :> frame = struct
(*
  Frames for 80386 (no displays - registers):

    |    argn    |  fp+4*(n+1)
    |    ...     |
    |    arg2    |  fp+16
    |    arg1    |  fp+12
    |  fp level  |  fp+8
    |  retorno   |  fp+4
    |   fp ant   |  fp
    --------------  fp
    |   local1   |  fp-4
    |   local2   |  fp-8
    |    ...     |
    |   localn   |  fp-4*n
*)

open tree

val fp = "efp"            (* frame pointer register *)
val sp = "esp"            (* stack pointer register *)
val rv = "eax"            (* return value register *)
val ov = "edx"            (* overflow value register (edx en el 386) *)

val wSz = 4               (* word size in bytes *)
val log2WSz = 2           (* base two logarithm of word size in bytes *)
val fpPrev = 0            (* offset (bytes) for prev fp *)
val fpPrevLev = 2 * wSz   (* offset (bytes) for the static link *)

val argsInitial = 0       (* number of args by default *)
val argsOffInitial = 3    (* offset for the first arg, in words *)
val argsGap = wSz         (* number of bytes for each arg *)
val regInitial = 1        (* initial value for reg counter *)
val localsInitial = 0     (* initial number of locals *)
val localsOffInitial = ~1 (* offset for the first local, in words *)

val specialregs = [rv, fp, sp] (* special purpose registers *)
val argregs = []               (* registers for passing first args *)
val callersaves = []       (* registers that must be preserved by the caller *)
val calleesaves = []       (* registers that must be preserved by the callee *)
val calldefs = [rv] @ callersaves (* registers possibly written by the callee *)

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
