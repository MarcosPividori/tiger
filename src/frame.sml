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

val fp = "FP"          (* frame pointer *)
val sp = "SP"          (* stack pointer *)
val rv = "RV"          (* return value  *)
val ov = "OV"          (* overflow value (edx en el 386) *)
val wSz = 4            (* word size in bytes *)
val log2WSz = 2        (* base two logarithm of word size in bytes *)
val fpPrev = 0         (* offset (bytes) *)
val fpPrevLev = 8      (* offset (bytes) *)
val argsInicial = 0    (* words *)
val argsOffInicial = 0 (* words *)
val argsGap = wSz      (* bytes *)
val regInicial = 1     (* reg *)
val localsInicial = 0  (* words *)
val localsGap = ~4     (* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = []
val calleesaves = []

type frame = {
  name: string,
  formals: bool list,
  locals: bool list,
  actualArg: int ref,
  actualLocal: int ref,
  actualReg: int ref
}

type register = string

datatype access = InFrame of int
                | InReg of temp.temp

datatype frag = PROC of {body: tree.stm, frame: frame}
              | STRING of temp.label * string

fun newFrame {name, formals} = {
  name=name,
  formals=formals,
  locals=[],
  actualArg=ref argsInicial,
  actualLocal=ref localsInicial,
  actualReg=ref regInicial
}

fun name ({name, ...}: frame) = name

fun formals ({formals=f, ...}: frame) =
  let fun aux(n, []) = []
        | aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
  in aux (argsInicial, f) end

fun maxRegFrame ({actualReg, ...}: frame) = !actualReg

fun allocArg ({actualArg, ...}: frame) true =
      let val ret = (!actualArg+argsOffInicial)*wSz
        val _ = actualArg := !actualArg+1
      in InFrame ret end
  | allocArg _ false = InReg(temp.newTemp())

fun allocLocal ({actualLocal, ...}: frame) true =
      let val ret = InFrame(!actualLocal+localsGap)
      in actualLocal := (!actualLocal-1); ret end
  | allocLocal _ false = InReg(temp.newTemp())

fun exp (InFrame k) _ = MEM(BINOP(PLUS, TEMP(fp), CONST k))
  | exp (InReg l) _ = TEMP l

fun externalCall (s, l) = CALL(NAME s, l)

end
