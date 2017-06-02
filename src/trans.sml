structure trans :> trans = struct

open frame
open tree
open temp
open abs

infixr 0 $;
fun x $ y = x y;

exception breakexc

type access = frame.access


(* Levels *)
type level = {parent: frame option, frame: frame, depth: int}

fun getDepth (l:level) = #depth l

val initialDepth = ~1

val outermost: level = {
  parent = NONE,
  frame = newFrame {name = "_main", formals = []},
  depth = initialDepth}

fun newLevel {parent = {frame, depth, ...} : level, name, formals} = {
  parent = SOME frame,
  frame = newFrame {name = name, formals = true :: formals},
          (* append static link (always escaped) *)
  depth = depth + 1}

(* Export some functions from frame module *)
fun allocLocal ({frame, ...}:level) b = frame.allocLocal frame b

fun formals ({frame, ...}:level) = tl (frame.formals frame)
                                   (* remove static link *)


(* Main data type for translation into intermediate representation *)
datatype exp =
    Ex of tree.exp
  | Nx of tree.stm
  | Cx of label * label -> tree.stm

fun seq [] = EXP (CONST 0)
  | seq [s] = s
  | seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
  | unEx (Nx s) = ESEQ (s, CONST 0)
  | unEx (Cx cf) =
    let
      val r = newTemp()
      val t = newLabel()
      val f = newLabel()
    in
      ESEQ (seq [MOVE (TEMP r, CONST 1),
                cf (t, f),
                LABEL f,
                MOVE (TEMP r, CONST 0),
                LABEL t],
           TEMP r)
    end

fun unNx (Ex e) = EXP e
  | unNx (Nx s) = s
  | unNx (Cx cf) =
    let
      val t = newLabel()
      val f = newLabel()
    in
      seq [cf(t, f),
           LABEL t,
           LABEL f]
    end

fun unCx (Nx s) = raise Fail "Error (UnCx(Nx..))"
  | unCx (Cx cf) = cf
  | unCx (Ex (CONST 0)) = (fn (t, f) => JUMP (NAME f, [f]))
  | unCx (Ex (CONST _)) = (fn (t, f) => JUMP (NAME t, [t]))
  | unCx (Ex e) = (fn (t, f) => CJUMP (NE, e, CONST 0, t, f))


(* globData *)
type frag = frame.frag
val globData = ref ([]: frag list)

fun procEntryExit ({frame, ...}: level) body =
    let val label = STRING (name frame, "")
      val body' = PROC {frame = frame, body = unNx body}
      val final = STRING (";;-------", "")
    in globData:= (!globData@[label, body', final]) end

fun getResult() = !globData


(* Translate main expressions *)
fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun stringLen s =
    let fun aux[] = 0
          | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
          | aux(_::t) = 1+aux(t)
    in aux(explode s) end

fun stringExp (s:string) =
    let val l = newLabel()
      val len = ".quad "^makestring(stringLen s)
      val str = ".ascii \""^s^"\""
      val _ = globData:= (!globData @ [STRING (l, len), STRING ("", str)])
    in Ex (NAME l) end

fun callExp ({depth=actDepth, ...}:level) name external isproc
            ({depth, ...}:level) ls =
    let
      fun getSL 0 = TEMP FP
        | getSL n = MEM $ BINOP (PLUS, CONST FpPrevLev, getSL (n-1))
      val fpLev = if depth = actDepth
              then MEM $ BINOP (PLUS, CONST FpPrevLev, TEMP FP)
              else if depth < actDepth
               then getSL (actDepth - depth + 1)
               else TEMP FP
      fun prepArgs []     (rt, re) = (rt, re)
        | prepArgs (h::t) (rt, re) = case h of
              Ex (CONST s) => prepArgs t ((CONST s)::rt, re)
            | Ex (NAME n)  => prepArgs t ((NAME n )::rt, re)
            | Ex (TEMP te) => prepArgs t ((TEMP te)::rt, re)
            | _        => let val temp = TEMP (newTemp())
          in prepArgs t (temp::rt, MOVE (temp, unEx h)::re) end
      val (ta, la') = prepArgs ls ([], [])
      val ta' = if external
            then rev ta
            else fpLev::(rev ta)
    in
      if isproc
        then Nx (seq (la' @ [EXP $ CALL (NAME name, ta')]))
        else let val temp = TEMP (newTemp())
             in Ex $ ESEQ (seq $ la'@ [ EXP $ CALL (NAME name, ta'),
                                        MOVE (temp, TEMP RV)], temp)
             end
    end

fun binOpIntExp oper left right =
  Ex (BINOP (
    case oper of
      PlusOp   => PLUS
    | MinusOp  => MINUS
    | TimesOp  => MUL
    | DivideOp => DIV
    | _ => raise Fail "Internal error, call binOpIntExp with wrong op."
    , unEx left, unEx right))

fun binOpIntRelExp oper left right =
  Cx (fn (l1, l2) => CJUMP (case oper of
           EqOp  => EQ
         | NeqOp => NE
         | LtOp  => LT
         | LeOp  => LE
         | GtOp  => GT
         | GeOp  => GE
         | _ => raise Fail "Internal error, call binOpIntRelExp with wrong op."
        , unEx left, unEx right, l1, l2))

fun binOpStrExp oper left right =
  Cx (fn (l1, l2) => CJUMP (case oper of
          EqOp  => EQ
        | NeqOp => NE
        | LtOp  => LT
        | LeOp  => LE
        | GtOp  => GT
        | GeOp  => GE
        | _ => raise Fail "Internal error, call binOpStrExp with wrong op."
      , externalCall ("_stringCompare", [unEx left, unEx right])
      , CONST 0
      , l1
      , l2))

fun recordExp l =
    let
      val ret = newTemp()
      fun gentemp 0 = []
        | gentemp n = newTemp()::gentemp(n-1)
      val regs = gentemp (length l)
      fun aux((e, s), t) = (MOVE (TEMP t, unEx e), TEMP t, s)
      val lexps = map aux (ListPair.zip(l, regs))
      val lexps1 = map #1 lexps
      val l' = Listsort.sort(fn ((_,_,m),(_,_,n)) => Int.compare(m, n)) lexps
      val lexps2 = map #2 l'
    in
      Ex $ ESEQ (seq $ lexps1 @ [ EXP $ externalCall("_allocRecord",
                                                     CONST (length l)::lexps2)
                                , MOVE (TEMP ret, TEMP RV)],
                 TEMP ret)
    end

fun seqExp ([]:exp list) = Nx (EXP (CONST 0))
  | seqExp (exps:exp list) =
    let
      fun dropLast [x] = []
        | dropLast (x::xs) = x::(dropLast xs)
        | dropLast [] = []
    in
      case List.last exps of
        Nx s => Nx $ seq $ map unNx exps
      | Ex e => Ex $ ESEQ (seq $ map unNx $ dropLast exps, e)
      | cond => Ex $ ESEQ (seq $ map unNx $ dropLast exps, unEx cond)
    end

fun assignExp var exp =
    let
      val v = unEx var
      val vl = unEx exp
    in
      Nx (MOVE (v, vl))
    end

fun ifThenExp {test, then'} =
    let
      val s = unCx test
      val (ltrue, out) = (newLabel(), newLabel())
    in
      Nx $ seq [s(ltrue, out),
                LABEL ltrue,
                unNx then',
                LABEL out]
    end

fun ifThenElseExp {test, then', else'} =
    let
      val s = unCx test
      val (ltrue, lfalse, out) = (newLabel(), newLabel(), newLabel())
      val ret = newTemp()
    in
      Ex $ ESEQ (seq [s(ltrue, lfalse),
                      LABEL ltrue,
                      MOVE (TEMP ret , unEx then'),
                      JUMP (NAME out, [out]),
                      LABEL lfalse,
                      MOVE (TEMP ret , unEx else'),
                      LABEL out],
                 TEMP ret)
    end

fun ifThenElseExpUnit {test, then', else'} =
    let
      val s = unCx test
      val (ltrue, lfalse, out) = (newLabel(), newLabel(), newLabel())
    in
      Nx $ seq [s(ltrue, lfalse),
                LABEL ltrue,
                unNx then',
                JUMP (NAME out, [out]),
                LABEL lfalse,
                unNx else',
                LABEL out]
    end

(* While and for loops need the last label to jump in break statements *)
type looplvl = label option

val nilLoopLevel = NONE
fun newLoopLevel() = SOME $ newLabel()

fun whileExp {test: exp, body: exp, lev: level, looplvl: looplvl} =
    let
      val cf = unCx test
      val expb = unNx body
      val (test, loop, done) = (newLabel(), newLabel(), valOf looplvl)
    in
      Nx $ seq [LABEL test,
                cf(loop, done),
                LABEL loop,
                expb,
                JUMP (NAME test, [test]),
                LABEL done]
    end

fun forExp {lo, hi, var, body, looplvl} =
    let
      val (loop, loop1, done) = (newLabel(), newLabel(), valOf looplvl)
      val lo' = unEx lo
      val hi' = unEx hi
      val var' = unEx var
      val body' = unNx body
      val fresh = newTemp()
    in
      Nx $ seq [MOVE (TEMP fresh, hi'),
                MOVE (var', lo'),
                CJUMP (LE, var', TEMP fresh, loop, done),
                LABEL loop,
                body',
                CJUMP (GE, var', TEMP fresh, done, loop1),
                LABEL loop1,
                MOVE (var', BINOP (PLUS, var', CONST 1)),
                JUMP (NAME loop, [loop]),
                LABEL done]
    end

fun breakExp (SOME label) = Nx $ JUMP (NAME label, [label])
  | breakExp NONE = raise breakexc

fun arrayExp {size, init} =
    let
      val s = unEx size
      val i = unEx init
    in
      Ex $ externalCall("_allocArray", [s, i])
    end


(* Translate access to variables *)
fun simpleVar actDepth (InFrame offset) depth =
    let
      fun followSL 0 = TEMP FP
        | followSL n = MEM (BINOP (PLUS, CONST FpPrevLev, followSL (n-1)))
    in
      Ex $ MEM $ BINOP (PLUS, CONST offset, followSL (actDepth-depth))
    end
  | simpleVar _ (InReg temp) _ = Ex $ TEMP temp

fun fieldVar var p =
    let
      val record = unEx var
      val pos = CONST p
    in
      Ex $ ESEQ (EXP $ externalCall("_checkNil", [record]),
                 MEM $ BINOP (PLUS, record, BINOP (MUL, pos, CONST WSize)))
    end

fun subscriptVar arr ind =
    let
      val a = unEx arr
      val i = unEx ind
      val ra = newTemp()
      val ri = newTemp()
    in
      Ex $ ESEQ (seq [MOVE (TEMP ra, a),
                      MOVE (TEMP ri, i),
                      EXP $ externalCall("_checkIndex", [TEMP ra, TEMP ri])],
                 MEM $ BINOP (PLUS, TEMP ra,
                              BINOP (MUL, TEMP ri, CONST WSize)))
    end


(* Translate declarations *)
fun varDec acc actDepth = simpleVar actDepth acc actDepth

fun functionDec e lev proc =
    let val body =
         if proc then unNx e
         else MOVE (TEMP RV, unEx e)
      val () = procEntryExit lev $ Nx body
    in Ex (CONST 0) end

end
