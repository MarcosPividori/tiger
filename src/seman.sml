structure seman =
struct

local

open abs
open env
open error
open hashtable
open temp
open trans
open types

infixr 0 $;
fun x $ y = x y;

type Tenv = (string, Type) HashT
type Venv = (string, EnvEntry) HashT

val base_tenv : Tenv = htFromList [("int", TInt), ("string", TString)]

val base_venv : Venv = htFromList
  [("print", Func{level=outermost, label="print",
          formals=[TString], result=TUnit, extern=true}),
  ("flush", Func{level=outermost, label="flush",
          formals=[], result=TUnit, extern=true}),
  ("getchar", Func{level=outermost, label="getstr",
          formals=[], result=TString, extern=true}),
  ("ord", Func{level=outermost, label="ord",
          formals=[TString], result=TInt, extern=true}),
  ("chr", Func{level=outermost, label="chr",
          formals=[TInt], result=TString, extern=true}),
  ("size", Func{level=outermost, label="size",
          formals=[TString], result=TInt, extern=true}),
  ("substring", Func{level=outermost, label="substring",
          formals=[TString, TInt, TInt], result=TString, extern=true}),
  ("concat", Func{level=outermost, label="concat",
          formals=[TString, TString], result=TString, extern=true}),
  ("not", Func{level=outermost, label="not",
          formals=[TInt], result=TInt, extern=true}),
  ("exit", Func{level=outermost, label="exit",
          formals=[TInt], result=TUnit, extern=true})
  ]

fun typeEq (TRecord _) TNil = true
  | typeEq TNil (TRecord _) = true
  | typeEq (TRecord (_, u1)) (TRecord (_, u2)) = (u1=u2)
  | typeEq (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | typeEq a b = (a=b)

fun transExp topLevel loopLevel venv tenv = let
  fun trexp(VarExp v) = trvar(v)
    | trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
    | trexp(NilExp _) = {exp=nilExp(), ty=TNil}
    | trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt}
    | trexp(StringExp(s, _)) = {exp=stringExp s, ty=TString}
    | trexp(CallExp({func, args}, ln)) =
      let
        val {formals, result, level, label, extern} =
          case htSearch venv func of
            SOME (Func r) => r
          | _   => raiseError ln $ "undeclared function \""^func^"\""
        val targs = List.map trexp args
        fun comp ([],[]) = ()
          | comp ([],_)  = raiseError ln $
              "not enough arguments for function \""^func^"\""
          | comp (_,[])  = raiseError ln $
              "too many arguments for function \""^func^"\""
          | comp (({ty,...}::xs),(ty2::ys)) = if typeEq ty ty2
              then comp (xs, ys)
              else raiseError ln "type error in function's params."
        val _ = comp (targs, formals)
      in
        {exp=callExp topLevel label extern (typeEq result TUnit) level
                     (List.map #exp targs), ty=result}
      end
    | trexp(OpExp({left, oper, right}, ln)) =
      let
        val {exp=expl, ty=tyl} = trexp left
        val {exp=expr, ty=tyr} = trexp right

        val getEqExp = if typeEq tyl TString
             then binOpStrExp
             else binOpIntRelExp
        val getArithExp = binOpIntExp
        val getCmpExp = if typeEq tyl TString
             then binOpStrExp
             else binOpIntRelExp

        fun checkEqType tyl tyr = typeEq tyl tyr andalso
              not (tyl = TNil andalso tyr = TNil) andalso tyl <> TUnit
        fun checkArithType tyl tyr = typeEq tyl tyr andalso typeEq tyl TInt
        fun checkCmpType tyl tyr = typeEq tyl tyr andalso
              (typeEq tyl TInt orelse typeEq tyl TString)

        val (checkOpType, getOpExp)  = case oper of
            EqOp     => (checkEqType, getEqExp)
          | NeqOp    => (checkEqType, getEqExp)
          | PlusOp   => (checkArithType, getArithExp)
          | MinusOp  => (checkArithType, getArithExp)
          | TimesOp  => (checkArithType, getArithExp)
          | DivideOp => (checkArithType, getArithExp)
          | LtOp     => (checkCmpType, getCmpExp)
          | LeOp     => (checkCmpType, getCmpExp)
          | GtOp     => (checkCmpType, getCmpExp)
          | GeOp     => (checkCmpType, getCmpExp)
      in
        if not $ checkOpType tyl tyr
         then raiseError ln "types are not appropriate for operator."
         else {exp=getOpExp oper expl expr, ty=TInt}
      end
    | trexp(RecordExp({fields, typ}, ln)) =
      let
        val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

        val (tyr, cs) = case htSearch tenv typ of
            SOME (TRecord (cs, u)) => (TRecord (cs, u), cs)
          | SOME _ => raiseError ln $ typ^" is not a record type."
          | NONE   => raiseError ln $ "unknown type ("^typ^")."

        fun find xs _ [] = NONE
          | find xs e (x::rest) = if e = #1 x
                                    then SOME (x, xs @ rest)
                                    else find (x::xs) e rest

        fun verify [] [] = []
          | verify (c::cs) [] = raiseError ln "not enough fields."
          | verify [] (c::cs) = raiseError ln "too many fields."
          | verify cs ((sy,{exp,ty})::ds) =
              case find [] sy cs of
                SOME ((_,ref (SOME tt), y), cs') => if typeEq ty tt
                                  then (exp, y)::(verify cs' ds)
                                  else raiseError ln $ "type error on field "^sy
              | _  => raiseError ln "unknown or duplicated field."

        val lfields = verify cs tfields
      in
        {exp=recordExp lfields, ty=tyr}
      end
    | trexp(SeqExp(s, ln)) =
      let
        val lexti = map trexp s
        val exprs = map #exp lexti
        val {ty=typ, ...} = List.last lexti
      in
        {exp=seqExp exprs, ty=typ}
      end
    | trexp(AssignExp({var=v, exp}, ln)) =
      let
        val {ty=tvar, exp=eVar} = trvar (v,ln)
        val {ty=texp, exp=eValue} = trexp exp
        val _ = case v of
            SimpleVar s => (case htSearch venv s of
                SOME (VIntro _) =>
                  raiseError ln "for's indexes are read only."
              | _  => ())
          | _           => ()
      in
        if typeEq tvar texp
          then {exp=assignExp eVar eValue, ty=TUnit}
          else raiseError ln "type mismatch in assignment."
      end
    | trexp(IfExp({test, then', else'=SOME else'}, ln)) =
      let
        val {exp=exptest, ty=tytest} = trexp test
        val {exp=expthen, ty=tythen} = trexp then'
        val {exp=expelse, ty=tyelse} = trexp else'
        fun trexpIf TUnit = ifThenElseExpUnit
          | trexpIf _ = ifThenElseExp
        val ifExp = trexpIf tythen {test=exptest, then'=expthen, else'=expelse}
      in
        if typeEq tytest TInt andalso typeEq tythen tyelse
          then {exp=ifExp, ty=tythen}
          else raiseError ln "type error on if expression."
      end
    | trexp(IfExp({test, then', else'=NONE}, ln)) =
      let
        val {exp=exptest, ty=tytest} = trexp test
        val {exp=expthen, ty=tythen} = trexp then'
      in
        if typeEq tytest TInt andalso tythen=TUnit
          then {exp=ifThenExp {test=exptest, then'=expthen}, ty=TUnit}
          else raiseError ln "type error on if expression."
      end
    | trexp(WhileExp({test, body}, ln)) =
      let
        val {ty=tytest, exp=etest} = trexp test
        val loopLev = newLoopLevel()
        val {ty=tybody, exp=ebody} = transExp topLevel loopLev venv tenv body
        val expWhile = if not $ typeEq tytest TInt
          then raiseError ln "type error in while condition."
          else if not $ typeEq tybody TUnit
            then raiseError ln "type error in while's block (must be Unit)."
            else whileExp {test=etest, body=ebody, lev=topLevel, looplvl=loopLev}
      in
        {exp=expWhile, ty=TUnit}
      end
    | trexp(ForExp({var, escape, lo, hi, body}, ln)) =
      let
        val {ty=tylo, exp=elo} = trexp lo
        val {ty=tyhi, exp=ehi} = trexp hi
        val _ = if not (typeEq tylo TInt andalso typeEq tyhi TInt)
                  then raiseError ln "for's bounds must be integers."
                  else ()
        val acc = allocLocal topLevel (!escape)
        val venv' = htRInsert venv var $
            VIntro {access=acc, depth=getDepth topLevel}
        val expVar = simpleVar (getDepth topLevel) acc (getDepth topLevel)
        val loopLev = newLoopLevel()
        val {ty=tbody, exp=ebody} = transExp topLevel loopLev venv' tenv body
        val _ = if tbody <> TUnit
                  then raiseError ln "type error in for's block (must be Unit)."
                  else ()
        val expFor = forExp {var=expVar,lo=elo,hi=ehi,body=ebody,looplvl=loopLev}
      in
        {exp=expFor, ty=TUnit}
      end
    | trexp(LetExp({decs, body}, _)) =
      let
        fun aux (d, (v, t, exps1)) =
          let val (vv, tt, exps2) = trdec (v, t) d
          in (vv, tt, exps1@exps2) end
        val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
        val {exp=expbody, ty=tybody} =
            transExp topLevel loopLevel venv' tenv' body
      in
        {exp=seqExp (expdecs@[expbody]), ty=tybody}
      end
    | trexp(BreakExp ln) = ({exp=breakExp loopLevel, ty=TUnit}
        handle breakexc => raiseError ln "Inappropriate break statement.")
    | trexp(ArrayExp({typ, size, init}, ln)) =
      let
        val {ty=typel, exp=expInit} = trexp init
        val {ty=typs, exp=expSize} = trexp size
        val _ = if not $ typeEq typs TInt
                  then raiseError ln "type error, array's size must be integer."
                  else ()
      in
        case htSearch tenv typ of
          SOME (TArray (t, u)) => if typeEq typel t
                then {exp=arrayExp{size=expSize, init=expInit}, ty=TArray (t,u)}
                else raiseError ln $ "type mismatch between array's type "
                  ^"and initial value."
        | SOME _ => raiseError ln "must be array type."
        | NONE => raiseError ln $ "unknown type ("^typ^")."
      end

  and trvar(SimpleVar s, ln) =
      let
        val (tvar, acc, lvl) = case htSearch venv s of
                SOME (Var {ty, access, depth}) => (ty, access, depth)
              | SOME (VIntro {access, depth}) => (TInt, access, depth)
              | _ => raiseError ln $ "undeclared variable \""^s^"\""
      in
        {exp=simpleVar (getDepth topLevel) acc lvl, ty=tvar}
      end
    | trvar(FieldVar(v, simb), ln) =
      let
        val {ty=tvar, exp=expVar} = trvar (v,ln)
        val lst = case tvar of
                    TRecord (cs, u) => List.filter (fn s => #1 s = simb) cs
                  | _ => raiseError ln "record type expected."
        val (typ, pos) = case lst of
                           [] => raiseError ln $ "unknown field \""^simb^"\""
                         | ((_,ref (SOME t),s)::_) => (t,s)
                         | _ => raiseError ln "shouldn't happen."
      in
        {exp=fieldVar expVar pos, ty=typ}
      end
    | trvar(SubscriptVar(v, e), ln) =
      let
        val {ty=t1, exp=eIndex} = trexp e
        val _ = if not $ typeEq t1 TInt
                 then raiseError ln "type error, Int expected for array index."
                 else ()
        val {ty=t2, exp=eArray} = trvar (v,ln)
        val typ = case t2 of
                    TArray (t,_) => t
                  | _ => raiseError ln "type error, array type expected."
      in
        {exp=subscriptVar eArray eIndex, ty=typ}
      end

  and trdec (venv,tenv) (VarDec ({name,escape,typ,init},ln)) =
      let
        val {ty=t', exp=e'} = transExp topLevel loopLevel venv tenv init
        val varTyp = case typ of
            SOME s => (case htSearch tenv s of
                         NONE    => raiseError ln $ "unknown type ("^s^")."
                       | SOME t  => if typeEq t' t then t
                               else raiseError ln $ "type mismatch between var"^
                                                    " type and initial value.")
          | NONE => (case t' of
                       TNil => raiseError ln $ "type must be explicit when "^
                                              "assigned a Nil expression."
                     | TUnit => raiseError ln "unit type can not be assigned."
                     | _ => t')
        val acc = allocLocal topLevel (!escape)
        val e'' = assignExp (varDec acc (getDepth topLevel)) e'
      in
        (htRInsert venv name $ Var {ty=varTyp, access=acc,
                                    depth=getDepth topLevel}, tenv, [e''])
      end
    | trdec (venv,tenv) (FunctionDec fnList) =
      let
        val _ = List.foldl (fn (({name,...},ln),decs) =>
          case htSearch decs name of
            SOME _ => raiseError ln $ "duplicated function definition for "^name
          | NONE => htInsert decs name ()) (htNew ()) fnList
        fun check ln s = case htSearch tenv s of
                           NONE => raiseError ln $ "unknown type ("^s^")."
                         | SOME t => t
        fun addFnToEnv (({name,params,result,body},ln),(v,ls)) =
              let
                val typRes = case result of
                               NONE   => TUnit
                             | SOME t => check ln t
                val typArg = List.map (check ln o #typ) params
                val lab = newLabel()
                val lev = newLevel {parent=topLevel
                      , name=if name = "_tigermain" then "_tigermain" else lab
                      , formals=List.map (fn x => !(#escape x)) params}
              in
                (htRInsert v name $ Func {level=lev, label=lab, formals=typArg,
                                          result=typRes, extern=false}, lev::ls)
              end
        val (venv', levels) = List.foldl addFnToEnv (venv,[]) fnList
        fun processFn (({name,params,result,body},ln),lev) =
              let
                val _ = List.foldl (fn ({name,...},lst) =>
                  if (List.exists (fn y => name = y) lst)
                    then raiseError ln $ "duplicated parameter "^name
                    else name::lst) [] params
                val typRes = case result of
                               NONE   => TUnit
                             | SOME t => check ln t
                val venv'' = List.foldl
                      (fn (({typ,name,escape},acc),v) =>
                         htRInsert v name $ Var {ty=check ln typ,
                                                 access=acc,
                                                 depth=getDepth topLevel})
                      venv' $ ListPair.zip (params, formals lev)
                val {ty, exp=eBody} = transExp lev nilLoopLevel venv'' tenv body
              in
                if typeEq ty typRes
                  then functionDec eBody lev (typRes=TUnit)
                  else raiseError ln "Type error in return value."
              end
        val _ = List.map processFn $ ListPair.zip (fnList,rev levels)
      in
        (venv', tenv, [])
      end
    | trdec (venv,tenv) (TypeDec ts) =
      let
        val mapTy = List.foldl (fn (({name,ty},ln),t) => case htSearch t name of
              SOME _ => raiseError ln $
                "Duplicated type definition for \""^name^"\""
            | NONE   => htRInsert t name ty) (htNew ()) ts

        fun depends [] = []
          | depends ({name,ty=NameTy sym}::xs) = (sym, name)::(depends xs)
          | depends ({name,ty=ArrayTy sym}::xs) = (sym, name)::(depends xs)
          | depends (_::xs) = depends xs

        val ord = topsort.topsort $ depends $ map #1 ts

        val ord' = ord @ (List.filter
                (fn x => not (List.exists (fn y => x = y) ord)) (htKeys mapTy))

        fun addType name typ (tenv',refs) = case typ of
            NameTy sym => let val t = htGet tenv' sym
                           in (htRInsert tenv' name t,refs) end
          | ArrayTy sym => let val t = htGet tenv' sym
                          in (htRInsert tenv' name $ TArray (t, ref()),refs) end
          | RecordTy flds => let
                val _ = List.app (fn ({typ,...}) =>
                  if htSearch mapTy typ = NONE andalso htSearch tenv typ = NONE
                    then raise Error (NONE, "Unknown type \""^typ^"\".")
                    else ()) flds
                val (lst, refs') = List.foldr
                  (fn ({name,typ,...},(l,r)) =>
                      case htSearch r typ of
                        NONE => let val rr = ref (htSearch tenv' typ)
                                 in ((name, rr)::l, htInsert r typ rr) end
                      | SOME rr => ((name, rr)::l, r))
                  ([],refs) flds
                val lst' = Listsort.sort
                             (fn ((a,_),(b,_)) => String.compare(a,b)) lst
                val _ = List.foldr (fn ((n1,_),n2) => if n1 = n2
                  then raise Error (NONE, "Duplicated field \""^n1^"\".")
                  else n1) "" lst'
                val (lst'',_) = List.foldl
                              (fn ((a,b),(l,n)) => ((a,b,n)::l,n+1)) ([],0) lst'
                val recordType = TRecord (lst'',ref ())
              in (htRInsert tenv' name recordType,refs') end

        val (tenv',refs) = List.foldl
                (fn (n,(tenv',refs)) => case htSearch mapTy n of
                      SOME t => addType n t (tenv',refs)
                    | NONE => (case htSearch tenv n of
                         NONE => raise Error (NONE, "Unknown type \""^n^"\".")
                       | SOME _ => (tenv',refs)))
                (tenv,htNew ()) ord'
        val _ = List.app (fn k => (htGet refs k) := htSearch tenv' k)
                         (htKeys refs)
      in
        (venv, tenv', [])
      end
  in
    trexp
  end

in

fun transProg ex =
  let
    val main = LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
                                           result=SOME "int", body=ex}, 0)]],
                       body=UnitExp 0}, 0)
    val _ = transExp outermost nilLoopLevel base_venv base_tenv main
  in
    ()
  end

end
end
