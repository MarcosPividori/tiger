structure seman =
struct

local

open abs
open env
open error
open hashtable
open types

infixr 0 $;
fun x $ y = x y;

type Tenv = (string, Type) HashT
type Venv = (string, EnvEntry) HashT

val base_tenv : Tenv = htFromList [("int", TInt), ("string", TString)]

val base_venv : Venv = htFromList
  [("print", Func{level=(), label=(),
          formals=[TString], result=TUnit, extern=true}),
  ("flush", Func{level=(), label=(),
          formals=[], result=TUnit, extern=true}),
  ("getchar", Func{level=(), label=(),
          formals=[], result=TString, extern=true}),
  ("ord", Func{level=(), label=(),
          formals=[TString], result=TInt, extern=true}),
  ("chr", Func{level=(), label=(),
          formals=[TInt], result=TString, extern=true}),
  ("size", Func{level=(), label=(),
          formals=[TString], result=TInt, extern=true}),
  ("substring", Func{level=(), label=(),
          formals=[TString, TInt, TInt], result=TString, extern=true}),
  ("concat", Func{level=(), label=(),
          formals=[TString, TString], result=TString, extern=true}),
  ("not", Func{level=(), label=(),
          formals=[TInt], result=TInt, extern=true}),
  ("exit", Func{level=(), label=(),
          formals=[TInt], result=TUnit, extern=true})
  ]

fun typeEq (TRecord _) TNil = true
  | typeEq TNil (TRecord _) = true
  | typeEq (TRecord (_, u1)) (TRecord (_, u2)) = (u1=u2)
  | typeEq (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | typeEq a b = (a=b)

fun transExp(venv, tenv) = let
  fun trexp(VarExp v) = trvar(v)
    | trexp(UnitExp _) = {exp=(), ty=TUnit}
    | trexp(NilExp _) = {exp=(), ty=TNil}
    | trexp(IntExp(i, _)) = {exp=(), ty=TInt}
    | trexp(StringExp(s, _)) = {exp=(), ty=TString}
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
        {exp=(), ty=result}
      end
    | trexp(OpExp({left, oper, right}, ln)) =
      let
        fun getOpExp oper expl expr = case oper of
            EqOp     => ()
          | NeqOp    => ()
          | PlusOp   => ()
          | MinusOp  => ()
          | TimesOp  => ()
          | DivideOp => ()
          | LtOp     => ()
          | LeOp     => ()
          | GtOp     => ()
          | GeOp     => ()
        fun checkEqType tyl tyr = typeEq tyl tyr andalso
              not (tyl = TNil andalso tyr = TNil) andalso tyl <> TUnit
        fun checkArithType tyl tyr = typeEq tyl tyr andalso typeEq tyl TInt
        fun checkCmpType tyl tyr = typeEq tyl tyr andalso
              (typeEq tyl TInt orelse typeEq tyl TString)
        fun checkOpType oper = case oper of
            EqOp     => checkEqType
          | NeqOp    => checkEqType
          | PlusOp   => checkArithType
          | MinusOp  => checkArithType
          | TimesOp  => checkArithType
          | DivideOp => checkArithType
          | LtOp     => checkCmpType
          | LeOp     => checkCmpType
          | GtOp     => checkCmpType
          | GeOp     => checkCmpType
        val {exp=expl, ty=tyl} = trexp left
        val {exp=expr, ty=tyr} = trexp right
      in
        if not $ checkOpType oper tyl tyr
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

        val _ = verify cs tfields
      in
        {exp=(), ty=tyr}
      end
    | trexp(SeqExp(s, ln)) =
      let
        val lexti = map trexp s
        val exprs = map #exp lexti
        val {ty=typ, ...} = List.last lexti
      in
        {exp=(), ty=typ}
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
          then {exp=(), ty=TUnit}
          else raiseError ln "type mismatch in assignment."
      end
    | trexp(IfExp({test, then', else'=SOME else'}, ln)) =
      let
        val {exp=testexp, ty=tytest} = trexp test
        val {exp=thenexp, ty=tythen} = trexp then'
        val {exp=elseexp, ty=tyelse} = trexp else'
      in
        if typeEq tytest TInt andalso typeEq tythen tyelse
          then {exp=(), ty=tythen}
          else raiseError ln "type error on if expression."
      end
    | trexp(IfExp({test, then', else'=NONE}, ln)) =
      let
        val {exp=exptest, ty=tytest} = trexp test
        val {exp=expthen, ty=tythen} = trexp then'
      in
        if typeEq tytest TInt andalso tythen=TUnit
          then {exp=(), ty=TUnit}
          else raiseError ln "type error on if expression."
      end
    | trexp(WhileExp({test, body}, ln)) =
      let
        val {ty=tytest, exp=etest} = trexp test
        val {ty=tybody, exp=ebody} = trexp body
        val wExp = if not $ typeEq tytest TInt
          then raiseError ln "type error in while condition."
          else if not $ typeEq tybody TUnit
            then raiseError ln "type error in while's block (must be Unit)."
            else ()
      in
        {exp=wExp, ty=TUnit}
      end
    | trexp(ForExp({var, escape, lo, hi, body}, ln)) =
      let
        val {ty=tylo, exp=elo} = trexp lo
        val {ty=tyhi, exp=ehi} = trexp hi
        val _ = if not (typeEq tylo TInt andalso typeEq tyhi TInt)
                  then raiseError ln "for's bounds must be integers."
                  else ()
        val venv' = htRInsert venv var $ VIntro {access=(), level=()}
        val {ty=tbody, exp=ebody} = transExp (venv',tenv) body
        val _ = if tbody <> TUnit
                  then raiseError ln "type error in for's block (must be Unit)."
                  else ()
      in
        {exp=(), ty=TUnit}
      end
    | trexp(LetExp({decs, body}, _)) =
      let
        fun aux (d, (v, t, exps1)) =
          let
            val (vv, tt, exps2) = trdec (v, t) d
          in
            (vv, tt, exps1@exps2)
          end
        val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
        val {exp=expbody,ty=tybody} = transExp (venv', tenv') body
      in
        {exp=(), ty=tybody}
      end
    | trexp(BreakExp ln) = {exp=(), ty=TUnit}
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
                then {exp=(), ty=TArray (t,u)}
                else raiseError ln $ "type mismatch between array's type "
                  ^"and initial value."
        | SOME _ => raiseError ln "must be array type."
        | NONE => raiseError ln $ "unknown type ("^typ^")."
      end

  and trvar(SimpleVar s, ln) =
      let
        val (tvar,acc,lvl) = case htSearch venv s of
                SOME (Var {ty,access,level}) => (ty,access,level)
              | SOME (VIntro {access,level}) => (TInt,access,level)
              | _ => raiseError ln $ "undeclared variable \""^s^"\""
      in
        {exp=(), ty=tvar}
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
        {exp=(), ty=typ}
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
        {exp=(), ty=typ}
      end

  and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},ln)) =
      let
        val {ty=t', exp=e'} = transExp (venv,tenv) init
        val _ = case t' of
            TNil => raiseError ln $ "type error, type must be explicit when "^
                      "assigned a Nil expression."
          | TUnit => raiseError ln "type error, unit type can not be assigned."
          | _ => ()
      in
        (htRInsert venv name $ Var {ty=t',access=(),level=()}, tenv, [])
      end
    | trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},ln)) =
      let
        val {ty=t', exp=e'} = transExp (venv,tenv) init
        val varTyp = case htSearch tenv s of
              SOME t  => if typeEq t' t then t
                    else raiseError ln $ "type mismatch between var type "
                                         ^"and initial value."
            | NONE    => raiseError ln $ "unknown type ("^s^")."
      in
        (htRInsert venv name $ Var {ty=varTyp,access=(),level=()}, tenv, [])
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
        fun addFnToEnv (({name,params,result,body},ln),v) =
              let
                val typRes = case result of
                               NONE   => TUnit
                             | SOME t => check ln t
                val typArg = List.map (check ln o #typ) params
              in
                htRInsert v name $ Func {level=(), label=(), formals=typArg,
                                         result=typRes, extern= false}
              end
        val venv' = List.foldl addFnToEnv venv fnList
        fun processFn ({name,params,result,body},ln) =
              let
                val _ = List.foldl (fn ({name,...},lst) =>
                  if (List.exists (fn y => name = y) lst)
                    then raiseError ln $ "duplicated parameter "^name
                    else name::lst) [] params
                val typRes = case result of
                               NONE   => TUnit
                             | SOME t => check ln t
                val venv'' = List.foldl
                      (fn ({typ,name,escape},v) =>
                         htRInsert v name $ Var {ty=check ln typ
                                                ,access=()
                                                ,level=()}) venv' params
                val {ty, exp=eBody} = transExp(venv'', tenv) body
              in
                if typeEq ty typRes
                  then ()
                  else raiseError ln "Type error in return value."
              end
        val _ = List.app processFn fnList
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
    val _ = transExp (base_venv, base_tenv) main
  in
    ()
  end

end
end
