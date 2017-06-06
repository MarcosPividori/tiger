structure show =
struct

open abs
open tree
open PP

infixr 0 $
fun x $ y = x y

fun showTree s0 =
  let fun sayln s = s ^ "\n"
    fun indent 0 = ""
      | indent i = " " ^ indent (i-1)

    fun stm (SEQ (a, b)) d = indent d ^ sayln "SEQ(" ^ stm a (d+1) ^
          sayln "," ^ stm b (d+1) ^ ")"
      | stm (LABEL lab) d = indent d ^ "LABEL " ^ lab
      | stm (JUMP (e, _)) d =  indent d ^ "JUMP(" ^ exp e (d+1) ^ ")"
      | stm (CJUMP (r, a, b, t, f)) d = indent d ^ "CJUMP(" ^
          relop r ^ sayln "," ^ exp a (d+1) ^ sayln "," ^ exp b (d+1) ^
          sayln "," ^ indent (d+1) ^ t ^ "," ^ f ^ ")"
      | stm (MOVE (a, b)) d = indent d ^ sayln "MOVE(" ^ exp a (d+1) ^
          sayln "," ^ exp b (d+1) ^ ")"
      | stm (EXP e) d = indent d ^ sayln "EXP(" ^ exp e (d+1) ^ ")"

    and exp (BINOP (p, a, b)) d = indent d ^ "BINOP(" ^ binop(p) ^
          sayln "," ^ exp a (d+1) ^ sayln "," ^ exp b (d+1) ^ ")"
      | exp (MEM e) d = indent d ^ sayln "MEM(" ^ exp e (d+1) ^ ")"
      | exp (TEMP t) d = indent d ^ "TEMP " ^ t
      | exp (ESEQ (s,e)) d = indent d ^ sayln "ESEQ(" ^ stm s (d+1) ^
          sayln "," ^ exp e (d+1) ^ ")"
      | exp (NAME lab) d = indent d ^ "NAME " ^ lab
      | exp (CONST i) d = indent d ^ "CONST " ^ (Int.toString i)
      | exp (CALL (e,el)) d = indent d ^ sayln "CALL(" ^ (exp e (d+1)) ^
          concat (map (fn a => sayln "," ^ exp a (d+2)) el) ^ ")"

    and binop PLUS = "PLUS"
      | binop MINUS = "MINUS"
      | binop MUL = "MUL"
      | binop DIV = "DIV"
      | binop AND = "AND"
      | binop OR = "OR"
      | binop LSHIFT = "LSHIFT"
      | binop RSHIFT = "RSHIFT"
      | binop ARSHIFT = "ARSHIFT"
      | binop XOR = "XOR"

    and relop EQ = "EQ"
      | relop NE = "NE"
      | relop LT = "LT"
      | relop GT = "GT"
      | relop LE = "LE"
      | relop GE = "GE"
      | relop ULT = "ULT"
      | relop ULE = "ULE"
      | relop UGT = "UGT"
      | relop UGE = "UGE"

 in stm s0 0 ^ sayln "" end

local
  fun ppexpr pps e0 =
    let
      fun ppf {name, escape, typ} =
            (add_string pps $ "{name=" ^ name ^ ", ";
            add_break pps (0, 1);
            add_string pps "escape=";
            add_string pps $ Bool.toString $ !escape;
            add_string pps $ "typ=" ^ typ;
            add_string pps "}"; add_break pps (0, 1))
      and ppd (FunctionDec flist) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "FunctionDec([";
            List.app
              (fn ({name, params, result, body}, _) =>
                (add_string pps $ "{name=" ^ name ^ ", ";
                add_break pps (0, 0);
                add_string pps "params=[";
                List.app ppf params;
                add_string pps "], "; add_break pps (0, 1);
                add_string pps $ "result=" ^
                  (case result of SOME s => s | _ => "NONE");
                add_break pps (0, 0);
                add_string pps "body="; ppe body;
                add_string pps "}"; add_break pps (0, 0)))
              flist;
            add_string pps "])"; add_break pps (0, 0);
            end_block pps)
        | ppd (VarDec ({name, escape, typ, init}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps $ "VarDec {name=" ^ name ^ ", ";
            add_break pps (0, 1);
            add_string pps "escape=";
            add_string pps $ Bool.toString $ !escape;
            add_string pps ", "; add_break pps (0, 1);
            add_string pps $ "typ=" ^
              (case typ of SOME s => s | _ => "NONE");
            add_string pps ", "; add_break pps (0, 1);
            add_string pps "init="; ppe init;
            add_string pps "}"; add_break pps (0, 0);
            end_block pps)
        | ppd (TypeDec tlist) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "TypeDec([";
            List.app
              (fn ({name: symbol, ty: ty}, _) =>
                (add_string pps $ "{name=" ^ name ^ ", "; add_break pps (0, 1);
                add_string pps "ty=";
                ppt ty; add_string pps "}";
                add_break pps (0, 1)))
              tlist;
            add_string pps "])"; add_break pps (0, 0);
            end_block pps)
      and ppt (NameTy s) =
            (begin_block pps INCONSISTENT 0;
            add_string pps $ "NameTy(" ^ s ^ ")"; add_break pps (0, 0);
            end_block pps)
        | ppt (RecordTy fieldlist) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "RecordTy([";
            List.app ppf fieldlist;
            add_string pps "])";
            end_block pps)
        | ppt (ArrayTy s) =
            (begin_block pps INCONSISTENT 0;
            add_string pps $ "ArrayTy(" ^ s ^ ")"; add_break pps (0, 1);
            end_block pps)
      and ppv (SimpleVar s) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "SimpleVar("; add_break pps (0, 1);
            add_string pps $ s ^ ")"; add_break pps (0, 0);
            end_block pps)
        | ppv (FieldVar (v, s)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "FieldVar(";
            ppv v; add_break pps (0, 1);
            add_string pps $ ", " ^ s ^ ")";
            end_block pps)
        | ppv (SubscriptVar (v, e)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "SubscriptVar("; add_break pps (0, 1);
            ppv v; add_string pps ", "; add_break pps (0, 1);
            ppe e; add_string pps ")"; add_break pps (0, 0);
            end_block pps)
      and ppe (UnitExp _) = add_string pps "UnitExp"
        | ppe (NilExp _) = add_string pps "NilExp"
        | ppe (IntExp (n, _)) = add_string pps $ Int.toString n
        | ppe (StringExp (s, _)) = add_string pps $ "\"" ^ s ^ "\""
        | ppe (BreakExp _) = add_string pps "BreakExp"
        | ppe (VarExp (v, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "VarExp("; add_break pps (0, 1);
            ppv v; add_break pps (0, 0);
            add_string pps ")"; add_break pps (0, 0);
            end_block pps)
        | ppe (OpExp ({left, oper, right}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "OpExp {"; add_break pps (0, 0);
                add_string pps "left=";
                ppe left; add_string pps ", "; add_break pps (0, 1);
            add_string pps "oper=";
            add_string pps
              (case oper of
              PlusOp => "PlusOp" | MinusOp => "MinusOp"
              | TimesOp => "TimesOp" | DivideOp => "DivideOp"
              | EqOp => "EqOp" | NeqOp => "NeqOp"
              | LtOp => "LtOp" | LeOp => "LeOp"
              | GtOp => "GtOp" | GeOp => "GeOp");
            add_string pps ", "; add_break pps (0, 1);
            add_string pps "right="; ppe right; add_string pps "}";
            end_block pps)
        | ppe (WhileExp ({test, body}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "WhileExp {test=";
            ppe test; add_string pps ", "; add_break pps (0, 0);
            add_string pps "body=";
            add_string pps "body="; ppe body; add_string pps "}";
            end_block pps)
        | ppe (ForExp ({var, escape, lo, hi, body}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "ForExp {var=";
            add_string pps var; add_string pps ", "; add_break pps (0, 0);
            add_string pps "escape=";
            add_string pps $ Bool.toString $ !escape;
            add_string pps ", "; add_break pps (0, 0);
            add_string pps "lo="; ppe lo; add_string pps ", ";
            add_break pps (0, 0);
            add_string pps "hi="; ppe hi; add_string pps ", ";
            add_break pps (0, 0);
            add_string pps "body="; ppe body; add_string pps "}";
            add_break pps (0, 0);
            end_block pps)
        | ppe (AssignExp ({var, exp}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "AssignExp {"; add_break pps (0, 1);
            add_string pps "var=";
            ppv var; add_string pps ", "; add_break pps (0, 1);
            add_string pps "exp="; ppe exp; add_string pps "}";
            end_block pps)
        | ppe (IfExp ({test, then', else'}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "IfExp {"; add_break pps (0, 0);
            add_string pps "test=";
            ppe test; add_string pps ", "; add_break pps (0, 0);
            add_string pps "then'=";
            ppe then'; add_string pps ", "; add_break pps (0, 0);
            add_string pps "else'=";
            case else' of SOME e => ppe e | NONE => add_string pps "NONE";
            add_break pps (0, 0);
            add_string pps "}";
            end_block pps)
        | ppe (CallExp ({func, args}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "CallExp {"; add_break pps (0, 0);
            add_string pps $ "func=" ^ func ^ ", "; add_break pps (0, 0);
            add_string pps "args=[";
            List.app (fn e => (ppe e; add_break pps (0, 0))) args;
            add_string pps "]}";
            add_break pps (0, 0);
            end_block pps)
        | ppe (SeqExp (explist, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "SeqExp([";
            List.app (fn e => (ppe e; add_break pps (0, 0))) explist;
            add_string pps "])"; add_break pps (0, 0);
            end_block pps)
        | ppe (RecordExp ({fields, typ}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "RecordExp {"; add_break pps (0, 1);
            add_string pps "fields=["; add_break pps (0, 1);
            List.app
              (fn (s, e) => (add_string pps $ "(" ^ s ^ ", ";
                ppe e; add_string pps ")"; add_break pps (0, 0)))
              fields;
            add_string pps "], "; add_break pps (0, 1);
            add_string pps "typ=";
            add_string pps typ;
            add_string pps "}";
            end_block pps)
        | ppe (ArrayExp ({typ, size, init}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "ArrayExp {test=";
            add_string pps $ typ ^ ", "; add_break pps (0, 1);
            add_string pps "size=";
            ppe size; add_string pps ", "; add_break pps (0, 1);
            add_string pps "init=";
            ppe init; add_string pps "}";
            end_block pps)
        | ppe (LetExp ({decs, body}, _)) =
            (begin_block pps INCONSISTENT 0;
            add_string pps "LetExp {decs=[";
            List.app ppd decs;
            add_string pps "], "; add_break pps (0, 1);
            add_string pps "body=";
            ppe body; add_string pps "}";
            end_block pps)
    in
      begin_block pps INCONSISTENT 0;
      ppe e0;
      end_block pps
    end

  val ppstrm =
    PP.mk_ppstream {
      consumer = print,
      linewidth = 79,
      flush = fn () => TextIO.flushOut TextIO.stdOut
    }
in
  fun printAst e =
    (ppexpr ppstrm e;
    flush_ppstream ppstrm;
    print "\n")
end

end
