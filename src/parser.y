%{
open abs

fun pos() = !numLine.numLine

fun simpleVarId (SimpleVar s) = s
  | simpleVarId _ = raise Fail "Should be SimpleVar!"

fun joinDecs(TypeDec [x], (TypeDec xs)::rest) = TypeDec(x::xs)::rest
  | joinDecs(FunctionDec [x], (FunctionDec xs)::rest) = FunctionDec(x::xs)::rest
  | joinDecs(fst, rest) = fst::rest
%}

%token EOF
%token TYPE ARRAY OF VAR FUNCTION
%token LET IN END IF THEN ELSE WHILE DO FOR TO BREAK
%token PTO DOSP DOSPIG COMA PCOMA IGUAL PI PD CI CD LI LD
%token AMPER PIPE MENOR MENIG MAYOR MAYIG DIST
%token MAS MENOS POR DIV MENOS NIL
%token<int> NRO
%token<string> LITERAL ID

%type<abs.exp> prog
%type<abs.ty> ty
%type<abs.field> ty_field
%type<abs.field list> ty_field_list
%type<abs.exp> expr
%type<abs.exp list> expr_list
%type<(abs.symbol * abs.exp) list> record_fields
%type<abs.exp list> args
%type<var> left_value
%type<abs.dec> dec
%type<abs.dec> vardec
%type<abs.dec> fundec
%type<abs.dec list> dec_list

%nonassoc THEN
%left ELSE
/* Resolves shift-reduce conflicts in expressions like:
  "if e1 then e2 . else e3" (because "else" has more prec than "then"). */
%nonassoc DO
/* Resolves many shift-reduce conflicts, for example, in expressions like:
  "while e1 do e2 . + e3" o "for e1 to e2 do e3 . | e4" */
%nonassoc OF
/* Resolves shift-reduce conflicts in expressions like:
  "m[10] . of 11" */
%nonassoc DOSPIG
%left PIPE
%left AMPER
%nonassoc IGUAL MENOR MENIG MAYOR MAYIG DIST
%left MAS MENOS
%left POR DIV
%right UMENOS

%start prog

%%

prog:
    expr EOF {$1}
  ;

expr:
    NRO {IntExp($1, pos())}
  | PI PD {UnitExp(pos())}
  | NIL {NilExp(pos())}
  | LITERAL {StringExp($1, pos())}
  | BREAK {BreakExp(pos())}
  | left_value {VarExp($1, pos())}
  | left_value DOSPIG expr {AssignExp({var=$1, exp=$3}, pos())}
  | PI expr PCOMA expr_list PD {SeqExp($2::$4, pos())}
  | expr PIPE expr {IfExp({test=$1, then'=IntExp(1, pos()), else'=SOME $3},
                          pos())}
  | expr AMPER expr {IfExp({test=$1, then'=$3, else'=SOME(IntExp(0, pos()))},
                           pos())}
  | expr IGUAL expr {OpExp({left=$1, oper=EqOp, right=$3}, pos())}
  | expr DIST expr {OpExp({left=$1, oper=NeqOp, right=$3}, pos())}
  | expr MENOR expr {OpExp({left=$1, oper=LtOp, right=$3}, pos())}
  | expr MENIG expr {OpExp({left=$1, oper=LeOp, right=$3}, pos())}
  | expr MAYOR expr {OpExp({left=$1, oper=GtOp, right=$3}, pos())}
  | expr MAYIG expr {OpExp({left=$1, oper=GeOp, right=$3}, pos())}
  | expr MAS expr {OpExp({left=$1, oper=PlusOp, right=$3}, pos())}
  | expr MENOS expr {OpExp({left=$1, oper=MinusOp, right=$3}, pos())}
  | expr POR expr {OpExp({left=$1, oper=TimesOp, right=$3}, pos())}
  | expr DIV expr {OpExp({left=$1, oper=DivideOp, right=$3}, pos())}
  | MENOS expr  %prec UMENOS
      {OpExp({left=IntExp(0, pos()), oper=MinusOp, right=$2}, pos())}
  | PI expr PD {$2}
  | ID PI args PD {CallExp({func=$1, args=$3}, pos())}
  | IF expr THEN expr {IfExp({test=$2, then'=$4, else'=NONE}, pos())}
  | IF expr THEN expr ELSE expr {IfExp({test=$2, then'=$4, else'=SOME $6}, pos())}
  | WHILE expr DO expr {WhileExp({test=$2, body=$4}, pos())}
  | FOR ID DOSPIG expr TO expr DO expr
      {ForExp({var=$2, escape=ref false, lo=$4, hi=$6, body=$8}, pos())}
  | LET dec_list IN END {LetExp({decs=$2, body=UnitExp(pos())}, pos())}
  | LET dec_list IN expr END {LetExp({decs=$2, body=$4}, pos())}
  | LET dec_list IN expr PCOMA expr_list END
      {LetExp({decs=$2, body=SeqExp($4::$6, pos())}, pos())}
  | left_value CI expr CD OF expr {ArrayExp({typ=simpleVarId $1, size=$3, init=$6}, pos())}
  | ID LI record_fields LD {RecordExp({fields=$3, typ=$1}, pos())}
  ;

expr_list:
    expr PCOMA expr_list {$1::$3}
  | expr {[$1]}
  ;

record_fields:
    ID IGUAL expr COMA record_fields {($1,$3)::$5}
  | ID IGUAL expr {[($1,$3)]}
  |  {[]}
  ;

dec_list:
    dec dec_list {joinDecs($1, $2)}
  |  {[]}
  ;

dec:
    TYPE ID IGUAL ty {TypeDec [({name=$2, ty=$4}, pos())]}
  | vardec {$1}
  | fundec {$1}
  ;

ty:
    ID {NameTy $1}
  | LI ty_field_list LD {RecordTy $2}
  | ARRAY OF ID {ArrayTy $3}
  ;

ty_field_list:
    ty_field COMA ty_field_list {$1::$3}
  | ty_field {[$1]}
  |  {[]}
  ;

ty_field:
    ID DOSP ID {{name=$1, escape=ref false, typ=$3}}
  ;

vardec:
    VAR ID DOSPIG expr
      {VarDec({name=$2, escape=ref false, typ=NONE, init=$4}, pos())}
  | VAR ID DOSP ID DOSPIG expr
      {VarDec({name=$2, escape=ref false, typ=SOME $4, init=$6}, pos())}
  ;

fundec:
    FUNCTION ID PI ty_field_list PD IGUAL expr
      {FunctionDec [({name=$2, params=$4, result=NONE, body=$7}, pos())]}
  | FUNCTION ID PI ty_field_list PD DOSP ID IGUAL expr
      {FunctionDec [({name=$2, params=$4, result=SOME $7, body=$9}, pos())]}
  ;

args:
    expr COMA args {$1::$3}
  | expr {[$1]}
  |  {[]}
  ;

left_value:
    ID {SimpleVar $1}
  | left_value PTO ID {FieldVar ($1, $3)}
  | left_value CI expr CD {SubscriptVar ($1, $3)}
  ;

%%
