structure tree = struct

datatype exp = CONST of int
       | NAME of temp.label
       | TEMP of temp.temp
       | BINOP of binop*exp*exp
       | MEM of exp
       | CALL of exp*exp list
       | ESEQ of stm*exp

and stm = MOVE of exp*exp
  | EXP of exp
  | JUMP of exp*temp.label list
  | CJUMP of relop*exp*exp*temp.label*temp.label
  | SEQ of stm*stm
  | LABEL of temp.label

and binop = PLUS | MINUS | MUL | DIV | AND | OR
    | LSHIFT | RSHIFT | ARSHIFT | XOR

and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE
    | UGT | UGE

fun notRel EQ = NE
  | notRel NE = EQ
  | notRel LT = GE
  | notRel GE = LT
  | notRel GT = LE
  | notRel LE = GT
  | notRel ULT = UGE
  | notRel UGE = ULT
  | notRel ULE = UGT
  | notRel UGT = ULE

end
