structure show =
struct

open tree

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

end
