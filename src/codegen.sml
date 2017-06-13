structure codegen =
struct
local
  open tree
  open assem
  open frame

  infixr 0 $
  fun x $ y = x y

  fun st n = let val str = Int.toString n
              in if n < 0 then "-" ^ String.extract(str, 1, NONE) else str end

  fun withTmp gen = let val t = temp.newTemp() in gen t; t end

  (* checks if the number is in the range of 32bit signed integers *)
  val two31 = let fun f x n = if n = 32 then x else f (x * x) (2 * n)
               in (f 2 1) div 2 end
  val max32int = two31 - 1
  val min32int = ~ two31
  fun is32b n = n >= min32int andalso n <= max32int

  fun mapExp f (MOVE (e1, e2)) = MOVE (f e1, f e2)
    | mapExp f (EXP e1) = EXP $ f e1
    | mapExp f (JUMP (e1, l)) = JUMP (f e1, l)
    | mapExp f (CJUMP (r, e1, e2, l1, l2)) = CJUMP (r, f e1, f e2, l1, l2)
    | mapExp f (SEQ (st1, st2)) = SEQ (mapExp f st1, mapExp f st2)
    | mapExp f (LABEL l) = LABEL l
in

fun codegen frame (stm: stm) : instr list =
  let val ilist = ref (nil: instr list)
    fun emit x = ilist := x :: !ilist
    fun emitOper assem src dst =
            emit $ AOPER {assem=assem, src=src, dst=dst, jump=[]}
    fun emitJmp assem labels =
            emit $ AOPER {assem=assem, src=[], dst=[], jump=labels}

    (* rplConst ensures all const are 32bit signed integers, because for x64
     * immediate operands can only be 32b, except for move op into a reg. *)
    fun rplConst (CONST i) = if is32b i
          then CONST i
          else TEMP $ withTmp (fn t => emitOper ("movq $"^st i^", 'd0") [] [t])
      | rplConst (BINOP (b, e1, e2)) = BINOP (b, rplConst e1, rplConst e2)
      | rplConst (MEM e) = MEM $ rplConst e
      | rplConst (CALL (e, el)) = CALL (rplConst e, map rplConst el)
      | rplConst e = e

    (* munchArgs: Tree.exp list -> Temp.temp list
     * Push all the argument to the registers and the stack according to the
     * calling convention. *)
    fun munchArgs args = let
          fun munchArgsReg [] _ = []
            | munchArgsReg xs [] = munchArgsStack (rev xs)
            | munchArgsReg (x::xs) (reg::regs) =
                let val _ = munchStm $ MOVE (TEMP reg, x)
                in reg :: (munchArgsReg xs regs) end

          and munchArgsStack [] = []
            | munchArgsStack (x::xs) =
                let val _ = case x of
                    CONST i => emitOper ("pushq $"^(st i)) [] []
                  | NAME n => emitOper ("pushq "^n) [] []
                  | TEMP t => emitOper "pushq 's0" [t] []
                  (* Shouldn't happen because of the definition of callExp. *)
                  | MEM (TEMP t) => emitOper "pushq ('s0)" [t] []
                  | MEM (BINOP (PLUS, e, CONST c)) =>
                        emitOper ("pushq "^(st c)^"('s0)") [munchExp e] []
                  | MEM e => emitOper "pushq ('s0)" [munchExp e] []
                  | _ => emitOper "pushq 's0" [munchExp x] []
                in munchArgsStack xs end
        in munchArgsReg args argregs end

    (* munchStm: Tree.stm -> unit
     * Emits assembly to execute the given statement. *)
      (* move to temporary. *)
    and munchStm (MOVE (TEMP t1, e)) = (case e of
            MEM (BINOP (PLUS, CONST i, e1)) =>
               emitOper ("movq "^(st i)^"('s0), 'd0") [munchExp e1] [t1]
          | MEM (BINOP (PLUS, e1, CONST i)) =>
               emitOper ("movq "^(st i)^"('s0), 'd0") [munchExp e1] [t1]
          | MEM e1 => emitOper "movq ('s0), 'd0" [munchExp e1] [t1]
          | NAME l => emitOper ("movq $"^l^", 'd0") [] [t1]
          | CONST i => emitOper ("movq $"^(st i)^", 'd0") [] [t1]
          | _ => emit $ AMOVE {assem="movq 's0, 'd0", src=munchExp e, dst=t1})

      (* move to mem location. *)
      | munchStm (MOVE (MEM e1, e2)) = (case (e1, e2) of
            (BINOP (PLUS, CONST i, e), NAME l) =>
                emitOper ("movq $"^l^", "^(st i)^"('s0)") [munchExp e] []
          | (BINOP (PLUS, CONST i, e), CONST j) =>
                emitOper ("movq $"^(st j)^", "^(st i)^"('s0)") [munchExp e] []
          | (BINOP (PLUS, CONST i, e), _) =>
             emitOper ("movq 's0, "^(st i)^"('s1)") [munchExp e2, munchExp e] []
          | (BINOP (PLUS, e, CONST i), _) =>
                munchStm $ MOVE (MEM $ BINOP (PLUS, CONST i, e), e2)
          | (_, CONST i) =>
                emitOper ("movq $"^(st i)^", ('s0)") [munchExp e1] []
          | _ => emitOper "movq 's0, ('s1)" [munchExp e2, munchExp e1] [])

      | munchStm (MOVE (e1, e2)) =
          raise Fail "Invalid move to no temporary nor mem location."

      (* function call. *)
      | munchStm (EXP (CALL (NAME n, args))) =
          let val diffTmp = length args - length argregs
            (* diff * 8 must be 16byte aligned. *)
            val diff = if diffTmp mod 2 = 0 orelse diffTmp < 0
                  then diffTmp
                  else (emitOper "pushq $0" [] []; diffTmp + 1)
            val _ = emitOper ("call "^n) (munchArgs args) calldefs
          in if diff > 0
               then emitOper ("addq $"^(st diff)^", 'd0") [] []
               else ()
          end
      | munchStm (EXP (CALL _)) = raise Fail "Invalid call with no label."

      | munchStm (EXP e) = (munchExp e; ())

      (* jumps. *)
      | munchStm (JUMP (NAME n, lst)) = emitJmp ("jmp "^n) lst
      | munchStm (JUMP _) = raise Fail "Invalid jmp to no label."

      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
         let val _ = emitOper "cmpq 's1, 's0" [munchExp e1, munchExp e2] []
         in case oper of
              EQ => emitJmp ("je "^l1) [l1,l2]
            | NE => emitJmp ("jne "^l1) [l1,l2]
            | LT => emitJmp ("jl "^l1) [l1,l2]
            | GT => emitJmp ("jg "^l1) [l1,l2]
            | LE => emitJmp ("jle "^l1) [l1,l2]
            | GE => emitJmp ("jge "^l1) [l1,l2]
            | _ => raise Fail "Relational operator not supported."
         end

      (* SEQ shouldn't appear after canonization. *)
      | munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)

      | munchStm (LABEL l) = emit $ ALABEL {assem=l^":", lab=l}

    (* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    and munchExp (CONST n) = withTmp (fn r => munchStm $ MOVE (TEMP r, CONST n))
      | munchExp (TEMP t) = t
      | munchExp (BINOP (PLUS, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP r, e2);
           case e1 of
             CONST i => emitOper ("addq $"^(st i)^", 'd0") [r] [r]
           | TEMP t => emitOper "addq 's1, 'd0" [r, t] [r]
           | MEM (TEMP t) => emitOper "addq ('s1), 'd0" [r, t] [r]
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emitOper ("addq "^(st i)^"('s1), 'd0") [r, t] [r]
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emitOper ("addq "^(st i)^"('s1), 'd0") [r, t] [r]
           | MEM e => emitOper "addq ('s1), 'd0" [r, munchExp e] [r]
           | _ => emitOper "addq 's1, 'd0" [r, munchExp e1] [r]))

      | munchExp (BINOP (MINUS, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP r, e1);
           case e2 of
             CONST i => emitOper ("subq $"^(st i)^", 'd0") [r] [r]
           | TEMP t => emitOper "subq 's1, 'd0" [r, t] [r]
           | MEM (TEMP t) => emitOper "subq ('s1), 'd0" [r, t] [r]
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emitOper ("subq "^(st i)^"('s1), 'd0") [r, t] [r]
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emitOper ("subq "^(st i)^"('s1), 'd0") [r, t] [r]
           | MEM e => emitOper "subq ('s1), 'd0" [r, munchExp e] [r]
           | _ => emitOper "subq 's1, 'd0" [r, munchExp e2] [r]))

      | munchExp (BINOP (MUL, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP RAX, e2);
           case e1 of
             CONST i => emitOper ("imulq $"^(st i)^", 's0") [RAX] [RAX, RDX]
           | TEMP t => emitOper "imulq 's1, 's0" [RAX, t] [RAX, RDX]
           | MEM (TEMP t) => emitOper "imulq ('s1), 's0" [RAX, t] [RAX, RDX]
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emitOper ("imulq "^(st i)^"('s1), 's0") [RAX, t] [RAX, RDX]
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emitOper ("imulq "^(st i)^"('s1), 's0") [RAX, t] [RAX, RDX]
           | MEM e => emitOper "imulq ('s1), 's0" [RAX, munchExp e] [RAX, RDX]
           | _ => emitOper "imulq 's1, 's0" [RAX, munchExp e1] [RAX, RDX];
          munchStm $ MOVE (TEMP r, TEMP RAX)))

      | munchExp (BINOP (DIV, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP RAX, e1);
           emitOper "cqo" [RAX] [RDX]; (* sign extension. *)
           emitOper "idivq 's2" [RAX, RDX, munchExp e2] [RAX, RDX];
           munchStm $ MOVE (TEMP r, TEMP RAX)))

      | munchExp (BINOP _) = raise Fail "Binary operator not supported."

      | munchExp (MEM e) = withTmp (fn r => munchStm $ MOVE (TEMP r, MEM e))
      | munchExp (CALL _) = raise Fail "CALL shouldn't appear after canon."
      | munchExp (NAME n) = withTmp (fn r => munchStm $ MOVE (TEMP r, NAME n))
      | munchExp (ESEQ _) = raise Fail "ESEQ shouldn't appear after canon."
  in munchStm $ mapExp rplConst stm;
     rev $ !ilist
  end

end
end
