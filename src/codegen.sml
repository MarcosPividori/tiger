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
in

fun codegen frame (stm: stm) : instr list =
  let val ilist = ref (nil: instr list)
    fun emit x = ilist := x :: !ilist
    (* munchArgs: Tree.exp list -> Temp.temp list *)
    fun munchArgs args = let
          fun munchArgsReg [] _ = []
            | munchArgsReg xs [] = munchArgsStack (rev xs)
            | munchArgsReg (x::xs) (reg::regs) =
                let val _ = munchStm $ MOVE (TEMP reg, x)
                in reg :: (munchArgsReg xs regs) end

          and munchArgsStack [] = []
            | munchArgsStack (x::xs) =
                let val _ = case x of
                    CONST i => emit $ AOPER {assem="pushq $"^(st i),
                                             src=[], dst=[sp], jump=NONE}
                  | NAME n => emit $ AOPER {assem="pushq "^n,
                                            src=[], dst=[sp], jump=NONE}
                  | TEMP t => emit $ AOPER {assem="pushq 's0",
                                            src=[t], dst=[sp], jump=NONE}
                  (* Shouldn't happen because of the definition of callExp. *)
                  | MEM (TEMP t) => emit $ AOPER {assem="pushq ('s0)",
                                                  src=[t], dst=[sp], jump=NONE}
                  | MEM (BINOP (PLUS, e, CONST c)) =>
                        emit $ AOPER {assem="pushq "^(st c)^"('s0)",
                                      src=[munchExp e], dst=[sp], jump=NONE}
                  | MEM e => emit $ AOPER {assem="pushq ('s0)",
                                          src=[munchExp e], dst=[sp], jump=NONE}
                  | _ => emit $ AOPER {assem="pushq 's0",
                                       src=[munchExp x], dst=[sp], jump=NONE}
                in munchArgsStack xs end
        in munchArgsReg args argregs end

    (* munchStm: Tree.stm -> unit *)
      (* move to temporary. *)
    and munchStm (MOVE (TEMP t1, MEM (BINOP (PLUS, CONST i, e)))) =
          emit $ AOPER {assem="movq "^(st i)^"('s0), 'd0",
                        src=[munchExp e], dst=[t1], jump=NONE}
      | munchStm (MOVE (TEMP t1, MEM (BINOP (PLUS, e, CONST i)))) =
          munchStm $ MOVE (TEMP t1, MEM $ BINOP (PLUS, CONST i, e))

      | munchStm (MOVE (TEMP t1, MEM e)) =
          emit $ AOPER {assem="movq ('s0), 'd0",
                        src=[munchExp e], dst=[t1], jump=NONE}

      | munchStm (MOVE (TEMP t1, NAME l)) =
          emit $ AOPER {assem="movq $"^l^", 'd0",
                        src=[], dst=[t1], jump=NONE}

      | munchStm (MOVE (TEMP t1, CONST i)) =
          emit $ AOPER {assem="movq $"^(st i)^", 'd0",
                        src=[], dst=[t1], jump=NONE}

      | munchStm (MOVE (TEMP t1, e)) =
          emit $ AMOVE {assem="movq 's0, 'd0", src=munchExp e, dst=t1}

      (* move to mem location. *)
      | munchStm (MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2)) =
          (case e2 of
            NAME l => emit $ AOPER {assem="movq $"^l^", "^(st i)^"('s0)",
                                    src=[munchExp e1], dst=[], jump=NONE}
          | CONST j => emit $ AOPER{assem="movq $"^(st j)^", "^(st i)^"('s0)",
                                    src=[munchExp e1], dst=[], jump=NONE}
          | _ => emit $ AOPER{assem="movq 's0, "^(st i)^"('s1)",
                             src=[munchExp e2, munchExp e1], dst=[], jump=NONE})
      | munchStm (MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2)) =
          munchStm $ MOVE (MEM $ BINOP (PLUS, CONST i, e1), e2)

      | munchStm (MOVE (MEM e1, e2)) =
          (case e2 of
            CONST i => emit $ AOPER {assem="movq $"^(st i)^", ('s0)",
                                     src=[munchExp e1], dst=[], jump=NONE}
          | _ => emit $ AOPER {assem="movq 's0, ('s1)",
                             src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

      | munchStm (MOVE (e1, e2)) =
          raise Fail "Invalid move to no temporary nor mem location!"

      (* function call. *)
      | munchStm (EXP (CALL (NAME n, args))) =
          let val _ = emit $ AOPER {assem="call "^n, src=munchArgs args,
                                    dst=calldefs, jump=NONE}
            val diff = length args - length argregs
          in if diff > 0
               then emit $ AOPER {assem="addq $"^(st diff)^", 'd0",
                                  src=[], dst=[sp], jump=NONE}
               else ()
          end
      | munchStm (EXP (CALL _)) = raise Fail "Invalid call with no label."

      | munchStm (EXP e) = (munchExp e; ())

      (* jumps. *)
      | munchStm (JUMP (NAME n, lst)) =
          emit $ AOPER {assem="jmp "^n, src=[], dst=[], jump=SOME lst}
      | munchStm (JUMP _) = raise Fail "Invalid jmp to no label."

      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
         let val _ = emit $ AOPER {assem="cmpq 's1 's0",
                              src=[munchExp e1, munchExp e2], dst=[], jump=NONE}
         in case oper of
            EQ => emit $ AOPER {assem="je "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | NE => emit $ AOPER {assem="jne "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | LT => emit $ AOPER {assem="jl "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | GT => emit $ AOPER {assem="jg "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | LE => emit $ AOPER {assem="jle "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | GE => emit $ AOPER {assem="jge "^l1,src=[],dst=[],jump=SOME [l1,l2]}
          | _ => raise Fail "Operator not supported!"
         end

      | munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)
                                  (* shouldn't happen after canonization.*)

      | munchStm (LABEL l) = emit $ ALABEL {assem=l^":", lab=l}

    (* munchExp: Tree.exp -> Temp.temp *)
    and munchExp (CONST n) = withTmp (fn r => munchStm $ MOVE (TEMP r, CONST n))
      | munchExp (TEMP t) = t
      | munchExp (BINOP (PLUS, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP r, e2);
           case e1 of
             CONST i => emit $ AOPER {assem="addq $"^(st i)^", 'd0",
                                      src=[r], dst=[r], jump=NONE}
           | TEMP t => emit $ AOPER {assem="addq 's1, 'd0",
                                     src=[r, t], dst=[r], jump=NONE}
           | MEM (TEMP t) => emit $ AOPER {assem="addq ('s1), 'd0",
                                           src=[r, t], dst=[r], jump=NONE}
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emit $ AOPER {assem="addq "^(st i)^"('s1), 'd0",
                               src=[r, t], dst=[r], jump=NONE}
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emit $ AOPER {assem="addq "^(st i)^"('s1), 'd0",
                               src=[r, t], dst=[r], jump=NONE}
           | MEM e => emit $ AOPER {assem="addq ('s1), 'd0",
                                    src=[r, munchExp e], dst=[r], jump=NONE}
           | _ => emit $ AOPER {assem="addq 's1, 'd0",
                                src=[r, munchExp e1], dst=[r], jump=NONE}))

      | munchExp (BINOP (MINUS, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP r, e1);
           case e2 of
             CONST i => emit $ AOPER {assem="subq $"^(st i)^", 'd0",
                                      src=[r], dst=[r], jump=NONE}
           | TEMP t => emit $ AOPER {assem="subq 's1, 'd0",
                                     src=[r, t], dst=[r], jump=NONE}
           | MEM (TEMP t) => emit $ AOPER {assem="subq ('s1), 'd0",
                                           src=[r, t], dst=[r], jump=NONE}
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emit $ AOPER {assem="subq "^(st i)^"('s1), 'd0",
                               src=[r, t], dst=[r], jump=NONE}
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emit $ AOPER {assem="subq "^(st i)^"('s1), 'd0",
                               src=[r, t], dst=[r], jump=NONE}
           | MEM e => emit $ AOPER {assem="subq ('s1), 'd0",
                                    src=[r, munchExp e], dst=[r], jump=NONE}
           | _ => emit $ AOPER {assem="subq 's1, 'd0",
                                src=[r, munchExp e2], dst=[r], jump=NONE}))

      | munchExp (BINOP (MUL, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP rax, e2);
           case e1 of
             CONST i => emit $ AOPER {assem="imulq $"^(st i)^", 's0",
                                      src=[rax], dst=[rax, rdx], jump=NONE}
           | TEMP t => emit $ AOPER {assem="imulq 's1, 's0",
                                     src=[rax, t], dst=[rax, rdx], jump=NONE}
           | MEM (TEMP t) => emit $ AOPER {assem="imulq ('s1), 's0",
                                        src=[rax, t], dst=[rax, rdx], jump=NONE}
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emit $ AOPER {assem="imulq "^(st i)^"('s1), 's0",
                               src=[rax, t], dst=[rax, rdx], jump=NONE}
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emit $ AOPER {assem="imulq "^(st i)^"('s1), 's0",
                               src=[rax, t], dst=[rax, rdx], jump=NONE}
           | MEM e => emit $ AOPER {assem="imulq ('s1), 's0",
                               src=[rax, munchExp e], dst=[rax, rdx], jump=NONE}
           | _ => emit $ AOPER {assem="imulq 's1, 's0",
                            src=[rax, munchExp e1], dst=[rax, rdx], jump=NONE};
          munchStm $ MOVE (TEMP r, TEMP rax)))

      | munchExp (BINOP (DIV, e1, e2)) = withTmp (fn r =>
          (munchStm $ MOVE (TEMP rax, e1);
           emit $ AOPER {assem="cdq", src=[rax], dst=[rdx], jump=NONE};
           case e1 of
             CONST i => emit $ AOPER {assem="idivq $"^(st i),
                                      src=[rax, rdx], dst=[rax, rdx], jump=NONE}
           | TEMP t => emit $ AOPER {assem="idivq 's2",
                                   src=[rax, rdx, t], dst=[rax, rdx], jump=NONE}
           | MEM (TEMP t) => emit $ AOPER {assem="idivq ('s2)",
                                   src=[rax, rdx, t], dst=[rax, rdx], jump=NONE}
           | MEM (BINOP (PLUS, CONST i, TEMP t)) =>
                 emit $ AOPER {assem="idivq "^(st i)^"('s2)",
                               src=[rax, rdx, t], dst=[rax, rdx], jump=NONE}
           | MEM (BINOP (PLUS, TEMP t, CONST i)) =>
                 emit $ AOPER {assem="idivq "^(st i)^"('s2)",
                               src=[rax, rdx, t], dst=[rax, rdx], jump=NONE}
           | MEM e => emit $ AOPER {assem="idivq ('s2)",
                          src=[rax, rdx, munchExp e], dst=[rax, rdx], jump=NONE}
           | _ => emit $ AOPER {assem="idivq 's2",
                       src=[rax, rdx, munchExp e1], dst=[rax, rdx], jump=NONE};
          munchStm $ MOVE (TEMP r, TEMP rax)))

      | munchExp (BINOP _) = raise Fail "Operator not supported!"

      | munchExp (MEM e) = withTmp (fn r => munchStm $ MOVE (TEMP r, MEM e))
      | munchExp (CALL _) = raise Fail "CALL shouldn't appear after canon."
      | munchExp (NAME n) = withTmp (fn r => munchStm $ MOVE (TEMP r, NAME n))
      | munchExp (ESEQ _) = raise Fail "ESEQ shouldn't appear after canon."
  in munchStm stm;
     rev $ !ilist
  end

end
end
