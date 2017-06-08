structure canon =
struct
local
  open dict
  open tree

  infixr 0 $
  fun x $ y = x y

  (* From an arbitrary Tree statement, produce a list of cleaned trees
   * satisfying the following properties:
   *  1. No SEQ's or ESEQ's
   *  2. The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
   *)
  fun linearize (stm0: stm) : stm list =
    let
      (* if left stm is a const exp, ignore it, otherwise, check right stm *)
      infix %
      fun (EXP (CONST _)) % x = x
        | x % (EXP (CONST _)) = x
        | x % y = SEQ (x, y)

      (* determines if the given stmt s commutes with the given expression e,
       * that means is the same to execute s before or after evaluating e *)
      fun commute (EXP (CALL (NAME "_checkIndexArray", _))) _ = true
            (* Not sure about the correctness of this commute...
             * for example: i:=10; (i:=0; 1) + (checkIdx a i; a[i])
             * is not the same than:
             *              i:=10; checkIdx a i; (i:=0; 1) + (a[i])
             *)
        | commute (EXP (CALL (NAME "_checkNil", _))) _ = true
            (* Not sure about the correctness of this commute...
             * for example: r:=nil; (r:={a=1}; 1) + (checkNil r; r.a)
             * is not the same than:
             *              r:=nil; checkNil r; (r:={a=1}; 1) + (r.a)
             *)
        | commute (EXP x) y =
            let fun immut (NAME _) = true
                  | immut (CONST _) = true
                  | immut (TEMP FP) = true
                  | immut (BINOP (_, x, y)) = immut x andalso immut y
                  | immut _ = false
            in immut x orelse immut y end
        | commute _ _ = false

      (* nop statement *)
      val nop = EXP $ CONST 0

      (* takes a list of expressions and returns a pair of (statement, expr-list).
       * The statement contains all the things that must be executed before the
       * expr-list. We remove all the ESEQ from the expressions. *)
      fun reorder ((e as CALL _)::rest) =
            (* We need to move the CALLs to the statement list, because each CALL
             * has the side-effect of rewriting the rv register. If we have
             * multiple calls inside an exp, one call can overwrite the rv of the
             * other call. *)
            let val t = temp.newTemp()
            in reorder (ESEQ (MOVE (TEMP t, e), TEMP t) :: rest) end
        | reorder (a::rest) =
            let val (stms, e) = do_exp a
              val (stms', el) = reorder rest
            in if commute stms' e
              then (stms % stms', e::el)
              else
                let val t = temp.newTemp()
                in (stms % MOVE (TEMP t, e) % stms',
                    TEMP t :: el) end
            end
        | reorder nil = (nop, nil)

      (* reorders the given expr-list and then applies the build fn to the new
       * exp-list, to build a new version of the expression given an ESEQ-clean
       * version of each subexp. Returns a pair: (statement, new-expr)
       * The statement contains all the side effects pulled out of the expr. *)
      and reorder_exp el build =
            let val (stms, el') = reorder el
            in (stms, build el') end

      (* reorders the given expr-list and then applies the build fn to the new
       * exp-list, to build a new version of the statement given an ESEQ-clean
       * version of each subexp. Returns a statement. *)
      and reorder_stm el build =
            let val (stms, el') = reorder el
            in stms % build el' end

      (* takes an stm and returns a new stm after processing all the exprs *)
      and do_stm (SEQ (a, b)) = do_stm a % do_stm b
        | do_stm (JUMP (e, labs)) =
            reorder_stm [e] (fn l => JUMP (hd l, labs))
        | do_stm (CJUMP (p, a, b, t, f)) =
            reorder_stm [a, b] (fn l => CJUMP (p, hd l, hd $ tl l, t, f))
        | do_stm (MOVE (TEMP t, CALL (NAME f, el))) =
            reorder_stm el (fn l => MOVE (TEMP t, CALL (NAME f, l)))
        | do_stm (MOVE (TEMP t, CALL (e, el))) = (* never happen for tiger *)
            reorder_stm (e::el) (fn l => MOVE (TEMP t, CALL (hd l, tl l)))
        | do_stm (MOVE (TEMP t, b)) =
            reorder_stm [b] (fn l => MOVE (TEMP t, hd l))
        | do_stm (MOVE (MEM e, b)) =
            reorder_stm [e, b] (fn l => MOVE (MEM $ hd l, hd $ tl l))
        | do_stm (MOVE (ESEQ (s, e), b)) = do_stm $ SEQ (s, MOVE (e, b))
        | do_stm (EXP (CALL (NAME f, el))) =
            reorder_stm el (fn l => EXP $ CALL (NAME f, l))
        | do_stm (EXP (CALL (e, el))) = (* never happen for tiger *)
            reorder_stm (e::el) (fn l => EXP $ CALL (hd l, tl l))
        | do_stm (EXP e) = reorder_stm [e] (fn l => EXP $ hd l)
        | do_stm s = reorder_stm [] (fn _ => s)

      (* takes an exp and returns a pair: (statement, new-expr) *)
      and do_exp (BINOP (p, a, b)) =
            reorder_exp [a, b] (fn l => BINOP (p, hd l, hd $ tl l))
        | do_exp (MEM a) = reorder_exp [a] (fn l => MEM $ hd l)
        | do_exp (ESEQ (s, e)) =
            let val stms = do_stm s
              val (stms', e) = do_exp e
            in (stms%stms', e) end
        | do_exp (CALL (NAME f, el)) =
            reorder_exp el (fn l => CALL (NAME f, l))
        | do_exp (CALL (e, el)) = (* never happen for tiger *)
            reorder_exp (e::el) (fn l => CALL (hd l, tl l))
        | do_exp e = reorder_exp [] (fn _ => e)

      (* gets rid of the top-level SEQ's, producing a list *)
      fun linear (SEQ (a, b)) l = linear a $ linear b l
        | linear s l = s::l

    in (* body of linearize *)
      linear (do_stm stm0) nil
    end

  (* From a list of cleaned trees, produce a list of
   * basic blocks satisfying the following properties:
   *  1. and 2. as above;
   *  3. Every block begins with a LABEL;
   *      4. A LABEL appears only at the beginning of a block;
   *      5. Any JUMP or CJUMP is the last stm in a block;
   *      6. Every block ends with a JUMP or CJUMP;
   * Also produce the "done" label to which control will be passed
   * upon exit. *)
  fun basicBlocks stms =
    let val done = temp.newLabel()
      fun blocks ((head as LABEL _) :: tail) blist =
            let
              fun endblock stms thisblock = blocks stms $ rev thisblock :: blist
              fun next ((s as (JUMP _))::rest) thisblock =
                    endblock rest (s::thisblock)
                | next ((s as (CJUMP _))::rest) thisblock =
                    endblock rest (s::thisblock)
                | next (stms as (LABEL lab :: _)) thisblock =
                    next (JUMP (NAME lab, [lab]) :: stms) thisblock
                | next (s::rest) thisblock = next rest $ s::thisblock
                | next nil thisblock =
                    next [JUMP (NAME done, [done])] thisblock
            in next tail [head] end
        | blocks nil blist = rev blist
        | blocks stms blist = blocks (LABEL (temp.newLabel())::stms) blist
    in (blocks stms nil, done) end

  (* given a non-nil list xs, returns a pair: (xsDeletedLast, last) *)
  fun splitLast [x] = (nil, x)
    | splitLast (h::t) = let val (t', last) = splitLast t in (h::t', last) end
    | splitLast _ = raise Fail "empty list not expected!"

  fun trace table (b as (LABEL lab :: _)) rest =
        let val table = dictRInsert table lab nil
        in case splitLast b of
             (most, JUMP (NAME lab, _)) =>
               (case dictSearch table lab of
                  SOME (b' as _::_) => most @ trace table b' rest
                | _ => b @ getNext table rest)
           | (most, CJUMP (opr, x, y, t, f)) =>
               (case (dictSearch table t, dictSearch table f) of
                  (_, SOME (b' as _::_)) => b @ trace table b' rest
                | (SOME (b' as _::_), _) =>
                  most @ [CJUMP (notRel opr, x, y, f, t)]
                       @ trace table b' rest
                | _ => let val f' = temp.newLabel()
                    in most @ [CJUMP (opr, x, y, t, f'),
                               LABEL f',
                               JUMP (NAME f, [f])]
                            @ getNext table rest
                    end)
           | (most, JUMP _) => b @ getNext table rest
           | _ => raise Fail "unexpected pattern matching failure!"
        end
    | trace _ _ _ = raise Fail "unexpected pattern matching failure!"

  and getNext table ((b as (LABEL lab::_))::rest) =
        (case dictSearch table lab of
           SOME (_::_) => trace table b rest
         | _ => getNext table rest)
    | getNext _ nil = nil
    | getNext _ _ = raise Fail "unexpected pattern matching failure!"

  (* From a list of basic blocks satisfying properties 1-6,
   * along with an "exit" label, produce a list of stms such that:
   *  1. and 2. as above;
   *  7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
   * The blocks are reordered to satisfy property 7; also
   * in this reordering as many JUMP(T.NAME(lab)) statements
   * as possible are eliminated by falling through into T.LABEL(lab).
   *)
  fun traceSchedule (blocks, done) =
      let
        (* inserts the given blocks to the hash table indexed by their labels. *)
        fun enterBlock (b as (LABEL s :: _), table) = dictRInsert table s b
          | enterBlock (_, table) = table
        val blocksTable = foldr enterBlock (dictNewStr()) blocks
      in (getNext blocksTable blocks) @ [LABEL done]
      end
in

fun canonize (e: stm) : stm list = (traceSchedule o basicBlocks o linearize) e

end
end
