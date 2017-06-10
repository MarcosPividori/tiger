open canon
open codegen
open error
open escape
open lexer
open liveness
open parser
open regalloc
open seman
open show
open List BasicIO Nonstdio

infixr 0 $
fun x $ y = x y

fun errParsing lbuf = raiseError (!numLine) $
  "when parsing: " ^ Lexing.getLexeme lbuf

fun lexStream(is: instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n)

fun main args =
  let
    fun arg l s = (exists (fn x => x = s) l, filter (fn x => x <> s) l)
    val (ast, l1)     = arg args "-ast"
    val (escapes, l2) = arg l1 "-escape"
    val (ir, l3)      = arg l2 "-ir"
    val (canon, l4)   = arg l3 "-canon"
    val (code, l5)    = arg l4 "-code"
    val (flow, l6)    = arg l5 "-flow"
    val (interf, l7)  = arg l6 "-interf"
    val (inter, l8)   = arg l7 "-inter"
    val (entrada, fileName) =
      case l8 of
        [n] => ((open_in n, n) handle _ => raise Fail $ n ^ " doesn't exist!")
      | []  => (std_in, "stdin")
      | _   => raise Fail "unknown option!"
  in
    let
      val lexbuf = lexStream entrada
      val expr = prog Token lexbuf handle _ => errParsing lexbuf
      val _ = findEscape expr
      val _ = transProg expr
      val intermList = trans.getResult()

      val (procList, stringList) = foldr (fn (frag, (pl, sl)) =>
          case frag of
            frame.PROC r => (r :: pl, sl)
          | frame.STRING p => (pl, p :: sl))
        ([],[]) intermList

      fun processProc {frame, body} =
        let val canonizedBody = canonize body
          val assemLst = concat $ map (codegen frame) canonizedBody
          val (fGraph, nodeLst) = instrs2graph assemLst
          val (iGraph, liveOut) = interferenceGraph fGraph nodeLst
          val (instrLst, allocMap) = alloc assemLst frame
          val codeLst = map
                (assem.format (fn t => regToString $ dictGet allocMap t))
                instrLst
          val _ = if ir then print $ showTree body else ()
          val _ = if canon then
            let val canonList = concat $ map (canonize o #body) procList
             in app (print o showTree) canonList end else ()
          val _ = if code then app print codeLst else ()
          val _ = if flow then showfgraph fGraph assemLst nodeLst else ()
          val _ = if interf then showigraph iGraph else ()
        in
          ()
        end

      val _ = map processProc procList

      val _ = if ast then printAst expr else ()
    in
      printStderr "Successful compilation\n"
    end handle Error (line, msg) => printErrorMsg (SOME fileName) line msg
  end handle Fail msg => printErrorMsg NONE NONE msg

val _ = main $ CommandLine.arguments()
