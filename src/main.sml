open canon
open codegen
open error
open escape
open lexer
open parser
open seman
open show
open List BasicIO Nonstdio

infixr 0 $
fun x $ y = x y

fun errParsing lbuf = raiseError (!numLine)
  ("when parsing: " ^ (Lexing.getLexeme lbuf))

fun lexStream(is: instream) =
  Lexing.createLexer(fn b => fn n => buff_input is b 0 n)

fun main args =
  let
    fun arg l s = (exists (fn x => x = s) l, filter (fn x => x <> s) l)
    val (ast, l1)   = arg args "-ast"
    val (escapes, l2) = arg l1 "-escape"
    val (ir, l3)      = arg l2 "-ir"
    val (canon, l4)   = arg l3 "-canon"
    val (code, l5)    = arg l4 "-code"
    val (flow, l6)    = arg l5 "-flow"
    val (inter, l7)   = arg l6 "-inter"
    val (entrada, fileName) =
      case l7 of
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

      val assemList = concat $ foldr (fn ({frame, body}, lst) =>
                                      map (codegen frame) (canonize body) @ lst)
                               [] procList

      val _ = if ast then printAst expr else ()

      val _ = if ir then app (print o showTree o #body) procList else ()

      val _ = if canon then
                let val canonList = concat $ map (canonize o #body) procList
                 in app (print o showTree) canonList end else ()

      val _ = if code then app print $ map (assem.format (fn n => n)) assemList
                else ()
    in
      printStderr "Successful compilation\n"
    end handle Error (line, msg) => printErrorMsg (SOME fileName) line msg
  end handle Fail msg => printErrorMsg NONE NONE msg

val _ = main $ CommandLine.arguments()
