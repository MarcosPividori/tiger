open lexer
open parser
open error
open escape
open seman
open BasicIO Nonstdio

fun errParsing lbuf = raiseError (!numLine)
  ("when parsing: " ^ (Lexing.getLexeme lbuf))

fun lexStream(is: instream) =
  Lexing.createLexer(fn b => fn n => buff_input is b 0 n)

fun main args =
  let
    fun arg l s =
      (List.exists (fn x => x = s) l, List.filter (fn x => x <> s) l)
    val (arbol, l1)   = arg args "-arbol"
    val (escapes, l2) = arg l1 "-escapes"
    val (ir, l3)      = arg l2 "-ir"
    val (canon, l4)   = arg l3 "-canon"
    val (code, l5)    = arg l4 "-code"
    val (flow, l6)    = arg l5 "-flow"
    val (inter, l7)   = arg l6 "-inter"
    val (entrada, fileName) =
      case l7 of
        [n] => ((open_in n, n) handle _ => raise Fail (n ^ " doesn't exist!"))
      | []  => (std_in, "stdin")
      | _   => raise Fail "unknown option!"
  in
    let
      val lexbuf = lexStream entrada
      val expr = prog Token lexbuf handle _ => errParsing lexbuf
      val _ = findEscape expr
    in
      transProg expr;
      printStderr "Successful compilation\n"
    end handle Error (line, msg) => printErrorMsg (SOME fileName) line msg
  end handle Fail msg => printErrorMsg NONE NONE msg

val _ = main(CommandLine.arguments())
