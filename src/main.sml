open lexer
open parser
(*open escape
open semantic*)
open BasicIO Nonstdio

fun lexStream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n)

fun errParsing(lbuf) =
  let val msg = "Error when parsing!(" ^ (makestring(!numLine)) ^ ")" ^
                "[" ^ (Lexing.getLexeme lbuf) ^ "]\n"
  in print(msg); raise Fail "fin!"
  end

fun main(args) =
	let
    fun arg(l, s) =
			(List.exists (fn x => x = s) l, List.filter (fn x => x <> s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes")
		val (ir, l3)		  = arg(l2, "-ir")
		val (canon, l4)		= arg(l3, "-canon")
		val (code, l5)		= arg(l4, "-code")
		val (flow, l6)		= arg(l5, "-flow")
		val (inter, l7)		= arg(l6, "-inter")
		val entrada =
			case l7 of
			  [n] => ((open_in n) handle _ => raise Fail (n ^ " doesn't exist!"))
			| []  => std_in
			| _   => raise Fail "unknown option!"
		val lexbuf = lexStream entrada
		val expr = prog Token lexbuf handle _ => errParsing lexbuf
		(*val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()*)
	in
		(*transProg(expr);*)
		print "successful compilation!\n"
	end	handle Fail s => print("Fail: " ^ s ^ "\n")

val _ = main(CommandLine.arguments())
