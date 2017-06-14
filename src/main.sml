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
open List BasicIO Nonstdio Unix

infixr 0 $
fun x $ y = x y

fun errParsing lbuf = raiseError (!numLine) $
  "when parsing: " ^ Lexing.getLexeme lbuf

fun lexStream(is: instream) =
  Lexing.createLexer (fn b => fn n => buff_input is b 0 n)

fun main args =
  let
    fun arg l s = (exists (fn x => x = s) l, filter (fn x => x <> s) l)
    val (ast, l1)      = arg args "-ast"
    val (escapes, l2)  = arg l1 "-escape"
    val (ir, l3)       = arg l2 "-ir"
    val (canon, l4)    = arg l3 "-canon"
    val (code, l5)     = arg l4 "-code"
    val (flow, l6)     = arg l5 "-flow"
    val (interf, l7)   = arg l6 "-interf"
    val (assembly, l8) = arg l7 "-S"

    fun outputFile [] = (if assembly then "a.s" else "a.out", [])
      | outputFile ("-o" :: file :: rest) = (file, rest)
      | outputFile (x::xs) = let val (f, rest) = outputFile xs
                              in (f, x::rest) end

    val (outFile, l9) = outputFile l8

    val (inStream, fileName) =
      case l9 of
        [n] => ((open_in n, n) handle _ => raise Fail $ n ^ " doesn't exist!")
      | []  => (std_in, "stdin")
      | _   => raise Fail "unknown option!"

  in
    let
      val lexbuf = lexStream inStream
      val expr = prog Token lexbuf handle _ => errParsing lexbuf
      val _ = findEscape expr
      val _ = transProg expr
      val intermList = trans.getResult()

      val (outStream, closeOutStream) =
        let fun nothing () = ()
        in if code then (TextIO.stdOut, nothing)
           else if assembly then ((TextIO.openOut outFile, nothing)  handle _ =>
                      raise Fail $ "Couldn't open output file " ^ outFile ^ "!")
           else let val gccProc = execute ("/bin/sh", ["-c",
                "gcc -o " ^ outFile ^ " -x assembler - -x none -ltigerruntime"])
                  val (gccOut, gccIn) = streamsOf gccProc
                  fun finish () = (TextIO.closeOut gccIn;
                                   if OS.Process.isSuccess $ reap gccProc
                                   then () else raise Fail $ "Assembler error.")
                in (gccIn, finish) end
        end

      fun printOut str = (TextIO.output (outStream, str);
                          TextIO.flushOut outStream)

      fun processString (label, str) = printOut $ (string label str) ^ "\n"

      fun processProc {frame, body} =
        let val canonizedBody = canonize body
          val assemLst = procEntryExit2 $ concat $
                          map (codegen frame) canonizedBody
          val (fGraph, nodeLst) = instrs2graph assemLst
          val (iGraph, liveOut) = interferenceGraph fGraph nodeLst
          val (instrLst, allocMap) = alloc assemLst frame
          fun tmp2str t = "%" ^ regToString (dictGet allocMap t)
          val codeLst = map (assem.format tmp2str) instrLst
          val _ = printOut $ procEntryExit3 frame $ String.concat codeLst
          val _ = if ir then print $ showTree body else ()
          val _ = if canon then app (print o showTree) $ canonize body else ()
          val _ = if flow then showfgraph fGraph assemLst nodeLst else ()
          val _ = if interf then showigraph iGraph else ()
        in () end

      val (procLst, strLst) = foldr (fn (frag, (pL, sL)) => case frag of
                                                  frame.PROC r => (r::pL, sL)
                                                | frame.STRING s => (pL, s::sL))
                                    ([],[]) intermList

      val _ = printOut ".data\n"
      val _ = app processString strLst
      val _ = printOut ".text\n"
      val _ = app processProc procLst

      val _ = if ast then printAst expr else ()
      val _ = closeOutStream ()
    in
      printStderr "Successful compilation\n"
    end handle Error (line, msg) => printErrorMsg (SOME fileName) line msg
  end handle Fail msg => printErrorMsg NONE NONE msg

val _ = main $ CommandLine.arguments()
