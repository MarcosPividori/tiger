{
open numLine
open parser

val numComm = ref 0

fun ++ r = (r := !r + 1; !r)
fun -- r = (r := !r - 1; !r)

val stringToInteger = valOf o Int.fromString

infixr 0 $
fun x $ y = x y

fun controlToHexTabNl "\\n" = "\\x0a"
  | controlToHexTabNl "\\t" = "\\x09"
  | controlToHexTabNl _ = raise Fail "Unknown escaped character!"

fun integerToHex n =
  "\\x" ^ (if n<=15 then "0" else "") ^ (Int.fmt StringCvt.HEX n)

fun controlToHexCaret s =
  let val c = ord $ hd $ tl $ tl $ explode s
  in integerToHex(c - ord(#"@")) end

fun controlToHexInt s =
  let val n = ord $ valOf $ Char.fromString s
  in integerToHex n end

exception unknownKey

val keywords = Polyhash.mkPolyTable(20, unknownKey)

val _ = List.app (fn (c, v) => Polyhash.insert keywords (c, v))
  [("type",     TYPE),
   ("array",    ARRAY),
   ("of",       OF),
   ("var",      VAR),
   ("function", FUNCTION),
   ("let",      LET),
   ("in",       IN),
   ("end",      END),
   ("if",       IF),
   ("then",     THEN),
   ("else",     ELSE),
   ("while",    WHILE),
   ("do",       DO),
   ("for",      FOR),
   ("to",       TO),
   ("break",    BREAK),
   ("nil",      NIL)]

fun keywordOrId s = (Polyhash.find keywords s) handle unknownKey => ID s
}

let SPC = [` ``\t``\r``\^L`]
let MN = [`a`-`z`]
let L = [`A`-`Z``a`-`z`]
let LDU = [`A`-`Z``a`-`z``0`-`9``_`]
let D = [`0`-`9`]

rule Token = parse
    "/*"    { ++ numComm; Comment lexbuf }
  | eof     { EOF }
  | `\n`    { ++ numLine; Token lexbuf }
  | SPC+    { Token lexbuf }
  | `.`     { PTO }
  | `:`     { DOSP }
  | ":="    { DOSPIG }
  | `,`     { COMA }
  | `;`     { PCOMA }
  | `=`     { IGUAL }
  | `(`     { PI }
  | `)`     { PD }
  | `[`     { CI }
  | `]`     { CD }
  | `{`     { LI }
  | `}`     { LD }
  | `&`     { AMPER }
  | `|`     { PIPE }
  | `<`     { MENOR }
  | "<="    { MENIG }
  | `>`     { MAYOR }
  | ">="    { MAYIG }
  | "<>"    { DIST }
  | `+`     { MAS }
  | `-`     { MENOS }
  | `*`     { POR }
  | `/`     { DIV }
  | `"`     { LITERAL $ Literal lexbuf }
  | D+      { NRO $ stringToInteger $ getLexeme lexbuf }
  | MN+     { keywordOrId $ getLexeme lexbuf }
  | L LDU*  { ID $ getLexeme lexbuf }
  | _       { raise Fail ("lex:[" ^ getLexeme(lexbuf) ^ "]!") }

and Literal = parse
    `"`             { "" }
  | eof             { raise Fail "Incomplete string." }
  | `\n`            { raise Fail "Incomplete string. New line found." }
  | "\\"[`"``\\`]   { getLexeme lexbuf  ^ Literal lexbuf  }
  | "\\"[`n``t`]    { controlToHexTabNl (getLexeme lexbuf) ^ Literal lexbuf }
  | "\\^"[`@`-`_`]  { controlToHexCaret (getLexeme lexbuf) ^ Literal lexbuf }
  | "\\"D D D       { controlToHexInt (getLexeme lexbuf) ^ Literal lexbuf }
  | "\\"            { LiteralExt lexbuf }
  | _               { let val s = getLexeme lexbuf
                      in if s > "\^_"
                           then s ^ Literal lexbuf
                           else raise Fail "Illegal character!"
                      end
                    }

and LiteralExt = parse
    eof   { raise Fail "Incomplete string." }
  | `\n`  { ++ numLine ; LiteralExt lexbuf }
  | SPC+  { LiteralExt lexbuf }
  | "\\"  { Literal lexbuf }
  | _     { raise Fail "Incomplete string. \\\\ malformed." }

and Comment = parse
    "*/"    { (if -- numComm = 0 then Token else Comment) lexbuf }
  | "/*"    { ++ numComm; Comment lexbuf }
  | eof     { raise Fail "Incomplete comment." }
  | `\n`    { ++ numLine ; Comment lexbuf }
  | _       { Comment lexbuf }
;
