open BasicIO

exception Error of int option * string

fun raiseError line msg = raise Error (SOME line, msg)

fun printStderr msg = (output (std_err, msg); flush_out std_err)

fun printErrorMsg file line msg =
  let
    val fileStr = case file of
      SOME f => f ^ ":"
    | NONE => ""
    val lineStr = case line of
      SOME l => (makestring l) ^ ":"
    | NONE => ""
  in printStderr (fileStr ^ lineStr ^ " " ^ msg ^ "\n") end
