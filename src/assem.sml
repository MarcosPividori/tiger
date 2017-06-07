structure assem = struct

type reg = string
type temp = temp.temp
type label = temp.label

datatype instr = AOPER of {assem: string,
                           src: temp list,
                           dst: temp list,
                           jump: label list option}
               | ALABEL of {assem: string,
                            lab: label}
               | AMOVE of {assem: string,
                           src: temp,
                           dst: temp}

fun format (f: temp -> string) (i:instr) : string =
    case i of
        AOPER {assem, ...} => assem ^ "\n"
      | ALABEL {assem, ...} => assem ^ "\n"
      | AMOVE {assem, ...} => assem ^ "\n"

end