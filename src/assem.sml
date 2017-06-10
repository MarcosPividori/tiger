structure assem = struct

type reg = string
type temp = temp.temp
type label = temp.label

datatype instr = AOPER of {assem: string,
                           src: temp list,
                           dst: temp list,
                           jump: label list}
               | ALABEL of {assem: string,
                            lab: label}
               | AMOVE of {assem: string,
                           src: temp,
                           dst: temp}

fun replaceSrcDst src dst (#"'" :: #"s" :: n :: xs) =
      explode (List.nth (src, ord n - ord #"0")) @ replaceSrcDst src dst xs
  | replaceSrcDst src dst (#"'" :: #"d" :: n :: xs) =
      explode (List.nth (dst, ord n - ord #"0")) @ replaceSrcDst src dst xs
  | replaceSrcDst src dst (a :: xs) = a :: replaceSrcDst src dst xs
  | replaceSrcDst src dst [] = []

fun format (f: temp -> string) (i:instr) : string =
    case i of
        AOPER {assem, src, dst, jump} =>
          implode (replaceSrcDst (map f src) (map f dst) (explode assem)) ^ "\n"
      | AMOVE {assem, src, dst} =>
          implode (replaceSrcDst [f src] [f dst] (explode assem)) ^ "\n"
      | ALABEL {assem, ...} => assem ^ "\n"

end
