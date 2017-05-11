structure types =
struct

type unique = unit ref

datatype Type =
    TUnit
  | TNil
  | TInt
  | TString
  | TArray of Type * unique
  | TRecord of (string * Type option ref * int) list * unique

end
