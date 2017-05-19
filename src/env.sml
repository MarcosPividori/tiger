structure env =
struct

open types

datatype EnvEntry =
    VIntro of {access: trans.access, depth: int} (* int readonly *)
  | Var of {ty: Type, access: trans.access, depth: int}
  | Func of {level: trans.level, label: temp.label,
             formals: Type list, result: Type, extern: bool}

end
