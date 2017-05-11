structure env =
struct

open types

datatype EnvEntry =
    VIntro of {access: unit(*trans.access*), level: unit(*int*)} (* int readonly *)
  | Var of {ty: Type, access: unit(*trans.access*), level: unit(*int*)}
  | Func of {level: unit(*trans.level*), label: unit(*temp.label*),
             formals: Type list, result: Type, extern: bool}

end
