structure escape =
struct
local

open abs
open error
open dict

type depth = int
type escEnv = (string, depth * bool ref) Dict

fun travVar env d s nl =
  case s of
    SimpleVar s =>
      (case dictSearch env s of
         SOME (dd, b) => if d > dd then b:=true else ()
       | NONE => raiseError nl ("Unknown variable '"^s^"'."))
  | FieldVar(v, s) => travVar env d v nl
  | SubscriptVar(v, e) => (travVar env d v nl; travExp env d e)

and travExp env d s =
  case s of
    VarExp(v, nl) => travVar env d v nl
  | CallExp({args, ...}, nl) => travExp env d (SeqExp(args, nl))
  | OpExp({left, right, ...}, _) =>
      (travExp env d left; travExp env d right)
  | RecordExp({fields, ...}, _) =>
      List.app (travExp env d o #2) fields
  | SeqExp(le, _) =>
      List.app (travExp env d) le
  | AssignExp({var, exp}, nl) =>
      (travVar env d var nl; travExp env d exp)
  | IfExp({test, then', else'=NONE}, _) =>
      (travExp env d test; travExp env d then')
  | IfExp({test, then', else'=SOME e}, _) =>
      (travExp env d test; travExp env d then'; travExp env d e)
  | WhileExp({test, body}, _) =>
      (travExp env d test; travExp env d body)
  | ForExp({var, escape, lo, hi, body}, _) =>
      let val env' = dictRInsert env var (d, escape)
      in travExp env d lo;
        travExp env d hi;
        travExp env' d body
      end
  | LetExp({decs, body}, _) =>
      travExp (travDecs env d decs) d body
  | ArrayExp({typ, size, init}, _) => travExp env d init
  | _ => ()

and travDecs env d [] = env
  | travDecs env d (s::t) =
    let fun aux s =
      case s of
        FunctionDec l =>
          let fun aux ({params, body, ...}, _) =
            let fun insertVar (var, e) =
                  dictRInsert e (#name var) (d + 1, #escape var)
                val env' = foldr insertVar env params
            in travExp env' (d + 1) body end
          in List.app aux l; env end
      | VarDec({name, escape, init, ...}, _) =>
          (travExp env d init; dictRInsert env name (d, escape))
      | TypeDec _ => env
    in travDecs (aux s) d t end

in

fun findEscape prog = travExp (dictNewStr()) 0 prog

end
end
