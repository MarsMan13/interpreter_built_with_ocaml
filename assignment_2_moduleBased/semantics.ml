open Syntax
open Syntax.Exp
open Domain
open Domain.Value

(* Assumption: in LETREC, 'f' is not equal to 'x'. *)
let rec eval (exp: Exp.t) (env: Env.t) =
  match exp with
  | Exp.UNIT -> Value.Unit
  | Exp.TRUE -> Value.Bool true
  | Exp.FALSE -> Value.Bool false
  | Exp.CONST n -> Value.Int n
  | Exp.VAR v -> Env.lookup v env
  | Exp.ADD (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    (Value.add n1 n2)
  | Exp.SUB (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    (Value.sub n1 n2)
  | Exp.MUL (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    (Value.mul n1 n2)
  | Exp.EQUAL (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    (Value.is_equal n1 n2)
  | Exp.LESS (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    (Value.less_than n1 n2)
  | Exp.IF (e1, e2, e3) ->
    let b = eval e1 env in
    (match b with 
    | Value.Bool true -> eval e2 env
    | Value.Bool false -> eval e3 env
    | _ -> raise (InvalidComparison))
  | Exp.LET (x, e1, e2) ->
    let v1 = eval e1 env in
    let extended_env = Env.extend x v1 env in
    eval e2 extended_env
  | Exp.LETREC (f, x, e1, e2) ->
    let extended_env = Env.extend f (Value.RecProcedure(f, x, e1, env)) env in
    eval e2 extended_env
  | Exp.PROC (v, e) ->
    Value.Procedure (v, e, env)
  | Exp.CALL (e1, e2) ->
    let proc = eval e1 env in
    (match proc with
    | Procedure (px, pe, penv) -> 
      let v = eval e2 env in
      let extended_env = Env.extend px v penv in
      eval pe extended_env
    | RecProcedure (pf, px, pe, penv) -> 
      let v = eval e2 env in
      let extended_env = Env.extend pf proc (Env.extend px v penv) in
      eval pe extended_env
    | _ -> raise InvalidCall)

let run pgm = eval pgm Env.empty

let p1 =
  LET ("x", CONST 1,
    LET ("f", PROC ("y", ADD (VAR "x", VAR "y")),
      LET ("x", CONST 2,
        LET ("g", PROC ("y", ADD (VAR "x", VAR "y")),
          ADD (
              CALL (VAR "f", CONST 1),
              CALL (VAR "g", CONST 1)
              )
        )
      )
    )
  )

let p2 =
  LETREC ("double", "x",
    IF (EQUAL (VAR "x", CONST 0),
        CONST 0,
        ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)
    ),
    CALL (VAR "double", CONST 6)
  )

let _ = Value.print (run p1)
let _ = Value.print (run p2)
