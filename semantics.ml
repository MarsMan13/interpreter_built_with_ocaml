open Syntax
open Domain

(* Assumption: in LETREC, 'f' is not equal to 'x'. *)
(* TODO *)
let rec eval exp env = 
  match exp with
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false 
  | CONST n -> Int n 
  | VAR x -> lookup_env x env
  | ADD (e1, e2) -> 
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (InvalidArithmetic))
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (InvalidArithmetic))
  | MUL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> raise (InvalidArithmetic))
  | EQUAL (e1, e2) ->   (* 정의역: Unit, Int, Bool *)
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match (v1, v2) with
      | (Unit, Unit) -> Bool true
      | (Int n1, Int n2) -> Bool (n1 = n2)
      | (Bool b1, Bool b2) -> Bool (b1 = b2)
      | _ -> raise InvalidComparison
    )
  | LESS (e1, e2) ->    (* 정의역: Int *)
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match (v1, v2) with
      | (Int n1, Int n2) -> Bool (n1 < n2)
      | _ -> raise InvalidComparison
    )
  | IF (e1, e2, e3) ->
    let cond = eval e1 env in
    (match cond with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (InvalidComparison)
    )
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in
    let extended_env = extend_env x v1 env in
    eval e2 extended_env
  | LETREC (f, x, e1, e2) ->
    let extended_env = extend_env f (RecProcedure (f, x, e1, env)) env in
    eval e2 extended_env 
  | PROC (v, e) -> 
    Procedure (v, e, env)
  | CALL (e1, e2) ->
    let proc = eval e1 env in
    let v = eval e2 env in
    (match proc with
      | Procedure (px, pe, penv) ->
        let extended_env = extend_env px v penv in
        eval pe extended_env
      | RecProcedure (rpf, rpx, rpe, rpenv) ->
        let extended_env = extend_env rpf proc (extend_env rpx v rpenv) in
        eval rpe extended_env
      | _ -> raise InvalidCall
    )
    
let run pgm = eval pgm empty_env

(****** TEST CASES ******)
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

let _ = print_value (run p1)
let _ = print_value (run p2)