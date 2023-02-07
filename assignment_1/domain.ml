open Syntax

exception UnboundVariable of var
exception InvalidArithmetic
exception InvalidComparison
exception InvalidBranching  (* <- ISSUE: What is this for? *)
exception InvalidCall

module VarMap = Map.Make(struct type t = var let compare = Stdlib.compare end)

(* Val = void + boolean + int + procedure + rec_procedure *)
type value =
  | Unit
  | Int of int
  | Bool of bool
  | Procedure of var * exp * env
  | RecProcedure of var * var * exp * env
and env = value VarMap.t


let print_value v =
  match v with
  | Unit -> ()
  | Int i -> print_endline (string_of_int i)
  | Bool b -> print_endline (string_of_bool b)
  | _ -> print_endline "Unprintable value"

let empty_env = VarMap.empty

(* TODO *)
(* 환경 확장 : 기존 env에 x->v를 추가한 환경 *)
let extend_env x v env = VarMap.add x v env 

(* TODO *)
(* 환경에서 x변수의 값을 찾기 : env에서 x의 대응되는 값 *)
let lookup_env x env = 
  try
    VarMap.find x env
  with Not_found -> raise(Failure ("KeyError" ^ x))