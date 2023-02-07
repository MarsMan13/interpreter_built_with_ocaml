open Syntax

exception UnboundVariable of Var.t
exception InvalidArithmetic
exception InvalidComparison
exception InvalidBranching
exception InvalidCall

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Procedure of Var.t * Exp.t * Env.t
    | RecProcedure of Var.t * Var.t * Exp.t * Env.t

  val print: t -> unit

  val add: t -> t -> t

  val sub: t -> t -> t

  val mul: t -> t -> t

  val is_equal: t -> t -> t

  val less_than: t -> t -> t
end = struct
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Procedure of Var.t * Exp.t * Env.t
    | RecProcedure of Var.t * Var.t * Exp.t * Env.t

  let print v =
    match v with
    | Unit -> ()
    | Int i -> print_endline (string_of_int i)
    | Bool b -> print_endline (string_of_bool b)
    | _ -> print_endline "Unprintable value"

  let add v1 v2 = 
    match v1, v2 with
    | Int n1, Int n2 -> Int (n1 + n2)
    | _ -> raise InvalidArithmetic

  let sub v1 v2 = 
    match v1, v2 with
    | Int n1, Int n2 -> Int (n1 - n2)
    | _ -> raise InvalidArithmetic

  let mul v1 v2 = 
    match v1, v2 with
    | Int n1, Int n2 -> Int (n1 * n2)
    | _ -> raise InvalidArithmetic

  let is_equal v1 v2 = 
    match v1, v2 with
    | Unit, Unit -> Bool true
    | Int n1, Int n2 -> Bool (n1 = n2)
    | Bool b1, Bool b2 -> Bool (b1 = b2)
    | _ -> raise InvalidComparison

  let less_than v1 v2 = 
    match v1, v2 with
    | Int n1, Int n2 -> Bool (n1 < n2)
    | _ -> raise InvalidComparison
  
end
and Env : sig
    type t
    val empty: t
    val extend : Var.t -> Value.t -> t -> t
    val lookup : Var.t -> t -> Value.t
end = struct
    module VarMap = Map.Make(Var)
    type t = Value.t VarMap.t
    let empty = VarMap.empty
    let extend x v env = VarMap.add x v env
    let lookup x env =
      try
        VarMap.find x env
      with Not_found -> raise(Failure ("KeyError " ^ x))
end
