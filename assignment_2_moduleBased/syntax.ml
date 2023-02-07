module Var = struct
  type t = string
  let compare = Stdlib.compare
end

module Exp = struct
type t =
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of Var.t
  | ADD of t * t
  | SUB of t * t
  | MUL of t * t
  | EQUAL of t * t
  | LESS of t * t
  | IF of t * t * t
  | LET of Var.t * t * t
  | LETREC of Var.t * Var.t * t * t
  | PROC of Var.t * t
  | CALL of t * t
end
