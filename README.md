# Project Introduction
This project was given by Prof. JS Choi.  
After studying PL, I made a toy interpreter by using Ocaml.

# What is this project for?
- To study Ocaml
- To understand functional programming
- To understand PL and interpreting process
- To get ready for Static Analysis

# Interpreter Spec
## Syntax
<b>Exp -></b>  
  | UNIT  
  | TRUE  
  | FALSE  
  | CONST of int  
  | VAR of var  
  | ADD of exp * exp  
  | SUB of exp * exp  
  | MUL of exp * exp  
  | EQUAL of exp * exp  
  | LESS of exp * exp  
  | IF of exp * exp * exp  
  | LET of var * exp * exp  
  | LETREC of var * var * exp * exp  
  | PROC of var * exp  
  | CALL of exp * exp  

## Semantics
<b>(skip)</b>

## Variable Set
<b>Var = string</b>

## Value Set
<b>Value =  </b>
Unit + Int of int + Bool of bool + Procedure of var * exp * env + RecProcedure of var * var * exp * env

## Environment
<b>Env = var -> val</b>
