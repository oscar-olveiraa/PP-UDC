
open Context;;
open Lib;;

type arith_oper =
    Opp;;

type arith_bi_oper =
    Sum | Sub | Prod | Div | Mod | Pow;;

type arith =
    Float of float
  | Var of string
  | Arith_op of arith_oper * arith
  | Arith_bi_op of arith_bi_oper * arith * arith
  | Fun_call of string * arith;;

let rec eval ctx = function
    Float f ->
      f

  | Var name ->
      get_binding ctx name

  | Arith_op (Opp, x)->
  	0.0 -. eval ctx x  	
  	
  | Arith_bi_op (o,x,y)-> (match o with
  		Sum -> eval ctx x +. eval ctx y
  		|Sub -> eval ctx x -. eval ctx y
  		|Prod -> eval ctx x *. eval ctx y
  		|Div -> eval ctx x /. eval ctx y
  		|Mod -> mod_float (eval ctx x) (eval ctx y)
  		|Pow->eval ctx x ** eval ctx y) 
  	
  | Fun_call (o,x)->
	(get_function o) (eval ctx x);;

