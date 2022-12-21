
open Context;;
open Arith;;

exception End_of_program;;

type command =
    Eval of arith
  | Var_def of string * arith
  | Quit;;


let rec run ctx = function
    Eval e ->
      let f = eval ctx e in
      let _ = print_endline ((string_of_float f)^"\n") in ctx

  | Var_def (x,e)-> let valor = (eval ctx e) in
  		let _=print_string (x^"="^string_of_float valor^"\n\n")
  		in add_binding ctx x valor
  		
  |Quit -> raise (End_of_program);;

