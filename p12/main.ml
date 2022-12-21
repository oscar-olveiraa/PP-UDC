
open Parsing;;
open Lexing;;

open Context;;
open Lib;;
open Arith;;
open Command;;
open Parser;;
open Lexer;;

let rec loop ctx =
  print_string ">> ";

  let cmd = s token (from_string (read_line ())) in
	try
		loop(Command.run ctx cmd)
	with 
		Context.No_binding v->
			print_string ("Variable "^v^" not defined\n\n");
			loop ctx
		|Lib.Function_not_defined f->
			print_string("Function "^f^" not defined\n\n");
			loop ctx
		|Lexer.Lexical_error->
			print_string("Lexical error\n\n");
			loop ctx
		|Command.End_of_program ->
			print_string("\n")
		|Parsing.Parse_error|_->
			print_string ("Syntax error\n\n");
			loop ctx;;
			

let _ = print_endline "Floating point calculator..." in
let _ = loop empty_context in
print_endline "... bye!!!";;

