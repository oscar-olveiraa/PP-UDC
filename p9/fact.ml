let rec fact = function
	0 -> 1 
  | n -> n * fact (n - 1);;
  
try
	print_endline (string_of_int (fact (int_of_string Sys.argv.(1))))
with
    Failure _ 
  | Stack_overflow 
  | Invalid_argument _ -> print_endline "argumento invalido"
  
  
(**let mensaje = 
	if (Array.length Sys.argv)=2
	then try string_of_int (fact (int_of_string(Sys.argv.(1)))) with 
		|_->"fact : argumento invalido"
	else ("fact:numero de argumentos invalido")
	in print_endline mensaje;;**)
	


