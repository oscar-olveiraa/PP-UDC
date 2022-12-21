
exception Function_not_defined of string;;

let funs ()= 
	print_string ("abs : valor absouluto\n");
	print_string ("sqrt : raiz cuadrada\n");
	print_string ("exp : exponencial\n");
	print_string("ln : logaritmo natural\n");
	print_string("round : redondeo\n");
	print_newline();;

let round x =
	if x-.(floor x) > 0.5 then ceil x
	else floor x;;

let get_function = function
	"sqrt" ->(sqrt)
	|"exp"->(exp)
	|"ln"->(log)
	|"round"->(round)
	|x->raise (Function_not_defined x);; 
	

