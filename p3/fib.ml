let rec fib n = 
    if n <= 1 then n 
    else fib(n-1) + fib(n-2);;

let rec lista n =
    if n = 0
    then "0"
    else lista(n-1)^"\n"^string_of_int(fib(n));;	

let rec print = 
    if (Array.length Sys.argv)=2
    then (lista (int_of_string(Sys.argv.(1))))
    else ("o numero de argumentos ten que ser 2") in
    print_endline print;;
