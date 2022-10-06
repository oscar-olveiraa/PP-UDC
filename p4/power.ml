let rec power x n=
	if n=0 then 1
	else x * power x (n-1);;
	



let rec power2 x n=
	if n=0 then 1
	else if (n mod 2)=0 then power2 (x*x) (n/2)
	else x * power2 (x*x) (n/2);;

(*En cuestion de eficiencia é mellor power2 porque faia menos operacións dado que realiza a metade de operacions (n/2) polo que é unha ventaxa con respeto a power xa que fai o doble*)

let rec powerf x n=
	if n=0 then 1.0
	else if (n mod 2)=0 then powerf (x*.x) (n/2)
	else x *. powerf (x*.x) (n/2);;
	
