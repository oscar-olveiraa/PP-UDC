let rec sum_cifras n=
	if n=0 then 0
	else sum_cifras(n/10) + n mod 10;;
	
let rec num_cifras n=
	if n<10 then 1
	else 1+ num_cifras (n/10);;
	

let rec exp10 n=
	if n=0 then 1
	else 10 * exp10 (n-1);;
	
	
let rec reverse n=
	if n<10 then n
	else ((n mod 10)*(exp10 (num_cifras (n/10)))) + reverse(n/10);;
	

let rec palindromo s=
	let l = String.length s in
	let rec comp n=
		n=0 || (s.[l-n]=s.[n-1] && comp (n-1))
		in comp (l/2);;



