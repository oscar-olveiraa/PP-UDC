let f n = 
	if n mod 2 = 0 then n / 2 
	else 3 * n + 1;;
	
(*-----------------------------------------------------------*)

let rec orbit n=
	if n = 1 then "1"
	else (string_of_int n) ^ (",") ^ orbit(f n);;

(*-----------------------------------------------------------*)	
	
let rec length n =
	if n = 1  then 0
	else 1 + length (f n);;

(*-----------------------------------------------------------*)
	
let rec top n =
	if (n = 1) then 0
	else max n (top(f n));;

(*-----------------------------------------------------------*)
	
let rec length'n'top n (h,t)=
	if n = 1 then (h,t)
	else length'n'top(f n) (h + 1, max n t);;

let length'n'top n = length'n'top n (0, 1);;

(*-----------------------------------------------------------*)

let rec longest_in h t=
	let m= length h in
	if h = t then h,m
	else if length h >= length t then longest_in h (t-1)
		else longest_in (h+1) t;;
		
let rec highest_in h t=
	let m=top h in
		if h=t then h,m
		else let resto, rt = highest_in (h+1) t in
			if m>=rt then h,m 
			else resto, rt;;
