let rec powmod m b e = 
	if e=0 
		then 1
	else if e=1 
		then b mod m
	else if e mod 2 = 0
		then (powmod m b (e/2)) * (powmod m b (e/2)) mod m
	else
		(powmod m b (e/2))* (powmod m b ((e/2) +1)) mod m;;
		 
		 
	
