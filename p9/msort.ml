open List;;

let rec divide l = match l with
	h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;
	
let rec merge = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
			else h2 :: merge (h1::t1, t2);;
			
let rec msort1 l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l in
		merge (msort1 l1, msort1 l2);;

let randomlist x n=
	let rec aux lista n=
		if n>0 then aux (Random.int(x)::lista) (n-1)
		else lista
	in aux [] n;;		
		
		
let l2 = randomlist 10 10;;

(*Con recursividad non terminal pode que se desborde a pila de recursividad si a lista e moi grande*)

let divide' l =
	let rec aux l t1 t2 = match l with
		h1::h2::t->aux t  (h1::t1) (h2::t2)
		|_->(List.rev_append l t1), t2
	in aux l [] [];;
	
let merge' f (l1,l2) =
	let rec aux l1 l2 acc = match (l1,l2) with
		([],l) | (l,[])-> List.rev_append acc l
		|(h1::t1),(h2::t2)-> 
				if f h1 h2 then aux t1 (h2::t2) (h1::acc)
				else aux (h1::t1) t2 (h2::acc)
			in aux l1 l2 [];;
			
let rec msort2 f l = match l with
	[] | _::[]-> l
	|_-> let l1, l2 = divide' l in 
		merge' f (msort2 f l1, msort2 f l2);;
			

		
