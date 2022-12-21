open List;;

let rec qsort1 ord = function
	[] -> []
	| h::t -> let after, before = List.partition (ord h) t in
		qsort1 ord before @ h :: qsort1 ord after;;
		
(**Non e un bo caso para cando o arbol esta desbalanceado**)

let rec qsort2 ord =
	let append1 l1 l2 = List.rev_append (List.rev l1) l2 in function
		[] -> []
		| h::t -> let after, before = List.partition (ord h) t in
			append1 (qsort2 ord before) (h :: qsort2 ord after);;
			
(**Si non usamos @ podemos usar rev_append e rev enton qsort2 ten a ventaxa de que é mais rapido nos casos onte a lista xa esta inicialmente ordenada e ao usar terminalidad podemos ordenar listas mais grandes sin que se sobrepase a pila de recursividad**)


(**A desventaxa de qsort2 é que é mais lento que qsor1 para listas aleatorias, listas ordenadas inversamente. Nestos casos, qsort2 e sobre un 14% mais lento que qsort1 **)
let randomlist x n=
	let rec aux lista n=
		if n>0 then aux (Random.int(x)::lista) (n-1)
		else lista
	in aux [] n;;
	
let primeros n=
	let rec aux n l=
		if n=0 then l
		else aux (n-1) (n::l)
	in aux n [];;

let l1 = randomlist 2000 900000;;
let l2=[];;

let crono f x y =
	let t=Sys.time () in
	let _=f x y in (Sys.time() -. t);;
	
(**Si fago**)
qsort1 (<) l1;;
(*Stack overflow during evaluation (looping recursion?). No caso de l1 funciona obvio*)

qsort2 (<) l1;;
(*Tardou bastante pero consigueuno*)

let tiempo_desor=
	map(crono qsort1 (<)) (map (randomlist 100) [1000;10000;100000]);;
(*[0.0028229999999993538; 0.0588200000000043133; 6.76555199999999246]*)
let tiempo_desor=
	map(crono qsort2 (<)) (map (randomlist 100) [1000;10000;100000]);;
(*[0.0084550000000049863; 0.0710199999999972; 7.8609559999999945]*)

let tiempo_ord=
	map(crono qsort1 (<)) (map (primeros) [1000;10000;100000]);;
(*Killed*)
let tiempo_ord=
	map(crono qsort1 (<)) (map (primeros) [1000;10000]);;
(*val tiempo_ord : float list = [0.0966580000000000217; 6.2062779999999993]*)






