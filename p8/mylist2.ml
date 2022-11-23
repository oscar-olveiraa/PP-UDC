let hd l = match l with
	[]->raise (Failure "hd")
	|h::t->h;;

	
let tl l = match l with
	[]->raise (Failure "tl")
	|h::t->t;;

	
let length l =
	let rec aux l acum = match l with
		[]->acum
		|h::t -> aux t (acum + 1) in
		aux l 0;; 
		

let rec compare_lengths l1 l2 = match l1,l2 with
	([],[])->0
	|([],_)->(-1)
	|(_,[])->1
	|(_::t1,_::t2)->compare_lengths t1 t2;;
	

let rec nth l n = 
	if(n<0) then
		raise(Invalid_argument "nth")
	else let rec nth = function
		([],_)->raise (Failure "nth")
		|(h::t, 0)->h
		|(h::t,n)-> nth(t,n-1) in nth(l,n);;
		
let rec append =function
	[]->(function l2->l2)
	|(h1::t1)->function l2-> h1::(append t1 l2);;
	

let rec find f l = match l with
	[]->raise (Failure "find")
	|h::t->if f h then h
	else (find f t);;
	
let for_all f l =
  let rec aux f l compr = match l with
    [] -> compr
    | h::t -> (aux f t ((f h)&&(compr)))
  in aux f l true;;


let exists f l =
	let rec aux f l compr = match l with
	 []->compr
	 |h::t->(aux f t ((f h) || (compr)))
	 in aux f l true;;
	 
let rec mem a l = match l with
	[]->false
	|h::t->if (a=h) then true
		else mem a t;;

let rev l =
	let rec aux v l = match l with
		[]->v
		|h::t->aux (h::v) t
	in aux [] l;;
			
let filter f l =
	let rec aux f l l2 = match l with
		[]->l2
		|h::t-> if f h then
			aux f t (h::l2)
			else aux f t l2
		in aux f (rev l) [];;
		
let find_all = filter;;

let partition f l =
	let rec aux f l l1 l2 = match l with
		[]->(rev l1, rev l2)
		|h::t-> if f h 
			then aux f t (h::l1) l2
			else aux f t l1 (h::l2)
		in aux f l [] [];;
		
let rec split l1 = match l1 with
	[]->([],[])
	|(h1,h2)::t->let  t1,t2 = split t in
			h1::t1,h2::t2;;
			
let rec combine l1 l2 = match l1,l2 with
	([],[])->[]
	|(h1::t1,h2::t2)->(h1,h2)::combine t1 t2
	|(_,_)->raise(Invalid_argument "combine");;
	
let rec init n f =
	if n < 0 then raise (Invalid_argument "len<0")
	else let rec aux (i,l) =
		if i = n then l 
		else aux (i+1, f i::l)
	in rev(aux(0,[]));; 
		
let rev_append l1 l2=
	let rec aux l1 v = match l1 with
		[]->v
		|h::t->aux t (h::v)
	in aux l1 l2;;
	
let rec concat l = match l with
	[]->[]
	|[]::t-> concat t
	|h::[]-> h
	|(h::t)::l->h::(concat(t::l));;
	
let rec flatten l = match l with
	[]->[]
	|[]::t-> flatten t
	|h::[]-> h
	|(h::t)::l->h::(flatten(t::l));;
	
let rec map f l = match l with
	[]->[]
	|h::t->f(h)::(map f t);;
	
let rev_map func l=
	let rec aux func l sol=match l with
		[]->sol
		|h::t->(aux func t((func h)::sol))
		in aux func l [];;
		
let rec map2 f l1 l2 =
  if (length l1 != length l2)
    then raise (Invalid_argument"map2")
    else if (length l1 == 0)
      then []
      else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;
      
let rec fold_left f e l = match l with
	[]->e
	|h::t->fold_left f (f e h) t;;
	
let rec fold_right f l e = match l with
	[]->e
	|h::t->f h (fold_right f t e);;
		

		
  
 
 	
	
