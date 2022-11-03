(**Devolve a cabeza da lista**)
let hd l = match l with
	[]->raise (Failure "hd")
	|h::t->h;;(*creo que se pode poñer h::t como h::_*)
	 
(**Devolve todos os elementos menos a cabeza da lista**)	
let tl l = match l with
	[]->raise (Failure "tl")
	|h::t->t;;
	
(**Devolve a lonxitude da lista**)	
let rec length l = match l with
	[]->0
	|h::t-> 1 + length t;;
	
(**Compara o tamaño de duas listas**)
let rec compare_lengths l1 l2 = match l1,l2 with
	([],[])->0
	|([],_)->(-1)
	|(_,[])->1
	|(_::t1,_::t2)->compare_lengths t1 t2;;

(**Devolve o elemento da posicion que lle damos**)	
let rec nth l n = 
	if(n<0) then
		raise(Invalid_argument "nth")
	else let rec nth = function
		([],_)->raise (Failure "nth")
		|(h::t, 0)->h
		|(h::t,n)-> nth(t,n-1) in nth(l,n);;

(**Concatena duas listas**)		
let rec append =function
	[]->(function l2->l2)
	|(h1::t1)->function l2-> h1::(append t1 l2);;
	
(**Devolve o primer elemento que satisfacen a funcion**)
let rec find f l = match l with
	[]->raise (Failure "find")
	|h::t->if f h then h
	else (find f t);;
	
(**Devolve true si todos os elementos satisfacen f, no caso contrario, false**)
let rec for_all f l = match l with
	[]->true
	|(h::t)->(f h) && (for_all f t);;
	
(**Devolve true si polo menos un dos elementos satistace f, si non hai ningun, false**)
let rec exists f l = match l with
	[]->false
	|h::t->if (f h) then true
		else exists f t;;
		
(**Devolve true si o elemento que lle pasamos esta dentro da lista, no caso contrario false**)
let rec mem a l = match l with
	[]->false
	|h::t->if (a=h) then true
		else mem a t;;
		
(**Devolve unha lista dos elementos que satisfacen a lista**)
let rec filter f l = match l with
	[]->[]
	|h::t->if(f h) then h::(filter f t)
		else filter f t;;
		
(**O mesmo que filter**)
let rec find_all f l = match l with
	[]->[]
	|h::t->if(f h) then h::(find_all f t)
		else find_all f t;;
		
(**Devolve un par de listas (l1,l2) polo cal en l1 garda os elementos que satisface f e en l2 o caso contrario**)
let rec partition f l = match l with
	[]->([],[])
	|h::t->let (l1,l2) = partition f l
			in if (f h) then (h::l1,l2)
			else (l1, h::l2);; 		

(**Dos pares de elementos que forman unha lista, devolve duas listas, unha co elemento1 e outra co elemento2**)
let rec split l1 = match l1 with
	[]->([],[])
	|(h1,h2)::t->let  t1,t2 = split t in
			h1::t1,h2::t2;;
			

(**Transforma un par de listas en unha lista de pares->[a1;a2;an],[b1;b2;bn]->[(a1,b1);(a2,b2);(an,bn)]**)
let rec combine l1 l2 = match l1,l2 with
	([],[])->[]
	|(h1::t1,h2::t2)->(h1,h2)::combine t1 t2
	|(_,_)->raise(Invalid_argument "combine");;
	
(**Saca unha lista onde os elementos satisfagan a funcion f**)
let rec init = function n -> function f ->
  if n=1 then
    (f (n-1))::[]
  else
    append (init (n-1) f) ((f (n-1))::[]);;
    
(**Da a lista ao reves**)
let rev l =
	let rec aux lista = function
		[]->lista
		|h::t->aux (h::lista) t
		in aux [] l;;
	
(**Recibe duas listas (l1,l2) e concatenaas invertindo a l1**)
let rec rev_append l1 l2 = match (l1,l2) with
	([],l2)->l2
	|(h::t,l2)-> rev_append t (h::l2);; 
	
(**Concatena as listas de unha lista en unha**)
let rec concat l = match l with
	[]->[]
	|[]::t-> concat t
	|h::[]-> h
	|(h::t)::l->h::(concat(t::l));;
	
(**O mesmo que flatten**)
let rec flatten l = match l with
	[]->[]
	|[]::t-> flatten t
	|h::[]-> h
	|(h::t)::l->h::(flatten(t::l));;

(**Aplica unha funcion aos elementos e devolve a lista modificada**)
let rec map f l = match l with
	[]->[]
	|h::t->f(h)::(map f t);;
	
(**O mesmo que rev pero recursiva terminal**)
let rev_map func l=
	let rec aux func l sol=match l with
		[]->sol
		|h::t->(aux func t((func h)::sol))
		in aux func l[];;
		
(**Aplica a funcion ao elementos de l1 e l2**)
let rec map2 f l1 l2 =
  if (length l1 != length l2)
    then raise (Invalid_argument"map2")
    else if (length l1 == 0)
      then []
      else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;

(**Dado unha funcion, unha variable e unh lista, aplica a funcion a esa lista tendo en conta a variable pasada**)
let rec fold_left f e l = match l with
	[]->e
	|h::t->fold_left f (f e h) t;;
	
(**O mesmo que fold_left pero empeza pola dereita**)
let rec fold_right f l e = match l with
	[]->e
	|h::t->f h (fold_right f t e);;



		
		

