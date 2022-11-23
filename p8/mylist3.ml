(**ELimina a primer aparicion dun elemento da lista**)
let rec remove a l = match l with
	[]->[]
	|h::t-> if (a == h) then t
		else h :: (remove a t);;

(**ELimina todas as aparicions dun elemento da lista**)		
let rec remove_all a l = match l with
	[]->[]
	|h::t-> if (a == h) then (remove_all a t)
		else h :: (remove_all a t);;
	
(**Dados duas listas, elimina de l1 os elementos que hai en l2**)		
let rec ldif l1 l2 = match (l1,l2) with
	|(l1,[])->l1
	|([],l2)->[]
	|(h1::t1,h2::t2)-> ldif (remove_all h2 l1) t2;;
	
(**Calcula o produto cartesiano de duas listas**)	
let rec lprod l1 l2=match (l1,l2) with
	([],_)|(_,[])->[]
	|(h::[],h2::t2)->(h,h2)::(lprod [h] t2)
	|(h1::t1,l2)->(lprod[h1]l2)@(lprod t1 l2);;

(**Dunha lista principal l, devolve un par de listas onde en l1 estan os elementos das posicions impares de l e en l2 os elementos das posicions pares de l**)		
let rec divide = function
	h1::h2::t-> let l1,l2 = divide t in h1::l1,h2::l2
	|l->l, [];;
	
	

