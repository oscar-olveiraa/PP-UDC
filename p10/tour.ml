open List;;

 
(**List das casillas con arbol**)
let trees =
	[(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
	(3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
	(6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
	(8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
	(12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
	(15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
	(16,16)];;
	
(**Obtemos todos os posibles posicions da ardilla a partir da posicion (i,j)**)
let get_values (i,j) trees n =
	let rec aux pos sol = if (pos = List.length trees)
	then sol
	else let (a,b) = (List.nth trees pos) 
	in if (a,b)<>(i,j) && (((abs(a-i)<=n && b=j) || (a=i && abs(b-j)<=n)))
		then aux (pos+1) ((a,b)::sol)
		else aux (pos+1) sol
		in aux 0 [];; 
		
get_values (4,15) trees 3;;	

		
(**Eliminamos xa os arboles que pasou**)		
let filter_visited v l = 
	let rec aux l1 sol = match l1 with
		[]->sol
		|h::t->if List.mem h v then aux t sol
			else aux t (h::sol)
		in aux l [];;
filter_visited [(1,1);(1,3)] (get_values (4,3) trees 3);;
			
(**Generar todos os movimientos a partir dunha posicion eliminando os visitados**)
let get_posibilities visitados trees (i,j) n = 
	filter_visited visitados (get_values (i,j) trees n);;  

	
let tour m n trees k =
	let rec aux acum=
	if acum = [] then raise Not_found (*non hai camiño*)
	else let v,(x,y) = List.hd acum, List.hd (List.hd acum)
	     in if (x,y) = (m,n) then List.rev v
		else 
	 	   let pb = get_posibilities v trees (x,y) k
		   in let acum2 = (List.map (fun x->x :: v) pb) @ (List.tl acum)
		    	in aux acum2 
		    		in aux [[(1,1)]];;

