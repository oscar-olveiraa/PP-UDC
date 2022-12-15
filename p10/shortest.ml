let get_values (i,j) trees n =
	let rec aux pos sol = if (pos = List.length trees)
	then sol
	else let (a,b) = (List.nth trees pos) 
	in if (a,b)<>(i,j) && (((abs(a-i)<=n && b=j) || (a=i && abs(b-j)<=n)))
		then aux (pos+1) ((a,b)::sol)
		else aux (pos+1) sol
		in aux 0 [];; 
		

let filter_visited v l = 
	let rec aux l1 sol = match l1 with
		[]->sol
		|h::t->if List.mem h v then aux t sol
			else aux t (h::sol)
		in aux l [];;
		

let get_posibilities visitados trees (i,j) n = 
	filter_visited visitados (get_values (i,j) trees n);;  
	

let shortest_tour m n trees salto =
	let rec aux cam =
		if cam = [] then raise Not_found
		else let visited , (x,y) = List.hd cam, List.hd (List.hd cam) in
			if (x,y)=(m,n) then List.rev visited
			else let posibility = get_posibilities visited trees (x,y)salto 											 															                              in let pila2 = (List.map(fun x->x::visited) posibility) @ 
			     (List.tl cam) in aux (List.sort (fun x y->(List.length x)-
			     (List.length y)) pila2) 
			     in aux [[(1,1)]];;
