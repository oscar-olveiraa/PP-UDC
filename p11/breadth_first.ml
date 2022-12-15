
open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t gt = 
	let rec br1 a l = match a with
		Gt(r, [])->List.rev (r::l)
		|Gt(r,(Gt(y,t2))::t1) -> br1 (Gt(y,List.rev_append (List.rev t1) t2)) 
		(r::l) in br1 gt [];;
	

let t2 = 
	let rec aux t n =
		if n<=0 then t
		else aux (Gt(n, [t])) (n-1)
	in aux (Gt(300000, [])) 300000;;

