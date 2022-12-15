type 'a g_tree =
  Gt of 'a * 'a g_tree list;;


let rec size = function 
    Gt (_, []) -> 1
    |Gt (r, h::t) -> size h + size (Gt (r, t));;

  
let rec height = function
	Gt(r,[])->1
	|Gt(_, ramas)-> 1 + (List.fold_left (max) 1 (List.map height ramas));;
	
	
let rec leaves = function 
	Gt(r,[])->[r]
	|Gt(_,ramas)-> List.fold_left (@) [] (List.map leaves ramas);;
	

let rec mirror = function
	Gt(r,[])->Gt(r, [])
	|Gt(r,ramas)->Gt(r, List.map mirror (List.rev ramas));;


let rec preorder = function
	Gt(r,[])->[r]
	|Gt(r,ramas)->List.fold_left (@) [r] (List.map preorder ramas);;
	
	
let rec postorder = function
	Gt(r,[])->[r]
	|Gt(r,ramas)->List.fold_right (@) (List.map postorder ramas) [r];;
	



