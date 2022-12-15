
open Bin_tree;;

let rec insert_tree ord r = function
	Empty -> Node (r, Empty, Empty) 
	|Node(a, i, d)-> if ord r a then Node (a, insert_tree ord r i,d)
			else Node (a,i,insert_tree ord r d);;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l);;
