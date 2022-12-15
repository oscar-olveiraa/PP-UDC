
type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec map_tree f = function
	Empty->Empty
	|Node (r,i,d)-> Node (f r, map_tree f i, map_tree f d);;


let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;
  

let sum t= 
	let sum1 r i d =
		r + i + d
	in fold_tree sum1 0 t;;
	

let prod t= 
	let prod1 r i d =
		r *. i *. d
	in fold_tree prod1 1.0 t;;


let size t = 
	let size1 r i d =
		1 + i + d
	in fold_tree size1 0 t;;


let height t =
	let height1 r i d=
		1 + (max i d)
	in fold_tree height1 0 t;;


let inorder t = 
	let in1 r i d =
	 	i @ [r] @ d
	 in fold_tree in1 [] t;;


let mirror t = 
	let mirror1 r i d =
		Node(r, d , i)
	in fold_tree mirror1 Empty t;;


