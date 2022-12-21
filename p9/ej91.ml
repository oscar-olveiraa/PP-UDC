open List;;

let to0from n=
	let rec aux l i =
		if i<0 then List.rev l
		else aux (i::l) (i-1)
		in aux [] n;;
		

let fromto m n=
	let rec aux l i =
		if i<m then l
		else aux (i::l) (i-1)
		in aux [] n;;


let incseg l =
	let rec aux l i l2 = match l with
		[]->l2
		|h::t-> aux t (h+i) (List.append l2 [(h+i)])
		in aux l 0 [];; 
				

let remove x l=
	let rec aux i = function
		[]->l
		|h::t-> if x = h then List.rev_append i t
			else aux (h::i) t
			in aux [] l;;
			

let compress l = 
	let rec aux l sol= match l with
		|h1::h2::t->if h1 = h2 then aux (h2::t) sol
			else aux (h2::t) (h1::sol)
		|_->List.append l sol
		in aux (List.rev l) [];;


			



