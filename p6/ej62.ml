(*************************Ejercicio 1**************************)

let curry = function f -> function a -> function b -> f (a,b);;
let uncurry f (a,b) = f a b;;

uncurry (+);;
(*- : int * int -> int = <fun>*)

let sum = (uncurry (+));;
(*val sum : int * int -> int = <fun>*)

(*sum 1;;
Error xa que o tipo de uncurry pasa por parametro un int * int*)

sum(2,1);;
(*- : int = 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*val g : int -> int -> int = <fun>*)

(*g(2,5);;
Error xa que g ten como argumento int*)

let h = g 2;;
(*val h : int -> int = <fun>*)

h 1, h 2, h 3;;
(*- : int * int * int = (7, 10, 13*)

(*************************Ejercicio 2**************************)

let comp f g x = f (g x);;

let f = let square x = x * x in comp square ((+) 1);;
(*val f : int -> int = <fun>*)

f 1, f 2, f 3;;
(*- : int * int * int = (4, 9, 16)*)

(*************************Ejercicio 3**************************)

let i a = a;; 

let j = fst;;

let k =  snd;;

let l a = [a];;




