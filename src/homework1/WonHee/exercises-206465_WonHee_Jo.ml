(*
 *	Name: 조원희(WonHee Jo)
 *	Student ID: 206465
 *)

(* exercise (List-1) *)
let rec last l =
	match l with
	| [] -> None
	| [last] -> Some (last)
	| hd :: tl -> last tl
;;

last [1;2;3];;
last ['1';'2';'3'];;
last [];;

(* exercise (List-2) *)
let rec length l =
	match l with
	| [] -> 0
	| hd :: tl -> 1 + length tl
;;

length [1;2;3];;
length [];;

(* exercise (List-3) *)
let rec is_palindrome l =
	match l with
	| [] -> true
	| hd :: tl ->
		let rec is_hd_equal_last l =
			match l with
			| [] -> true
			| [last] -> hd = last
			| hd :: tl -> is_hd_equal_last tl
		in let rec remove_last l =
			match l with
			| [] -> []
			| [last] -> []
			| hd :: tl -> hd :: remove_last tl
		in if is_hd_equal_last tl then is_palindrome (remove_last tl) else false
;;

is_palindrome ['x'; 'a'; 'm'; 'a'; 'x'];;
is_palindrome ['c'; 'h'; 'o'; 'i'];;
is_palindrome [];;

(* exercise (List-4) *)
type 'a node =
| One of 'a
| Many of 'a node list
;;

let rec flatten nl =
	match nl with
	| [] -> []
	| [One (e)] -> [e]
	| [Many (l)] -> flatten l
	| hd :: tl -> flatten [hd] @ flatten tl
;;

flatten [One 'a'; Many [One 'b'; Many [One 'c'; One 'd']; One 'e']];;

(* exercise (List-5) *)
let run_length l =
	let rec count c e l =
		match l with
		| [] -> [(c, e)]
		| hd :: tl -> if hd = e then count (c + 1) hd tl else (c, e) :: count 1 hd tl
	in match l with
	| [] -> []
	| hd :: tl -> count 1 hd tl
;;

run_length ['a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'c'; 'c'];;

(* exercise (Num-1) *)
let newton_square n =
	let rec loop x y =
		if x = y then y else loop y ((y +. (n /. y)) /. 2.)
	in loop n ((n +. 1.) /. 2.)
;;

newton_square 2.;;

(* exercise (Num-2) *)
let all_primes min max =
	let rec is_prime n i =
		if i > (n / 2) then true else
		if (n mod i) = 0 then false else is_prime n (i + 1)
	in let rec prime_list n l =
		if n < min then l else
		if is_prime n 2 then prime_list (n - 1) (n :: l) else prime_list (n - 1) l
	in prime_list max []
;;

all_primes 2 20;;

(* exercise (Logic-1) *)
type bool_expr =
| Var of string
| Not of bool_expr
| And of bool_expr * bool_expr
| Or  of bool_expr * bool_expr
;;

let rec evaluate sbl be =
	let rec str_to_bool sbl str =
		match sbl with
		| [] -> failwith "str_to_bool"
		| (s, b) :: tl -> if str = s then b else str_to_bool tl str
	in match be with
	| Var (str) -> str_to_bool sbl str
	| Not (be0) -> not (evaluate sbl be0)
	| And (be1, be2) -> evaluate sbl be1 && evaluate sbl be2
	| Or  (be1, be2) -> evaluate sbl be1 || evaluate sbl be2
;;

evaluate
    [("a",true); ("b",false)]
    (And (Or (Var "a", Var "b"), And (Var "a", Var "b")))
;;

(* exercise (BinTree-1) *)
type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree
;;

let max bt =
	let rec m bt mv =
		match bt with
		| Empty -> mv
		| Node (e, btl, btr) ->
			let larger_e_mv = if e > mv then e else mv in
			let max_l = m btl larger_e_mv in
			let max_r = m btr larger_e_mv in
			if max_l > max_r then max_l else max_r
	in match bt with
	| Empty -> failwith "max Empty"
	| Node (e, btl, btr) -> m bt e
;;

max (Node ('a',
    (Node ('b', (Node ('d', Empty, Empty)), (Node ('e', Empty, Empty)))),
    (Node ('c', Empty, (Node ('f', (Node ('g', Empty, Empty)), Empty))))))
;;

(* exercise (BinTree-2) *)
type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec sum bt =
	match bt with
	| Empty -> 0
	| Node (e, btl, btr) -> e + sum btl + sum btr
;;

sum (Node (1, (Node (2, Empty, Empty)), (Node (3, Empty, (Node (4, Empty, Empty))))));;

(* exercise (BinTree-3) *)
type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec tostr bt =
	match bt with
	| Empty -> ""
	| Node (e, Empty, Empty) -> e
	| Node (e, btl, btr) -> e ^ "(" ^ tostr btl ^ "," ^ tostr btr ^ ")"
;;

tostr Empty;;
tostr (Node ("a", Empty, Empty));;
tostr (Node ("a", Empty, (Node ("b", Empty, Empty))));;
tostr (Node ("a", (Node ("b", Empty, Empty)), (Node ("c", Empty, (Node ("d", Empty, Empty))))));;
