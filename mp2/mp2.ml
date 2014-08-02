(* CS421 - Spring 2014
 * MP2 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let dist (x1, y1) (x2, y2) = sqrt((x2-.x1)**2.0 +. (y2-.y1)**2.0);;

(*Problem 2*)
let rec fibo_num n = match n with 0->0|1->1|x->fibo_num(x-1)+fibo_num(x-2);;

(*Problem 3*)
let rec fibo_sum n = match n with 0->0|1->1|x->fibo_num(x)+fibo_sum(x-1);;

(*Problem 4*)
let reverse_triple_to_list (a, b, c) = (c::b::a::[]);;

(*Problem 5*)
let rec sum l = match l with []->0|(x::xs)->x+sum(xs);;

(*Problem 6*)
let rec min l default  =  match l with []->default|(x::xs)-> if default == 0 then min xs x else if x < default then min xs x else min xs default;;

(*Problem 7*)
let rec is_sorted_ascend l = match l with []->true|(x::[])->true|(x1::x2::xs)->if(x1<x2) then is_sorted_ascend (x2::xs) else false;;

(*Problem 8*)
let rec zip l1 l2 = match l1 with [] -> [] | (x1::x1s) -> match l2 with [] -> [] | (x2::x2s) -> [(x1,x2)]@zip x1s x2s;;

let rec fold_right f a list = match list with | [] -> a | x::xs -> f x (fold_right f a xs);;

(*Problem 9*)
let rec unzip l = fold_right (fun (x,y) (xlst, ylst)->(x::xlst,y::ylst)) ([],[]) l;;

let rec sum_odd i lst = match lst with [] -> 0 | x::xs -> if i == 1 then x + sum_odd 0 xs else sum_odd 1 xs;;

(*Problem 10*)
let rec add_odd_pos l = sum_odd 1 l;;

(*Problem 11*)
let rec insert n l = match l with [] -> [n] | x::xs -> if n > x then x::insert n xs else n::l;;


(*Problem 12*)
let rec primes n = raise(Failure "Function not implemented yet.")
