open Mp3common
open List

(* Problem 1 *)
let rec even_count l = match l with []-> 0 | (x::xs) -> if(x mod 2 == 0) then 1+even_count xs else even_count xs;;

(* Problem 2 *)
let rec split_sum l f = match l with [] -> (0,0) | (x::xs) -> (let (a,b) = split_sum xs f in if(f x) then (a+x,b) else (a, b+x));;

(* Problem 3 *)
let rec merge l1 l2 = match l1 with [] -> l2 | (x::xs) -> match l2 with [] -> l1 | (y::ys) -> (x::y::merge xs ys);;

let inclusive a b c = if(c > a && c < b) then 1 else 0;;

(* Problem 4 *)
let rec range_count l m n =  match l with [] -> 0 | (x::xs) -> inclusive m n x + range_count xs m n;;

let max x y = if(x>y) then true else false;;

let rec max_index_aux l curr max index f = match l with []->index | x::xs ->if(f max x) then max_index_aux xs (curr+1) max index f else max_index_aux xs (curr+1) x curr f;;

(* Problem 5 *)
let rec max_index l = match l with [] -> [] | x::xs -> [(max_index_aux l 0 x 0 max)];;

let unique_to_neighbor_aux l x = match l with []->true | h::t -> if(x == h)then false else true;;


(* Problem 6 *)
let rec unique_to_neighbor l = match l with []->[] | x::xs -> if(unique_to_neighbor_aux xs x) then x::unique_to_neighbor xs else unique_to_neighbor xs;; 

(* Problem 7 *)
let remove_if_base = [] (* You may need to change this *)
let remove_if_rec p x r = if(p x) then r else x::r;;

(* Problem 8 *)
let split_sum_base = (0,0) (* You may need to change this *)
let split_sum_rec f x r = let (a,b) = r in (if(f x) then (x+a,b) else (a,b+x));;

(* Problem 9 *)
let all_positive_base = true(* You may need to change this *)
let all_positive_rec r x = if (x>0) then r else false;;

(* Problem 10 *)
let range_count_base = 0(* You may need to change this *)
let range_count_rec m n r x = if(x>m && x < n) then r+1 else r;;

(* Problem 11 *)
let app_all_with fs b l  = List.map(fun f->List.map(f b) l) fs;;

(* Problem 12 *)
let rle l = raise(Failure "Function not implemented yet.")
