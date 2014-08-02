(* CS421 - Spring 2014
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common

(* Problem 1 *)
let title = "MP1 -- Basic OCaml";;

(* Problem 2 *)
let e = 2.71828;;

(* Problem 3 *)
let firstFun n = 2*n+5;;

(* Problem 4 *)
let divide_e_by x = (2.71828)/.x;;

(* Problem 5 *)
let diff_square_9 m = match m with 3 -> 0 | 2 -> 9-4 | 1 -> 9-1 | 0 -> 9 | -1 -> 9-1 | -2 -> 9-4 | -3 -> 0 | x -> 9-x*x;;

(* Problem 6 *)
let dist_double s n = (print_string s; print_string ", guess its double or nothing!\n"; 2*n);;

(* Problem 7 *)
let swizzle (w,x,y,z) = match (w,x,y,z) with (w,x,y,z) -> (x,y,w,x);;

(* Problem 8 *)
let left_right_compose f g = fun x -> f(g(f(x)));;

