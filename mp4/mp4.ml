open Mp4common

let report x =
print_string "Result: ";
print_int x;
print_newline();;      

(* Problem 1 *)
let addk n m k = k(n+m);;
 
let subk n m k = k(n-m);;
  
let mulk n m k = k(n*m);;
  
let posk x k = k(x>0);;
  
let float_addk a b k = k(a+.b);;
  
let float_divk a b k = k(a/.b);;
  
let catk str1 str2 k = k(str1^str2);;
 
let consk e l k = k([e]@l);;
  
let eqk x y k = k(x=y);;
  
let geqk x y k = k(x>=y);;

(* Problem 2 *)
let poly x k = mulk x x (fun xsquare -> mulk xsquare x (fun xcube -> addk xcube x (fun xcubeplusx -> addk xcubeplusx 1 k)));;

(* Problem 3 *)
let composek f g x k = f x (fun x2 -> g x2 k);;

(* Problem 4 *)
let rec inverse_square_series n = match n with 0 -> 0.
| x -> float_addk (mulk x x (fun xsquare -> float_divk (float 1) (float xsquare) (fun b -> b))) (inverse_square_series (subk n 1 (fun a -> a))) (fun c -> c);;
  
let rec inverse_square_seriesk n k = eqk n 0 (fun b -> if b then k 0.
else subk n 1 
(fun s -> inverse_square_seriesk s 
(fun q -> float_addk q
(mulk n n 
(fun ssquare -> float_divk (float 1) (float ssquare) 
(fun c -> c)))k)));;


(* Problem 5 *)
let rec rev_map f l = match l with [] -> [] | (h::t) -> consk (f(h)) (rev_map f t) (fun a -> a);;

let rec rev_mapk f l k = match l with [] -> k [] 
|(h::t) -> rev_mapk f t 
(fun s -> f h (fun fh -> consk fh s k));;


(* Problem 6 *)
let rec partition l p = match l with [] -> ([],[]) | (h::t) -> (let (a,b) = partition t p in if(p h) then ([h]@a,b) else (a,[h]@b));;
  
let rec partitionk l p k =  match l with [] -> k ([],[]) 
| (h::t) -> p h 
(fun fh -> partitionk t p 
(fun s -> match s with (x,y) -> if (fh) 
then consk h x (fun ex -> k (ex,y))
else consk h y (fun ey -> k (x,ey))));;

(* Problem 7 *)
let rec findk l p normalk exceptionk = match l with [] -> exceptionk () | (h::t) -> p h (fun b -> if b then normalk h else findk t p normalk exceptionk);;

(* Problem 8 *)
let rec appk l x k =
  raise(Failure "Function not implemented yet.")

