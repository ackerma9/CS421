open Mp10common;;

type compVal = GT| EQ| LT

let const_to_val c = 
	match c with
    BoolConst b   -> BoolVal b
  | IntConst i    -> IntVal i
  | RealConst f   -> RealVal f
  | StringConst s -> StringVal s
  | NilConst      -> ListVal []
  | UnitConst     -> UnitVal

let monOpApply op v = 
	match (op, v) with
  | (IntNegOp, IntVal n)         -> IntVal (- n)
 (* Part 2, Problem 16 *)
  | (HdOp, ListVal [])         -> Exn 0
  | (HdOp, ListVal (h::t))    -> h
 (* Part 2, Problem 16 *)
  | (TlOp, ListVal [])         -> Exn 0
  | (TlOp, ListVal (h::t))    -> ListVal t
  | (FstOp, PairVal (v1, v2))  -> v1
  | (SndOp, PairVal (v1, v2))  -> v2
  | (PrintStringOp, StringVal s) -> (print_string s; UnitVal)
  | (_ , _ )                   -> failwith "monOpApply: bad input"

let gencompare a b =
  if a < b then LT
  else if a > b then GT
  else EQ

let rec compareVal v1 v2 =
   match (v1, v2) with
    (UnitVal, UnitVal) -> EQ
  | (BoolVal b1, BoolVal b2) -> gencompare b1 b2
  | (IntVal n1, IntVal n2) -> gencompare n1 n2
  | (RealVal r1, RealVal r2) -> gencompare r1 r2
  | (StringVal s1, StringVal s2) -> gencompare s1 s2
  | (PairVal (u1,u2), PairVal (v1,v2)) -> paircompare (u1,u2) (v1,v2)
  | (ListVal l1, ListVal l2) -> listcompare l1 l2
  | (ClosureVal _, _) -> raise (Failure "Cannot compare funtions")
  | (_, ClosureVal _) -> raise (Failure "Cannot compare funtions")
  | (RecVarVal _, _) -> raise (Failure "Cannot compare funtions")
  | (_, RecVarVal _) -> raise (Failure "Cannot compare funtions")
  | (_, _) -> raise (Failure "Type checking error")

and paircompare (u1, u2) (v1, v2) =
     (match compareVal u1 v1 with EQ -> compareVal u2 v2
      | r -> r)

and listcompare l1 l2 =
   match l1 with
     [] -> (match l2 with [] -> EQ | _ -> LT)
   | (x::xs) -> (match l2 with (y::ys) ->
                   (match compareVal x y with EQ -> listcompare xs ys
                     | r -> r)
                  | [] -> GT)




let binOpApply binop v1 v2 = 
	match (binop, v1, v2) with
    (IntPlusOp, IntVal n1, IntVal n2)      -> IntVal (n1 + n2)
  | (IntMinusOp, IntVal n1, IntVal n2)     -> IntVal (n1 - n2)
  | (IntTimesOp, IntVal n1, IntVal n2)     -> IntVal (n1 * n2)
  | (IntDivOp, IntVal n1, IntVal n2)       -> 
    if n2 = 0 then Exn 0 else IntVal (n1 / n2)
  | (RealPlusOp, RealVal n1, RealVal n2)   -> RealVal (n1 +. n2)
  | (RealMinusOp, RealVal n1, RealVal n2)  -> RealVal (n1 -. n2)
  | (RealTimesOp, RealVal n1, RealVal n2)  -> RealVal (n1 *. n2)
  | (RealDivOp, RealVal n1, RealVal n2)    ->
    if n2 = 0.0 then Exn 0 else RealVal (n1 /. n2)
  | (ConcatOp, StringVal s1, StringVal s2) -> StringVal (s1 ^ s2)
  | (ConsOp, v, ListVal l)                 -> ListVal (v::l)
  | (CommaOp, u1, u2)                      -> PairVal (u1,u2)
  | (EqOp, u1, u2)                         -> BoolVal (match compareVal u1 u2 with EQ -> true
                                                        | _ -> false)
  | (GreaterOp, u1, u2)                    -> BoolVal (match compareVal u1 u2 with GT -> true
                                                        | _ -> false)
  | (_, Exn _, _ )                         -> v1
  | (_, _, Exn _ )                         -> v2
  | (_, _, _)                              -> raise (Failure "typechecking error")

let rec eval_exp (exp, m) = 
	match exp with ConstExp t -> const_to_val t
	| VarExp x   -> (
     match lookup_env m x with
     	Some (RecVarVal(f,y,e', m'))
        	-> ClosureVal (y, e',ins_env m x (RecVarVal(f,y,e', m')))
        | Some v -> v
     | None -> failwith "eval_exp: variable not declared"
     )
	| FnExp(x, e) -> ClosureVal (x, e, m)
	| AppExp (e1, e2) ->
    (match eval_exp (e1, m) with
    	Exn i -> Exn i 
    | ClosureVal (x, e', m') ->
       (match eval_exp (e2, m)
       	with Exn i -> Exn i
       	| v2 -> eval_exp (e', ins_env m' x v2))
     | _ -> failwith "eval_exp: bad first argument in application"
 	)
 	| MonOpAppExp(mon_op, e) ->
    (match eval_exp (e, m) with
    	Exn i -> Exn i
    	| v -> monOpApply mon_op v )
    | BinOpAppExp(bin_op, e1, e2) ->
    	(match eval_exp (e1, m) with
    		Exn i -> Exn i
    		| v1 ->
       			(match eval_exp (e2, m) with
       				Exn i -> Exn i
       				| v2 -> binOpApply bin_op v1 v2))
    | IfExp(e1, e2, e3) -> (
     	match eval_exp (e1, m) with
       		BoolVal true  -> eval_exp(e2, m)
     		| BoolVal false -> eval_exp(e3, m)
     		| Exn i         -> Exn i
     		| _             -> failwith "eval_exp: bad arguments for IfExp")
    | RaiseExp e -> (
     	match eval_exp (e, m) with
       		Exn i    -> Exn i
     		| IntVal n -> Exn n
     		| _        -> failwith "eval_exp: raise called on non-int"
    )
    | HandleExp (e1, n1,e2,hdl) -> (
     	match eval_exp (e1, m) with
       		Exn j -> (
         		try let e = snd(List.find (function (Some n,_) -> j=n | (None,_) -> true)
                                 ((n1,e2):: hdl))
           		in eval_exp (e, m)
       		with Not_found -> Exn j 
      		)
    	| v     -> v)

let rec eval_dec (dec, m) =
	match dec with
		Val("", e) ->  ([(None, eval_exp (e, m))], empty_env)
  		| Val(x, e) -> (
    		match eval_exp (e, m) with
    			Exn i -> ([(None, Exn i)],empty_env)
    			    | v     -> ([(Some x, v)], make_env x v)
    	)
    	| Seq (d1, d2) ->
    		(match eval_dec (d1, m) with (b1, m1) ->
    			(match b1 with ((None, Exn i):: _) -> (b1, m1)
    				| _ -> (match eval_dec (d2,sum_env m1 m) with (b2, m2) -> (b2 @ b1, sum_env m2 m1)
        )))
    	| Local (dec1, dec2) ->
    		(match eval_dec (dec1, m) with (b1, m') ->
       			(match b1 with ((None, Exn i) :: _) ->
       				([(None, Exn i)], empty_env)
       				| _ -> eval_dec (dec2, sum_env m' m)
       	))
    	| Rec(f,x,e) -> ([(Some f,RecVarVal (f,x, e, m))],[(f,RecVarVal (f,x, e, m))])
