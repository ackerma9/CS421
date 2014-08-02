(* File: mp5-skeleton.ml *)

open Mp5common

(* Problem 1 *)
let rec import_list lst = match lst with [] -> ConstExp NilConst
  | (x, y)::t -> BinOpAppExp(ConsOp, BinOpAppExp(CommaOp, ConstExp(StringConst x), ConstExp (IntConst y)), import_list t);;

(* Problem 2 *)
let list_all = Rec("list_all","p",
FnExp("xs",IfExp(BinOpAppExp(EqOp, VarExp("xs"), ConstExp NilConst),
ConstExp(BoolConst true),
IfExp(AppExp(VarExp "p", MonOpAppExp(HdOp, VarExp("xs"))),
IfExp(AppExp(AppExp(VarExp("list_all"),VarExp("p")),
MonOpAppExp(TlOp, VarExp("xs"))),
ConstExp(BoolConst true), ConstExp(BoolConst false)),
ConstExp(BoolConst false)))));;

(* Problem 3 *)
let rec cal_max_exp_height exp = match exp with VarExp(v)-> 1
  | ConstExp(c)-> 1
  | MonOpAppExp(m,e) -> 1+cal_max_exp_height(e) 
  | BinOpAppExp(b,e1,e2)-> 1+(max (cal_max_exp_height(e1)) (cal_max_exp_height(e2)))
  | IfExp(e1,e2,e3) -> 1+(max (max (cal_max_exp_height(e1))(cal_max_exp_height(e2))) (cal_max_exp_height(e3)))
  | AppExp(e1,e2) -> 1+(max (cal_max_exp_height(e1)) (cal_max_exp_height(e2)))
  | FnExp(f,e) -> 1+cal_max_exp_height(e)
  | LetExp(d,e) -> 1+cal_max_exp_height(e)

and cal_max_dec_height dec = match dec with 
Val(s,e) -> 1+cal_max_exp_height(e)
| Rec(f,x,e) -> 1+cal_max_exp_height(e)
| Seq(d1,d2) -> 1+(max (cal_max_dec_height(d1)) (cal_max_dec_height(d2)));;


(* Problem 4 *)
let rec freeVarsInExp = function
  ConstExp(c) -> []
  | VarExp(s) -> [s]
  | IfExp(e1,e2,e3) -> (freeVarsInExp e2)@(freeVarsInExp e3)
  | FnExp(x,e) -> let z = freeVarsInExp e in List.filter (fun y -> (y<>x)) z
  | BinOpAppExp(b,e1,e2) -> (freeVarsInExp e1)@(freeVarsInExp e2)
  | MonOpAppExp(m,e) -> freeVarsInExp e
  | AppExp(e1,e2) -> (freeVarsInExp e1)@(freeVarsInExp e2)

and freeAndBindingVarsInDec dec = function
   Val(s,e) -> (freeVarsInExp (FnExp(s,e)),[s])
  |Rec(f,x,e) -> (freeVarsInExp(FnExp(x,FnExp(f,e))),[f]);;

(* Problem 5 *)
let rec cps_exp e k kx =  raise (Failure "Not implemented yet.")

and cps_dec dec ecps kx =  raise (Failure "Not implemented yet.")
