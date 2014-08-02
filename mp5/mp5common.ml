(* File: mp5common.ml *)

(* expressions for MicroML *)

type const =
     BoolConst of bool        (* for true and false *)
   | IntConst of int          (* 0,1,2, ... *)
   | RealConst of float       (* 2.1, 3.0, 5.975, ... *)
   | StringConst of string    (* "a", "hi there", ... *)
   | NilConst                 (* [ ] *)
   | UnitConst                (* ( ) *)

let string_of_const = function
   BoolConst b     -> (if b then "true" else "false")
 | IntConst i      -> string_of_int i
 | RealConst f     -> ((string_of_float f)^(if ceil f = floor f then ("0") else ("")))
 | StringConst s   -> ("\""^ (String.escaped s)^ "\"")
 | NilConst        -> "[]"
 | UnitConst       -> "()"

type mon_op =
     IntNegOp      (* integer negation *)
   | HdOp          (* hd *)
   | TlOp          (* tl *)
   | FstOp         (* fst *)
   | SndOp         (* snd *)

let string_of_mon_op = function
     IntNegOp -> "~"
   | HdOp -> "hd"
   | TlOp -> "tl"
   | FstOp -> "fst"
   | SndOp -> "snd"

type bin_op =
     IntPlusOp        (* _ + _ *)
   | IntMinusOp       (* _ - _ *)
   | IntTimesOp       (* _ * _ *)
   | IntDivOp         (* _ / _ *)
   | RealPlusOp       (* _ +. _ *)
   | RealMinusOp      (* _ -. _ *)
   | RealTimesOp      (* _ *. _ *)
   | RealDivOp        (* _ /. _ *)
   | ConcatOp         (* _ ^ _ *)
   | ConsOp           (* _ :: _ *)
   | CommaOp          (* _ , _ *)
   | EqOp             (* _ = _ *)
   | GreaterOp        (* _ > _ *)

let string_of_bin_op = function
     IntPlusOp  -> "+"
   | IntMinusOp -> "-"
   | IntTimesOp -> "*"
   | IntDivOp -> "/"
   | RealPlusOp -> "+."
   | RealMinusOp -> "-."
   | RealTimesOp -> "*."
   | RealDivOp -> "/."
   | ConcatOp -> "^"
   | ConsOp -> "::"
   | CommaOp -> ","
   | EqOp  -> "="
   | GreaterOp -> ">"

type dec =  (* This type will be expanded in later MPs *)
     Val of string * exp             (* val x = exp *)
   | Rec of string * string * exp    (* val rec f x = exp *)
   | Seq of dec * dec                (* dec1 dec2 *)
(*   | Local of dec * dec *)

and exp =  (* Exceptions will be added in later MPs *)
   | VarExp of string                    (* variables *)
   | ConstExp of const                   (* constants *)
   | MonOpAppExp of mon_op * exp         (* % exp1
                    where % is a builtin monadic operator *) 
   | BinOpAppExp of bin_op * exp * exp   (* exp1 % exp2                                  
                    where % is a builtin binary operator *)
   | IfExp of exp * exp * exp            (* if exp1 then exp2 else exp3 *)
   | AppExp of exp * exp                 (* exp1 exp2 *) 
   | FnExp of string * exp               (* fn x => x *)
   | LetExp of dec * exp                 (* let dec in exp end *)

let rec string_of_exp = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp e1) ^
                 " then " ^ (string_of_exp e2) ^
                 " else " ^ (string_of_exp e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b with CommaOp -> ("(" ^ (paren_string_of_exp e1) ^ (string_of_bin_op b) ^
                              (paren_string_of_exp e2) ^ ")")
    | _ -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b)
            ^ " " ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FnExp (x,e) ->  ("fn " ^ x ^ " => " ^ (string_of_exp e))
 | LetExp (d,e2) -> ("let " ^ (string_of_dec d) ^ " in " ^ (string_of_exp e2) ^ " end")
(*
 | RaiseExp e -> "raise " ^ (string_of_exp e)
 | Handle (e,{first = exc_match) ^ ( rest = match_list}) ->
                 "handle " ^ (string_of_exp e) ^  " with " ^ (
                 string_of_exc_match exc_match) ^ (
                 List.iter (fun m -> (" | " ^ (string_of_exc_match m))) match_list
*)

and string_of_dec = function
   Val (s, e) ->  ("val "^ s ^" = " ^ (string_of_exp e))
 | Rec (fname,argname,fn) -> ("val rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp fn))
 | Seq (d1,d2) -> (string_of_dec d1) ^ "\n" ^ string_of_dec d2
(*
   Val (Some s, e) -> ("val "^ s ^" = " ^ (string_of_exp e))
 | Val (None, e) -> ("val _ = " ^ (string_of_exp e))
 | Rec {first = (name, fn) ; rest = fs} ->
     ("val rec " ^ name ^ " = " ^ (string_of_exp fn) ^
      (List.iter (fun (name, fn) -> ("\n and " ^ name ^ " = " ^ (string_of_exp fn)))
      fs))
 | Local (d1,d2) -> "local "^ (string_of_dec d1) ^
   "\nin " ^ (string_of_dec d2) ^ " end"
*)

and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e

(*
and string_of_exc_match (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^
    " => " ^
    (string_of_exp e)
*)

let print_exp exp = print_string (string_of_exp exp)
let print_dec dec = print_string (string_of_dec dec)


type cps_cont = 
   External
 | ContVarCPS of int                                  (* _ki *)
 | ContCPS of string * exp_cps                        (* FUN x -> exp_cps *)

and exp_cps =
   VarCPS of cps_cont * string                        (* k x *)
 | ConstCPS of cps_cont * const                       (* k c *)
 | MonOpAppCPS of cps_cont * mon_op * string          (* k (% x) *)
 | BinOpAppCPS of cps_cont * bin_op * string * string (* k (x % y) *)
 | IfCPS of string * exp_cps * exp_cps             (* IF x THEN exp_cps1 ELSE exp_cps2 *)
 | AppCPS of cps_cont * string * string               
 | FnCPS of cps_cont * string * int * exp_cps          (* k (FN x _ki -> [[exp]]_ki) *)
 | FixCPS of cps_cont * string * string * int * exp_cps 
                                                      (* FIX f. FN x _ki -> [[exp]]_ki *)

let string_of_cont_var ky = "_k" ^ (string_of_int ky)
let rec string_of_exp_cps ext_cps =
    match ext_cps with VarCPS (k,x) -> paren_string_of_cps_cont k ^ " " ^ x
    | ConstCPS (k,c) -> paren_string_of_cps_cont k ^ " " ^ string_of_const c
    | MonOpAppCPS (k,m,r) ->
       paren_string_of_cps_cont k ^ "(" ^  string_of_mon_op m ^ " " ^ r ^ ")"
    | BinOpAppCPS (k,b,r,s) ->
       paren_string_of_cps_cont k ^ "(" ^ r ^ " " ^ string_of_bin_op b ^ " " ^ s ^")"
    | IfCPS (b,e1,e2) -> "IF "^b^" THEN "^ string_of_exp_cps e1 ^" ELSE "^string_of_exp_cps e2
    | AppCPS (k,r,s) -> r ^ " " ^ s ^ " " ^ paren_string_of_cps_cont k ^ ")" 
    | FnCPS (k, x, kx, e) ->  (paren_string_of_cps_cont k) ^ "(" ^ (string_of_fnk x kx e) ^ ")"
    | FixCPS (k,f,x,kx,e) -> paren_string_of_cps_cont k ^
                            "(FIX "^ f ^". " ^ (string_of_fnk x kx e) ^ ")"
and string_of_fnk x kx e =
    "FN " ^ x ^ " " ^ (string_of_cont_var kx) ^ " -> " ^ string_of_exp_cps e
and
   string_of_cps_cont k =
    match k with External -> "<external>"
    | ContVarCPS kx -> string_of_cont_var kx
    | ContCPS (x, e) -> "FUN " ^ x ^ " -> " ^ string_of_exp_cps e
and
  paren_string_of_cps_cont k =
   match k with ContCPS _ -> "(" ^ string_of_cps_cont k ^ ")"
   | _ -> string_of_cps_cont k

let rec freeVarsInExpCPS cont =
    match cont with VarCPS (k, x) -> x :: freeVarsInContCPS k
    | ConstCPS (k, c) -> freeVarsInContCPS k
    | MonOpAppCPS (k,m,s) -> s :: freeVarsInContCPS k
    | BinOpAppCPS (k,b,r,s) -> r :: s :: freeVarsInContCPS k
    | IfCPS (r,e1,e2) -> r :: ((freeVarsInExpCPS e1) @ (freeVarsInExpCPS e2))
    | AppCPS (k,x1,x2) -> x1::x2::(freeVarsInContCPS k)
    | FnCPS (k,x,c,e) ->
      (freeVarsInContCPS k) @ (List.filter (fun y -> not (x = y)) (freeVarsInExpCPS e))
    | FixCPS (k,f,x,kx,e) -> (freeVarsInContCPS k) @ 
      (List.filter (fun y -> not ((x = y) || (f = y))) (freeVarsInExpCPS e)) 
and
   freeVarsInContCPS k =
   match k with External -> []
   | ContVarCPS c -> []
   | ContCPS (k, e) -> (freeVarsInExpCPS e)


(* Fresh Name stuff *)

let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n)

let freshFor lst = 
    let rec fresh_ n = 
        if List.mem (int_to_string n) lst
           then fresh_ (n+1)
        else int_to_string n
    in fresh_ 1

(* End Fresh name stuff *)

(* Normalization functions for equality testing *)
let rec exp_cps_norm_aux var_subst cvar_subst free_vars next_cvar exp_cps =
 match exp_cps with
   VarCPS (kappa, var_name) ->
    let (kappa', new_cvar) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
    in (VarCPS (kappa',var_subst var_name), new_cvar) 
 | ConstCPS (kappa, const) ->
   let (kappa', new_cvar) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
    in (ConstCPS (kappa', const), new_cvar) 
 | MonOpAppCPS (kappa, binop, argvar) ->
   let (kappa', new_cvar) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
    in (MonOpAppCPS (kappa, binop, var_subst argvar), new_cvar) 
 | BinOpAppCPS (kappa, binop, fstvar, sndvar) ->
   let (kappa', new_cvar) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
    in (BinOpAppCPS (kappa', binop, var_subst fstvar, var_subst sndvar),
        new_cvar)
 | IfCPS (boolvar, thenexpcps, elseexpcps) ->
   let (thenexpcps', new_cvar1) =
       exp_cps_norm_aux var_subst cvar_subst free_vars next_cvar thenexpcps
   in
   let (elseexpcps', new_cvar2) =
       exp_cps_norm_aux var_subst cvar_subst free_vars new_cvar1 elseexpcps
   in (IfCPS (var_subst boolvar, thenexpcps', elseexpcps'), new_cvar2)
 | AppCPS (kappa, funvar, argvar) ->
   let (kappa', new_cvar) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
    in (AppCPS (kappa', var_subst funvar, var_subst argvar), new_cvar)
 | FnCPS (kappa, x, ki, bodyexpcps) -> 
   let (kappa', new_cvar1) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
   in
   let new_cvar_subst index =
       if index = ki then new_cvar1 else cvar_subst index
   in
   (*we will keep x; it is "user supplied"*)
   let new_var_subst name = if name = x then name else var_subst name 
   in
   let (bodyexpcps', new_cvar2) =
       exp_cps_norm_aux
        new_var_subst new_cvar_subst (x::free_vars) (new_cvar1+1) bodyexpcps
   in (FnCPS (kappa', x, new_cvar1, bodyexpcps'), new_cvar2)
 | FixCPS (kappa, f, x, ki, bodyexpcps) ->
   let (kappa', new_cvar1) = 
        cps_cont_norm_aux 
         var_subst cvar_subst free_vars next_cvar kappa
   in
   let new_cvar_subst index =
       if index = ki then new_cvar1 else cvar_subst index
   in
   (*we will keep f and x; they are "user supplied"*)
   let new_var_subst name =
       if name = x || name = f then name else var_subst name 
   in
   let (bodyexpcps', new_cvar2) =
       exp_cps_norm_aux
        new_var_subst new_cvar_subst (f::x::free_vars) (new_cvar1+1) bodyexpcps
   in (FixCPS (kappa', f, x, new_cvar1, bodyexpcps'), new_cvar2)

and cps_cont_norm_aux var_subst cvar_subst free_vars next_cvar kappa =
 match kappa with
   External -> (External, next_cvar)
 | ContVarCPS ki -> (ContVarCPS (cvar_subst ki), next_cvar)
 | ContCPS (y, bodyexpcps) ->
   let newy = freshFor free_vars
   in
   let new_var_subst name =
       if name = y then newy else var_subst name 
   in
   let (bodyexpcps', new_cvar) =
       exp_cps_norm_aux
        new_var_subst cvar_subst (newy::free_vars) next_cvar bodyexpcps
   in (ContCPS (newy, bodyexpcps'), new_cvar)

let exp_cps_normalize ec fv = exp_cps_norm_aux (fun s -> s) (fun n -> n) fv 0 ec

let rec mergesort list =
let split l =
  let rec split_aux l left right = 
    match l,left,right with
    | ([] | [_]),_,_ -> (List.rev left),right
    | (_::_::t),_,h::right_t -> split_aux t (h::left) right_t
    | _ -> assert false
  in
  split_aux l [] l
  in
let rec merge l1 l2 =
  match l1,l2 with
  | [],l | l,[] -> l
  | h1::t1,h2::t2 ->
    if h1 < h2  then h1::(merge t1 l2)
    else if h2 < h1 then h2::(merge l1 t2)
    else merge t1 l2
  in match list with
  | ([] | [_]) as l -> l
  | l ->  let left,right = split l in 
          merge (mergesort left) (mergesort right)

