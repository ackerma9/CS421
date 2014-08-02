type bool_exp = Var of string
              | True
              | False
              | Not of bool_exp
              | And of bool_exp * bool_exp
              | Or of bool_exp * bool_exp
;;
(*
We represent the boolean constants true, and false by Ture and False, respectively.
We represent a variable of name y by Var y
If phi represents a formula, we represent the negation of phi, by Not phi
If phi and psi represent formulae, we represent
 * the conjunction of phi and psi by And (phi,psi)
 * the disjunction of phi and psi by Or (phi, psi)
*)

(* bool_exp_eval will evaluate a boolean formuala e in an environment
env by recursion over the structure of e: *)
let rec bool_exp_eval e env =
 match e with
  (* variable: look it's name up in the environment *)
  | Var s -> env s
  (* boolean constants: go to what they represent *)
  | True  -> true
  | False -> false

  (* other constructors: recursively evaluate the subformulae, then
  translate the meaning to the corresponding operation in OCaml. *)

  (* unary boolean operations : *)
  | Not e1 ->
      let b1 = bool_exp_eval e1 env in
        not b1
  (* binary boolean operations *)
  | And (e1, e2) ->
      let b1 = bool_exp_eval e1 env in
      let b2 = bool_exp_eval e2 env in
        b1 && b2
  | Or (e1, e2) ->
      let b1 = bool_exp_eval e1 env in
      let b2 = bool_exp_eval e2 env in
        b1 || b2
;;

bool_exp_eval
  (Or (And (Var "a", Var "b"), Not True))
  (fun s -> match s with "a" -> true | "b" -> false)
;;

