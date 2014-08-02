(*
  micromlint.ml - DO NOT EDIT
*)

open Mp11common
open Student

let print_mappings l = List.fold_right 
(fun n -> fun () -> match n with 
  (Some s, value) -> (print_string ("val "^s^" = ");
                  print_value value; 
                  print_string "\n")
  | (None, value) -> (print_string "val _ = ";
                      print_value value;
                      print_string "\n")) ( l) ()

let rec eval_exp_cps env ecps =
    match one_step_exp_cps_eval env ecps
    with Failed -> (print_string ("This shouldn't happen; it should mean there is a free variable\n of some type without a corresponding value, or a type checking error."); raise (Failure "compiler error"))
    | UncaughtException n ->
      (print_string ("Exception no. "^string_of_int n^" raised.\n"); None)
    | Final v -> Some v
    | Intermediate(env', ecps') -> eval_exp_cps env' ecps'

let mk_binding env x exp_cps =
    match eval_exp_cps (List.map (fun (n,v) -> ValueBinding(n,v)) env) exp_cps
    with None -> ([],[])
    | Some v ->
     (match x with "" -> ([(None,v)], empty_env)
      | _ -> ([(Some x, v)], make_env x v))

let rec eval_dec (dec, env) = 
  match dec
  with Val (x, e) -> mk_binding env x (cps_exp e External EmptyExnContCPS)
  | Rec(f,x,e) ->
    mk_binding env f (cps_dec dec (VarCPS (External, f)) EmptyExnContCPS)
  | Seq (d1, d2) -> 
    (match eval_dec (d1, env) with (b1, m1) ->
      (match eval_dec (d2,sum_env m1 env)
       with (b2, m2) -> (b2 @ b1, sum_env m2 m1)))
  | Local (dec1, dec2) ->
    (match eval_dec (dec1, env)
     with (b1, m1) -> eval_dec (dec2, sum_env m1 env))

let _ =
  print_endline "\nWelcome to Student's MicroML evaluator \n";
  let rec loop (gamma: type_env) mem = 
  try
      let lexbuf = Lexing.from_channel stdin in
        print_string "> "; flush stdout;
        (try
          let dec = Micromlparse.main (fun lb -> match Micromllex.token lb
			             with Micromlparse.EOF -> raise Micromllex.EndInput
				     | r -> r)
                    lexbuf in
          match infer_dec gather_dec_ty_substitution gamma dec  with
             None          ->
              (print_string "\ndoes not type check\n"; loop gamma mem)
           | Some (env_inc ,p) -> (
             match eval_dec (dec, mem) with 
               
               (b, m) -> (print_string "\nresult:\n"; 
                          print_mappings b;
                          loop (sum_env env_inc gamma) (sum_env m mem))
             )
        with Failure s ->
              (print_newline();
               print_newline();
               loop gamma mem)
           | Parsing.Parse_error ->
              (print_string "\ndoes not parse\n";
               loop gamma mem));
  with Micromllex.EndInput -> exit 0
 in loop [] []



