(*
  micromlint.ml - DO NOT EDIT
*)

open Mp10common
open Student

let print_mappings l = List.fold_right 
(fun n -> fun () -> match n with 
  (Some s, value) -> (print_string ("val "^s^" = ");
                  print_value value; 
                  print_string "\n")
  | (None, value) -> (print_string "val _ = ";
                      print_value value;
                      print_string "\n")) ( l) ()

let _ =
  print_endline "\nWelcome to Student's MicroML evaluator \n";
  let rec loop (gamma: type_env) (mem:value env) = 
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



