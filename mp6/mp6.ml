open Mp6common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x ->
	(match lookup_env gamma x 
	  with None->None
	| Some tau' ->
	 (match unify [(tau,freshInstance tau')]
	  with None->None
		| Some sigma -> Some(Proof([],judgement,sigma)))
    | MonOpAppExp (monop, e1) ->
	let tau' = monop_signature monop in
	let tau1 = fresh() in
	(match gather_exp_ty_substitution gamma e1 tau1
	with None->None
	| Some(pf, sigma)->
		(match unify[(monoTy_lif_subst gamma (mk_fun_ty tau1 tau), freshInstance tau')]
		with None->None
		| Some subst-> Some(Proof([pf],judgement),subst_compose subst gamma)))
    | _ -> raise (Failure "Not implemented yet")

and gather_dec_ty_substitution gamma dec = 
    raise (Failure "Not implemented yet")
