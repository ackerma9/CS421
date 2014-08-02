/* Use the expression datatype defined in expressions.ml: */
/* Contributors mrlu2 */
%{
    open Mp9common
(* add any extra code here *)
let apply_binop binop x y = BinOpAppExp(binop, x, y)
let apply_monop monop x = MonOpAppExp( monop, x )
let make_orelse x y = IfExp(x, ConstExp(BoolConst true), y)
let make_andalso x y = IfExp(x, y, ConstExp(BoolConst false))
let apply_not x = IfExp(x, ConstExp(BoolConst false), ConstExp(BoolConst true))

%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> REAL
%token <bool> BOOL
%token <string> STRING IDENT
%token <(int*int)> OPCOM CLCOM
%token NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV CARAT LT GT LEQ GEQ
       EQUALS NEQ PIPE ARROW SEMI DCOLON AT NIL LET LOCAL VAL REC AND END IN
       IF THEN ELSE FUN FN OP MOD RAISE HANDLE WITH NOT ANDALSO ORELSE
       HD TL FST SND
       LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE
       UNIT ERROR EOF

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Mp9common.dec> main

%%

main:    expression SEMI                             { Val("it", $1) }
  | dec SEMI                                    { $1 }

dec:
    atomic_dec                                  { $1 }
  | dec atomic_dec                              { Seq($1, $2) }

atomic_dec:
    VAL simp_bind                               { Val (fst $2, snd $2) }
  | VAL REC IDENT rec_var_name_bind		{ Rec ($3, fst $4, snd $4) }
  | FUN IDENT rec_var_name_bind                 { Rec ($2, fst $3, snd $3) }
  | LOCAL dec IN dec END                        { Local($2,$4) }

simp_bind:
    IDENT EQUALS expression                     { ($1, $3) }
  | UNDERSCORE EQUALS expression                { ("", $3) }

rec_var_name_bind:
  IDENT EQUALS expression			{ ($1, $3)  }
  | IDENT rec_var_name_bind			{ ( $1, FnExp(fst $2, snd $2) ) }

expression:
    orelse_exp HANDLE exp_matches            { let x::ys = $3 in HandleExp ($1, fst(x), snd(x), ys) }
  | orelse_exp                                  { $1 }

/* an orelse_exp can't contain an unbracketed handle */
orelse_exp:
    orelse_exp_no_if_fn_raise ORELSE andalso_exp        { make_orelse $1 $3 }
  | andalso_exp                                            { $1 }

andalso_exp:
    andalso_exp_no_if_fn_raise ANDALSO rel_exp      { make_andalso $1 $3 }
  | rel_exp                                            { $1 }

rel_exp:
    rel_exp_no_if_fn_raise rel cons_exp                { $2 $1 $3 }
  | cons_exp                                           { $1 }

cons_exp:
    add_exp_no_if_fn_raise DCOLON cons_exp             { apply_binop ConsOp $1 $3 }
  | add_exp                                            { $1 }

add_exp:
    add_exp_no_if_fn_raise add mult_exp                { $2 $1 $3 }
  | mult_exp                                           { $1 }

mult_exp:
    mult_exp_no_if_fn_raise mult nonop_exp             { $2 $1 $3 }
  | nonop_exp                                          { $1 }

nonop_exp:
    if_fn_raise_exp                      { $1 }
  | app_exp                                     { $1 }


app_exp:
    atomic_or_monop_exp                           { $1 }
  | monadic_op app_exp	                        { MonOpAppExp($1, $2)}
  | app_exp_no_if_fn_raise_monop nonapp_exp    { AppExp($1,$2) }

app_exp_no_if_fn_raise_monop:
    atomic_expression                           { $1 }
  | app_exp_no_if_fn_raise_monop atomic_or_monop_exp    { AppExp($1,$2) }

atomic_or_monop_exp:
    atomic_expression                           { $1 }
  | monop_expression			{ FnExp( "x", $1 (VarExp "x") ) }

nonapp_exp:
    atomic_or_monop_exp                           { $1 }
  | if_fn_raise_exp                      { $1 }

atomic_expression:
    const                                       { ConstExp $1 }
  | OP op_binop                              { FnExp("x", (FnExp("y", $2 (VarExp "x") (VarExp "y")))) }
  | IDENT                                       { VarExp $1 }
  | parenthesis_expression                            { $1 }
  | list_expression                             { $1 }
  | LET dec IN exp_seq END                      { LetExp($2, $4) }

monop_expression:
    NEG                         { fun x -> apply_monop IntNegOp x }
  | HD                          { fun x -> apply_monop HdOp x}
  | TL                          { fun x -> apply_monop TlOp x}
  | FST                         { fun x -> apply_monop FstOp x}
  | SND                         { fun x -> apply_monop SndOp x }

if_fn_raise_exp:
    IF expression THEN expression ELSE orelse_exp   { IfExp($2, $4, $6) }
  | FN IDENT ARROW orelse_exp                       { FnExp($2, $4) }
  | RAISE orelse_exp                                { RaiseExp $2 }

exp_matches:
    exp_match                                   { [$1] }
  | no_handle_exp_match PIPE exp_matches        { $1::$3 }

exp_match:
    pattern ARROW expression                        { ($1, $3) }

no_handle_exp_match:
    pattern ARROW orelse_exp                          { ($1, $3) }

orelse_exp_no_if_fn_raise:
    orelse_exp_no_if_fn_raise ORELSE andalso_exp_no_if_fn_raise   { make_orelse $1 $3 }
  | andalso_exp_no_if_fn_raise                                       { $1 }

andalso_exp_no_if_fn_raise:
    andalso_exp_no_if_fn_raise ANDALSO rel_exp_no_if_fn_raise { make_andalso $1 $3 }
  | rel_exp_no_if_fn_raise                                       { $1 }

rel_exp_no_if_fn_raise:
    rel_exp_no_if_fn_raise rel cons_exp_no_if_fn_raise    { $2 $1 $3 }
  | cons_exp_no_if_fn_raise                                      { $1 }

cons_exp_no_if_fn_raise:
    add_exp_no_if_fn_raise DCOLON cons_exp_no_if_fn_raise  { apply_binop ConsOp $1 $3 }
  | add_exp_no_if_fn_raise                                        { $1 }

add_exp_no_if_fn_raise:
    add_exp_no_if_fn_raise add mult_exp_no_if_fn_raise          { $2 $1 $3 }
  | mult_exp_no_if_fn_raise                                     { $1 }

mult_exp_no_if_fn_raise:
    mult_exp_no_if_fn_raise mult app_exp_no_if_fn_raise         { $2 $1 $3 }
  | app_exp_no_if_fn_raise                                      { $1 }

app_exp_no_if_fn_raise:
    atomic_or_monop_exp                           { $1 }
  | monadic_op app_exp_no_if_fn_raise           { MonOpAppExp($1, $2)}
  | app_exp_no_if_fn_raise_monop atomic_or_monop_exp    { AppExp($1,$2) }

list_expression:
    LBRAC exp_list RBRAC                   { $2 }

exp_list:
    expression                                  { apply_binop ConsOp $1 (ConstExp NilConst) }
  | expression COMMA exp_list              { apply_binop ConsOp $1 $3 }

parenthesis_expression:
    LPAREN expression RPAREN                    { $2 }
  | LPAREN expression SEMI exp_seq RPAREN       { LetExp(Val("", $2),$4) }
  | LPAREN expression COMMA expression_pair RPAREN     { apply_binop CommaOp $2 $4 }

expression_pair:
    expression                                  { $1 }
  | expression COMMA expression_pair                   { apply_binop CommaOp $1 $3 }

exp_seq:
    expression                                  { $1 }
  | expression SEMI exp_seq                     { LetExp(Val("", $1),$3) }

rel:
    EQUALS          { apply_binop EqOp }
  | GT              { apply_binop GreaterOp }
  | derived_rel     { $1 }

derived_rel:
    LT              { fun x y -> apply_binop GreaterOp y x }
  | LEQ             { fun x y -> make_orelse (apply_binop GreaterOp y x) (apply_binop EqOp x y) }
  | NEQ             { fun x y -> apply_not(apply_binop EqOp x y) }
  | GEQ             { fun x y -> make_orelse (apply_binop GreaterOp x y) (apply_binop EqOp x y) }

op_binop:
    rel             { $1 }
  | add             { $1 }
  | mult            { $1 }
  | DCOLON          { fun x y -> apply_binop ConsOp x y}
  | COMMA           { fun x y -> apply_binop CommaOp x y}

add:
    PLUS            { fun x y -> apply_binop IntPlusOp x y}
  | MINUS           { fun x y -> apply_binop IntMinusOp x y}
  | DPLUS           { fun x y -> apply_binop RealPlusOp x y}
  | DMINUS          { fun x y -> apply_binop RealMinusOp x y}
  | CARAT           { fun x y -> apply_binop ConcatOp x y}

mult:
    TIMES           { fun x y -> apply_binop IntTimesOp x y}
  | DIV             { fun x y -> apply_binop IntDivOp x y}
  | DTIMES          { fun x y -> apply_binop RealTimesOp x y}
  | DDIV            { fun x y -> apply_binop RealDivOp x y}

const:
    INT                         { IntConst $1 }
  | BOOL            { BoolConst $1 }
  | REAL            { RealConst $1 }
  | STRING          { StringConst $1 }
  | LBRAC RBRAC         { NilConst }
  | NIL                         { NilConst }
  | UNIT            { UnitConst }
  | LPAREN RPAREN               { UnitConst }


monadic_op:
   NEG                         { IntNegOp }
  | HD                          { HdOp }
  | TL                          { TlOp }
  | FST                         { FstOp }
  | SND                         { SndOp }

pattern:
    UNDERSCORE  { None }
  | INT         { Some $1 }

