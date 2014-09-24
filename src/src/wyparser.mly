%{

open Printf
open Lexing

(* Create an astnd for the currently-parsed node *)
let astnd thing start_pos end_pos = 
  let start_pos = Parsing.rhs_start_pos start_pos in
  let end_pos = Parsing.rhs_end_pos end_pos in
  { Ast.prov = Global.Prov.Root( start_pos, end_pos ) ;
    Ast.data = thing ;
    Ast.info = Ast.dummyinfo; }
%}

%token EOL HASH CODE EOF 
%token <string> CODE_LINE 
%token <char> OTHER_CHAR
%token <string> CPREPROCESS

%token LBRACKET RBRACKET 
%token LBRACE RBRACE
%token LPAREN RPAREN
%token SEMI PIPE COLON

/* -- TODO */
%token TUNIT TWIRE UNITV EMP UNION TNAT TBOOL TSTRING TPROC
%token TRUE FALSE
%token SUBEQ AND TOP NOT
%token PAR SEC
%token SINGL PS FIX LAM MATCH WITH BEGIN END MEND LET 
%token IN AT WIRE PLUSPLUS PLUS GT WAPP WAPS WFOLD BIGARR WCOPY FROM AS CAST
%token ARRAY SELECT UPDATE LEFT_ARR OF
%token TSH MAKESH COMBSH
%token FOR TO DO DONE WHILE 
%token PRINT SYSOP SUBSET

%token DOT 
%token COMMA 
%token ARROW MINUS MULT DIV
%token EQUALS
%token IF THEN ELSE

%token <string> VAR
%token <string> CONS
%token <string> PRINC
%token <string> FIELD
%token <int>    NUMIDX
%token <int>    NUM
%token <string> STRLIT

/* See ast.ml */

%start file
%start env
%start value
%start place

%type <Ast.var_nd> var
%type <Ast.cons_nd> cons
%type <Ast.field_nd> field
%type <Ast.file_nd> file
%type <Ast.defn_nd> defn
%type <Ast.value_nd> value
%type <Ast.place_nd> place
%type <Ast.refine_nd> refine
%type <Ast.eff_nd> eff
%type <Ast.typ_nd> typ
%type <Ast.lam_nd> lam
%type <Ast.pat_nd> pat
%type <Ast.expr_nd> expr
%type <Ast.app_nd> app
%type <(Ast.var_nd * Ast.value_nd) list> env
%%

file:
| defn SEMI file { astnd (Ast.F_defn($1,$3)) 1 3 }
| expr           { astnd (Ast.F_body $1) 1 1 }
| SEMI file      { $2 }
;

defn:
| var pats EQUALS expr  { astnd (Ast.D_fundefn($1,$2,$4)) 1 4 }
| var COLON typ         { astnd (Ast.D_funtyp($1, $3)) 1 3 }
;
pats:
| pat { [ $1 ] }
| pat pats { $1 :: $2 }
;

var: VAR { astnd(Ast.Var ($1)) 1 1 }
;

cons: CONS { astnd(Ast.Cons ($1)) 1 1 }
;

princ: PRINC { astnd( $1 ) 1 1 }
;

field:
| FIELD  { astnd(Ast.Field ($1)) 1 1 }

value:
| value_ UNION value_ { astnd( Ast.V_ps_union($1, $3) ) 1 3 }
| cons values         { astnd (Ast.V_inj( $1, $2 )) 1 2 }
| value_              { $1 }
;
value_:
| STRLIT                 { astnd (Ast.V_string $1) 1 1 }
| princ                  { astnd (Ast.V_princ $1) 1 1 }
| cons                   { astnd (Ast.V_inj( $1, [] )) 1 2 }
| var                    { astnd (Ast.V_var $1) 1 1 }
| UNITV                  { astnd (Ast.V_unit ) 1 1 }
| TRUE                   { astnd( Ast.V_bool true) 1 1 }
| FALSE                  { astnd( Ast.V_bool false) 1 1 }
| NUM                    { astnd (Ast.V_nat $1 ) 1 1 }
| EMP                    { astnd( Ast.V_emp ) 1 1 }
| LBRACE rowvalue RBRACE { astnd( Ast.V_row $2 ) 1 3 }
| LBRACE values_c RBRACE { astnd( Ast.V_ps_lit $2 ) 1 3 } 
| value__                { $1 }
;
value__:
| LPAREN value RPAREN    { astnd (Ast.V_paren $2) 1 3 }
;
values:
| value_        { [ $1 ] }
| value_ values { $1 :: $2 }
;
values_c:
| value                { [ $1 ] }
| value COMMA values_c { $1 :: $3 }
;
rowvalue:
| field COLON value                { [ ($1, $3) ] }
| field COLON value COMMA rowvalue { ($1, $3) :: $5 }
;

place:
| TOP                      { astnd( Ast.Pl_top ) 1 1 }
| mode LPAREN value RPAREN { astnd( Ast.Pl_ps( $1, $3 )) 1 4 }
;
mode:
| PAR { astnd( Ast.Par ) 1 1 }
| SEC { astnd( Ast.Sec ) 1 1 }
;

refine:
| refine_ AND refine  { astnd( Ast.R_conj( $1, $3 )) 1 3 }
| NOT refine_         { astnd( Ast.R_not( $2 )) 1 2 }
| refine_             { $1 }
;
refine_:
| TRUE                 { astnd( Ast.R_true) 1 1 }
| SINGL                { astnd( Ast.R_singl) 1 1 }
| SUBEQ value          { astnd( Ast.R_subeq $2) 1 2 }
| EQUALS value         { astnd( Ast.R_eq $2) 1 2 }
| LPAREN refine RPAREN { $2 }
;

eff:
| eff_ COMMA eff    { astnd( Ast.Ef_cat( $1, $3 )) 1 3 }
| eff_              { $1 }
;
eff_:
| EMP               { astnd( Ast.Ef_emp ) 1 1 }
| place             { astnd( Ast.Ef_place $1) 1 1 }
| LPAREN eff RPAREN { $2 }
;

typ:
/* TODO: fix shift-reduce conflicts:
/*| typ value                             { astnd(Ast.T_appv( $1, $2 )) 1 2 }
| typ typ                               { astnd(Ast.T_appt( $1, $2 )) 1 2 }*/
| arrowtyp                              { $1 }
| typ_                                  { $1 }
;
arrowtyp:
| typ_ ARROW typ                                  { astnd( Ast.T_arr(None,$1,(astnd Ast.Ef_emp 2 2),$3) ) 1 3 }
| typ_ MINUS eff ARROW typ                        { astnd( Ast.T_arr(None,$1,$3,$5) ) 1 5 }
| LPAREN var COLON typ RPAREN ARROW typ           { astnd( Ast.T_arr(Some $2,$4,(astnd Ast.Ef_emp 6 6),$7) ) 1 7 }
| LPAREN var COLON typ RPAREN MINUS eff ARROW typ { astnd( Ast.T_arr(Some $2,$4,$7,$9) ) 1 9 }
;
typ_:
/*| var                                  { astnd(Ast.T_var( $1 )) 1 1 }*/
| TPROC                                  { astnd( Ast.T_proc ) 1 1 }
| TSTRING                                { astnd( Ast.T_string ) 1 1 }
| TUNIT                                  { astnd( Ast.T_unit ) 1 1 }
| TNAT                                   { astnd( Ast.T_nat ) 1 1 }
| TBOOL                                  { astnd( Ast.T_bool ) 1 1 }
| LBRACKET sumtyp RBRACKET               { astnd( Ast.T_sum $2 ) 1 3 }
| LBRACE rowtyp RBRACE                   { astnd(Ast.T_row $2) 1 3}
| PS LBRACE refine RBRACE                { astnd(Ast.T_ps $3) 1 4 }
| LPAREN typ RPAREN                      { $2 }
| TWIRE value typ_                       { astnd(Ast.T_wire( $2, $3 )) 1 3 }
| TSH value typ_                         { astnd(Ast.T_sh( $2, $3 )) 1 3 }
| ARRAY typ_                             { astnd(Ast.T_array( $2 )) 1 2 }
;
sumtyp:
| cons typs             { [ ($1, $2) ] }
| cons typs PIPE sumtyp { ($1, $2) :: $4 }
;
typs:
| typ      { [ $1 ] }
| typ typs {  $1 :: $2 }
;
      
rowtyp:
| field COLON typ              { [ ( $1 , $3 ) ] }
| field COLON typ COMMA rowtyp { ( $1 , $3 ) :: $5 }
;

lam:
| FIX var COLON typ DOT pat DOT expr { astnd( Ast.L_fix($2, $4, $6, $8) ) 1 8 }
| LAM pat DOT expr         { astnd( Ast.L_lam($2, $4) ) 1 4 }
;

pat:
| var                          { astnd( Ast.P_var $1 ) 1 1 }
| LPAREN pat COLON typ RPAREN  { astnd( Ast.P_cast( $2, $4 )) 1 4 }
| LBRACE rowpat RBRACE         { astnd( Ast.P_row $2 ) 1 3 }
;
rowpat:
| field COLON pat              { [ ( $1, $3 ) ] }
| field COLON pat COMMA rowpat { ( $1, $3 ) :: $5 }
;

expr:
| LET pat EQUALS expr IN expr                    { astnd( Ast.E_let($2, None, $4, $6)) 1 6 }
| LET pat AT place EQUALS expr IN expr           { astnd( Ast.E_let($2, Some $4, $6, $8)) 1 8 }
| expr_ PLUSPLUS expr_                           { astnd( Ast.E_wcat ($1, $3) ) 1 3 }
| WIRE expr COLON expr_                          { astnd( Ast.E_wire ($2, $4) ) 1 4 }
| app                                            { astnd (Ast.E_app $1) 1 1 }
| WCOPY AS expr_ FROM expr_                         { astnd( Ast.E_wcopy ($3, $5) ) 1 3 }
| expr_                                          { $1 }
| expr_natops                                    { $1 }

| CAST expr typ                                  { astnd ( Ast.E_cast($2, $3) ) 1 3 }
;
natop:
| PLUS    { astnd Ast.Natop_plus 1 1 }
| MINUS   { astnd Ast.Natop_sub 1 1 }
| MULT    { astnd Ast.Natop_mult 1 1 }
| DIV     { astnd Ast.Natop_div 1 1 }
| GT      { astnd Ast.Natop_gt 1 1 }
| EQUALS  { astnd Ast.Natop_equals 1 1 }
;
expr_natops:
| expr_ natop expr_                              { astnd( Ast.E_natop($2, $1, $3)) 1 3 }
;
expr_:
| PRINT expr                                        { astnd( Ast.E_print($2) ) 1 2 }
| SYSOP var values                                  { astnd( Ast.E_sysop($2, None, $3)) 1 3 }
| SYSOP var typ values                              { astnd( Ast.E_sysop($2, Some $3, $4)) 1 4 }
| MAKESH expr                                       { astnd( Ast.E_sh($2) ) 1 2 }
| COMBSH expr                                       { astnd( Ast.E_comb($2) ) 1 2 }
| ARRAY LBRACKET expr RBRACKET OF expr              { astnd( Ast.E_array($3, $6) ) 1 6 }
| SELECT expr__ LBRACKET expr RBRACKET                { astnd( Ast.E_select($2, $4) ) 1 5 }
| UPDATE expr__ LBRACKET expr RBRACKET LEFT_ARR expr  { astnd( Ast.E_update($2, $4, $7) ) 1 7 }
| WAPP value LBRACKET expr SEMI expr RBRACKET           { astnd (Ast.E_wapp ($2, $4, $6)) 1 7 }
| WAPS value LBRACKET expr SEMI lam RBRACKET           { astnd (Ast.E_waps ($2, $4, $6)) 1 7 }
| WFOLD value LBRACKET expr SEMI expr SEMI lam RBRACKET { astnd (Ast.E_wfold ($2, $4, $6, $8)) 1 9 }
| FOR var EQUALS value TO value DO expr DONE           { astnd( Ast.Macros.forloop  $2 $4 $6 $8     ) 1  9 }
| FOR var EQUALS value TO value DO var COLON expr DONE { astnd( Ast.Macros.forloopf $2 $4 $6 $8 $10 ) 1 11 }

| MATCH expr WITH patexps MEND                       { astnd( Ast.E_case($2, $4) ) 1 5 }
| IF expr THEN expr ELSE expr                       { astnd( Ast.E_cond($2, $4, $6) ) 1 6 }
| expr_ DOT field                                   { astnd( Ast.E_proj ($3, $1) ) 1 3 }
| expr_ LBRACKET expr RBRACKET                      { astnd( Ast.E_wproj ($1, $3) ) 1 4 }
| expr__                                            { $1 }
| SUBSET expr expr expr                             { astnd (Ast.E_subset($2, $3, $4)) 1 4 }

/*| FOR var EQUALS value TO value DO var COLON expr DONE { astnd( Ast.Macros.forloopf $2 $4 $6 $8 $10 ) 1 11 }*/
;
expr__:
| BEGIN expr END                                 { astnd (Ast.E_paren $2) 1 3 }
| LPAREN expr RPAREN                             { astnd (Ast.E_paren $2) 1 3 }
| value                                          { astnd( Ast.E_value $1 ) 1 1 }
;

patexps:
| PIPE patexps_ { $2 } /* optional: PIPE before initial branch.  */
| patexps_      { $1 }
;
patexps_:
| cons pats BIGARR expr               { [ ($1, $2, $4) ] }
| cons pats BIGARR expr PIPE patexps  { ( $1, $2, $4) :: $6 }
;

app:
| lam                { astnd( Ast.A_lam $1) 1 1 }
| app_ expr__        { astnd( Ast.A_app ($1,$2)) 1 2 }
| app_               { $1 }
;

app_:
| var               { astnd( Ast.A_var $1) 1 1 }
| LPAREN app RPAREN { astnd( Ast.A_paren $2) 1 3 }


env:
| LBRACE env_body RBRACE { $2 }
;
env_body:
| /* empty */                    { [ ] }
| var COLON value                { [ ($1, $3) ] }
| var COLON value COMMA env_body { ($1, $3) :: $5 }
;
