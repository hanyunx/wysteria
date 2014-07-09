{
  open Lexing
  open Wyparser

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- 
      { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
      }
}

let var_head = ['a'-'z' '_'] 
let var_num = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let var = var_head var_num* 

let cons_head = ['A'-'Z' '_'] 
let cons_num = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let cons = cons_head cons_num* 

let num = ['0'-'9']+

let bang = ['!']

let hash = ['#']
let nothash = [^ '#' '\n']
let noteol = [^'\n']

(* !! Important: 
   when we match this, we must increment the line number
   of the lexbuf record. *)
let eol = ['\n'] 

let strlit = '"' ( [^'"'] | "\\\"" )* '"'

let charlit = '\'' [^'\'']* '\''

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | eol        { incr_linenum lexbuf ; token lexbuf } 
  | '(' '*'    { nested_comment 0 lexbuf }
  | '.' { DOT }
  | ':' { COLON }
  | '|' { PIPE }
  | ';' { SEMI }
  | '('	{ LPAREN }
  | ')'	{ RPAREN }
  | '{'	{ LBRACE }
  | '}'	{ RBRACE }
  | '['	{ LBRACKET }
  | ']'	{ RBRACKET }
  | '='	{ EQUALS }
  | "<-" { LEFT_ARR }
  | "->" { ARROW }
  | "-" { MINUS }
  | ','  { COMMA }
  | "+" { PLUS }
  | ">" { GT }
  | "unit" { TUNIT }
  | "nat" { TNAT }
  | "bool" { TBOOL }
  | "string" { TSTRING }
  | "proc" { TPROC }
  | "W"  { TWIRE }
  | "in" { IN }
  | "let" { LET }
  | "true" { TRUE }
  | "false" { FALSE }
  | ("∪" | "union") { UNION }
  | ("⊆" | "subeq") { SUBEQ }
  | "()" { UNITV }
  | "and" { AND }
  | "top" { TOP }
  | ( "{}" | "∅" ) { EMP } 
  | "singl" { SINGL }
  | "par" { PAR }
  | "sec" { SEC }
  | "ps" { PS }
  | "fix" { FIX }
  | ("\\" | "" ) { LAM }
  | "match" { MATCH }
  | "for" { FOR }
  | "to"  { TO }
  | "do" { DO }
  | "done" { DONE }
  | "while" { WHILE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "with" { WITH }
  | "begin" { BEGIN }
  | "end" { END }
  | "mend" { MEND }
  | ("at" | "@") { AT }
  | "wire" { WIRE }
  | "wapp" { WAPP }
  | "waps" { WAPS }
  | "wcopy" { WCOPY }  
  | "from" { FROM }  
  | "as" { AS }  
  | "wfold" { WFOLD }
  | "of"     { OF }
  | "array"  { ARRAY }
  | "select" { SELECT }
  | "update" { UPDATE }
  | "print"  { PRINT }
  | "sysop"  { SYSOP }
  | "Sh"   { TSH }
  | "makesh" { MAKESH }
  | "combsh" { COMBSH }
  | "++" { PLUSPLUS }
  | "=>" { BIGARR }

  | "cast" { CAST }
      

(*
( ['\n'] [^'#' '\n'] [^'\n']+ ) as code 
      { (* Printf.eprintf "code: '%s'\n" code;  *)
        CODE_LINE code }
*)

  (* identifiers *)
  | var as v { VAR v }          (* variables *)
  | cons as c { CONS c }        (* constructors *)
  | hash var as f { FIELD f }  (* fields *)
  | bang var as p { PRINC p }  (* princs -- lowercase *)
  | bang cons as p { PRINC p }  (* princs -- uppercase *)
(*  | hash (num as n) { NUMIDX (int_of_string n) }   (* numeral indicies *) *)
  | num as n { NUM (int_of_string n) }
  | strlit as s { STRLIT (String.sub s 1 ((String.length s)-2)) (* Strip off '"'s *)}
  | charlit as s { STRLIT s }

  (* lastly: *)
  | eof { EOF }
  | _ as c  { OTHER_CHAR c }

  (* Line comments *)
  | '-' '-' '-' noteol+ eol { incr_linenum lexbuf ; token lexbuf }
  | '/' '/' noteol+ eol { incr_linenum lexbuf ; token lexbuf }

and nested_comment level = parse 
  | '(' '*' { nested_comment (level + 1) lexbuf }
  | '*' ')' { if level = 0 then 
                token lexbuf 
              else 
                nested_comment (level-1) lexbuf }
  | eol { incr_linenum lexbuf ; nested_comment level lexbuf }
  | _   { nested_comment level lexbuf }
