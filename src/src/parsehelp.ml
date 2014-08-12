open Ast
module Parser = Wyparser

exception Parse_error of exn * (int * int * string)

(* This code was moved from opsem.ml. *)
(* Now it is more general, and driver.ml uses it to parse the initial place. *)

let parse_prod : string -> string 
  -> [`Value | `Place] -> 
  [ `Value of value_nd | `Place of place_nd ]
  =
  let module Lexer = Wylexer in
  let module Parser = Wyparser in
  fun filename string production ->
    let lexbuf = Lexing.from_string string in
    let pos = lexbuf.Lexing.lex_curr_p in
    let _ = 
      lexbuf.Lexing.lex_curr_p <- 
        { pos with 
            Lexing.pos_fname = filename ; 
            Lexing.pos_lnum = 1 ; 
        } 
    in
    let _ = Global.set_lexbuf lexbuf in
    try ( match production with
            | `Value -> `Value (Parser.value Lexer.token lexbuf)
            | `Place -> `Place (Parser.place Lexer.token lexbuf)
        )
    with 
      | exn -> begin
          let curr = lexbuf.Lexing.lex_curr_p in
          let line = curr.Lexing.pos_lnum in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lexbuf in
          raise (Parse_error (exn, (line, cnum, tok)))
        end

let parse_place : string -> string -> place_nd = 
  fun file text ->
    match (parse_prod file text `Place)
    with 
      | `Place p -> p
      | _ -> failwith "impossible"
