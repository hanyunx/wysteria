exception Error of exn * (int * int * string)
exception Internal_error

exception TestCkt

open Ast
open Testtyp

module Lexer = Wylexer
module Parser = Wyparser

let unk = T_unknown

let testckt (ast:file_nd) =
  match ast.data with
    | F_body(exnd) ->      
      let astnd p d = { prov = p; data = d; info = T_unknown } in
      let modend = astnd Global.dprov Par in
      let topplcnd = astnd Global.dprov Pl_top in
      let plabc = astnd Global.dprov (Pl_ps(modend, ablitvnd)) in
      (*let plabc = astnd Global.dprov (Pl_ps(modend, abclitvnd)) in*)
      print_string "typing the place"; print_newline ();
      let tplabc = Typchk.typplcnd plabc topplcnd [] in
      print_string "typing the program"; print_newline ();
      let _ = Typchk.typex exnd tplabc [] in
      (*let (ckt, r) = Gencircuit.genex [] texnd in
      let ckt1 = Gencircuit.setupout ["!A"; "!B"] r texnd.info in
      Cktlib.printckt (ckt @ ckt1);*)
      print_string "done typing the program";
      print_newline ()
    | _ -> raise TestCkt

let parse_channel : string -> in_channel -> file_nd = 
  fun filename channel ->
    let lexbuf = Lexing.from_channel channel in
    let pos = lexbuf.Lexing.lex_curr_p in
    let _ = 
      lexbuf.Lexing.lex_curr_p <- 
        { pos with 
            Lexing.pos_fname = filename ; 
            Lexing.pos_lnum = 1 ; 
        } 
    in
    let _ = Global.set_lexbuf lexbuf in
    let ast : file_nd = 
      try Parser.file Lexer.token lexbuf 
      with 
        | exn -> begin
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf in
            raise (Error (exn, (line, cnum, tok)))
          end
    in
    ast

let process_file : string -> file_nd = fun filename ->
  let _ = Global.cur_filename := filename in
  let input = 
    if filename = "-" 
    then stdin 
    else open_in filename 
  in
  let ast = 
    try parse_channel filename input
    with
      | Error (_, (line, col, token)) ->
          ( Printf.eprintf "File %s, line %d, character %d: syntax error at %s\n%!"
              filename line col 
              ( if token = "\n" 
                then "newline" 
                else Printf.sprintf "`%s'" token ) ;
            exit (-1) )
  in
  begin match ! Global.func with
    | Global.F_test_opsem -> Driver.eval ast
    | Global.F_test_circuit_gen -> testckt ast
    | Global.F_pretty_print_ast -> print_string "AST-BEGIN:\n" ;
        Pretty.pp_file_nd ast ; print_string "\n" ;
        print_string "AST-END.\n" ;
  end
  ; ast
  
let _ = 
  let input_files : string list ref = ref [] in
  (*if !Global.print_passes then Printf.eprintf "parsing input files...\n%!" ;*)
  let _ = Arg.parse Global.args
    (fun filename -> input_files := filename :: !input_files)
    "usage: m3pc [options] [input files]"
  in
  if !input_files = [] then (
    Printf.eprintf "no input files given!\n" ;
    exit (-1);
  );
  
  Global.parse_princ_nat_mapping ();
  
  let _ = List.map process_file (List.rev (!input_files)) in
  (** TODO -- emit/do something! **)  
  () 
  ;
  if !Global.print_passes then
    (*Printf.eprintf "done.\n%!"*)
    ()
  else ()
