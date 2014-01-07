exception Error of exn * (int * int * string)
exception Internal_error

exception TestCkt

open Ast
open Testtyp

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

let test_opsem ast =
  (* -- TODO: parse local secrets into an environment:
  let secrets : (Raw.var_nd * Raw.value_nd) list =
    let lexbuf = Lexing.from_string !Global.secrets in
    let xs = Parser.env Lexer.token lexbuf in
    List.map begin fun (x, v) ->
      (x, Typchk.typv [] v)
    end xs
  in
  *)
  let module Flags = 
      struct 
        let variant = `Single_party_protocol (!Global.whoami)
      end 
  in
  let module S = Opsem.Make(Flags)in
  let cfg = 
    match ast.data with
      | F_body e ->
	let astnd p d = { prov = p; data = d; info = T_unknown } in
	(*let modend = astnd Global.dprov Par in*)
	let topplcnd = astnd Global.dprov Pl_top in
	(*let plabc = astnd Global.dprov (Pl_ps(modend, ablitvnd)) in
	print_string "typing the place"; print_newline ();
	let tplabc = Typchk.typplcnd plabc topplcnd [] in*)
	let tplabc = topplcnd in
	print_string "typing the program under "; Pretty.pp_place_nd tplabc; print_newline ();
	let typde = Typchk.typex e tplabc [] in
	print_string "done with type checking the program"; print_newline ();
	S.initial (fst typde)
      | _ -> raise (Invalid_argument "test_opsem")
  in
  (*print_string "running small-step operational semantics:\n" ; print_newline ();*)
  let rec driver_loop cfg =
    (*if !Global.debug_opsem then*)
    if false then
      ( S.pp_cfg cfg ; print_string "\n\n" )
    else () 
    ;
    if S.is_halted cfg then
      cfg
    else 
      driver_loop (S.step cfg)
  in
  let time1 = Unix.gettimeofday () in
  Random.self_init ();
  let final_cfg = driver_loop cfg in
  let time2 = Unix.gettimeofday () in
  (*print_string "halted, successfully.\n";*)
  
  begin
    match final_cfg.S.expr.data with
      | E_value({data=V_var({data=v})}) ->
	let cvnd = (Env.find v final_cfg.S.env).value in
	print_string ((!Global.whoami)^" output: "); Pretty.pp_value_nd cvnd;
	print_newline ();
	()

      | _ ->
	print_string "final cfg : \n";
	S.pp_cfg final_cfg; print_newline ();
	print_newline ();
	()
  end;

  (*let r = Gencircuit.gatherperformance.Gencircuit.collect () in
  
  print_string "Total secure blocks: "; print_int (Gencircuit.gatherperformance.Gencircuit.length ()); print_newline ();
  print_string "Total time in dumping inputs: "; print_float r.Gencircuit.t_dumpinput; print_newline ();
  print_string "Total time in circuit generation: "; print_float r.Gencircuit.t_cktgen; print_newline ();
  print_string "Total time in circuit dumping: "; print_float r.Gencircuit.t_cktdump; print_newline ();
  print_string "Total time in config dumping: "; print_float r.Gencircuit.t_configdump; print_newline ();
  print_string "Total time in GMW runs: "; print_float r.Gencircuit.t_gmwrun; print_newline ();
  print_string "Total time in parsing outputs: "; print_float r.Gencircuit.t_parseoutput; print_newline ();
  print_string "Total number of gates: "; print_int r.Gencircuit.num_gates; print_newline ();
  
  print_newline ();*)
  (*print_string "total opsem time : "; print_float (time2 -. time1);
  print_newline ()*)
  ()

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
    | Global.F_test_opsem -> test_opsem ast
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

  (* initialize princs to nat and nat to princs mapping *)
  let _ = 
    try
      let fin = open_in (!Global.p_enc_file) in
      let rec helper (x:unit) :unit =
	try
	  let s = input_line fin in
	  let l = Str.split (Str.regexp "[ ]+") s in
	  let p, n = match l with
	    | p1::n1::[] -> p1, n1
	    | _ ->
	      print_string "Line "; print_string s; print_string " in princ map file not in <princ_name>[ ]+<princ_id> format";
	      raise (Invalid_argument "")
	  in
	  
	  Global.princ_to_nat := Global.StringMap.add p (int_of_string n) (!Global.princ_to_nat);
	  Global.nat_to_princ := Global.IntMap.add (int_of_string n) p (!Global.nat_to_princ);
	  helper ()
	with
	  | End_of_file -> close_in fin
      in
      helper ()
    with
      | Sys_error(_) ->
	print_string "Warning: princs to nat map file not found";
	print_newline ()	
  in
  (* initialized *)
  
  let _ = List.map process_file (List.rev (!input_files)) in
  (** TODO -- emit/do something! **)  
  () 
  ;
  if !Global.print_passes then
    (*Printf.eprintf "done.\n%!"*)
    ()
  else ()
