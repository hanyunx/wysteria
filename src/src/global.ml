module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

type stringToIntMap = int StringMap.t
type intToStringMap = string IntMap.t

type func = 
  | F_pretty_print_ast
  | F_test_circuit_gen
  | F_test_opsem

(* let func = ref F_test_circuit_gen *)
let func = ref F_test_opsem

(* let verbose_errors = ref false *)
let print_passes = ref true
let debug_opsem = ref false
let debug_store = ref false
let whoami = ref "No_one"
let whoarewe = ref "top"
let secrets = ref "{}"
let gmwport = ref 0
let gmwaddr = ref "127.0.0.1"

let p_enc_file = ref "princs.txt"
let princ_to_nat:(stringToIntMap ref) = ref (StringMap.empty)
let nat_to_princ:(intToStringMap ref) = ref (IntMap.empty)

let p_to_n s = StringMap.find s (!princ_to_nat)
let n_to_p n = IntMap.find n (!nat_to_princ)

let args = Arg.align [ 
(*  ("--verbose",           Arg.Set verbose_errors, " give verbose (contextual) errors") ; *)
  ("--db-opsem",          Arg.Set debug_opsem, " give verbose information during operational semantics") ;
  ("--db-store",          Arg.Set debug_store, " when --db-opsem is set, also debugs the store too") ;
  ("--gmw-port",          Arg.Int (fun port -> gmwport := port), " set the GMW server port");
  ("--gmw-addr",          Arg.String (fun addr -> gmwaddr := addr), " set the GMW server IP address");
  ("--princs-encoding",   Arg.String (fun file -> p_enc_file := file), " file from which princs to nat encoding should be read");
  ("--pp-ast",            Arg.Unit (fun _ -> func := F_pretty_print_ast ), " pretty-print the ast then quit." ) ;
  ("--db-ckt",            Arg.Unit (fun _ -> func := F_test_circuit_gen ), " test/debug circuit generation." ) ;
  ("--test-opsem",        Arg.Unit (fun _ -> func := F_test_opsem), " test/debug small-step operational semantics." ) ;
  ("--i-am",              Arg.String (fun me -> whoami := me), " set local principal name (e.g., !Alice)") ;
  ("--we-are",            Arg.String (fun us -> Printf.printf "\n%s\n%!" us; whoarewe := us), " set global principal set (e.g., {!Alice,!Bob})");
  ("--my-secrets",        Arg.String (fun s -> secrets := s), " set local secrets (e.g., {x:3, y:7})") ;
]

let parse_princ_nat_mapping _ =
  let _ = 
    try
      let fin = open_in !p_enc_file in
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
	  
	  princ_to_nat := StringMap.add p (int_of_string n) (!princ_to_nat);
	  nat_to_princ := IntMap.add (int_of_string n) p (!nat_to_princ);
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
  ()


let cur_filename = ref ""

let lexbuf : Lexing.lexbuf ref = ref (Lexing.from_string "")

let set_lexbuf lb = lexbuf := lb
let get_lex_pos _ = (!lexbuf).Lexing.lex_curr_p

module Prov = struct
  type loc = Lexing.position           
  type loc_range = loc * loc

  and prov = 
    | Synth
    | Root of loc_range
    | Stepped of prov

  let rec sprint_prov prefix = function
    | Synth -> prefix^"synth"
    | Stepped p -> sprint_prov prefix p
    | Root (loc1, loc2) -> 
        Printf.sprintf "%sFile \"%s\", line %d, characters %d-%d"
          prefix
          loc1.Lexing.pos_fname
          loc1.Lexing.pos_lnum
          (loc1.Lexing.pos_cnum - loc1.Lexing.pos_bol)
          (loc2.Lexing.pos_cnum - loc2.Lexing.pos_bol)

          (*
    | Subst (n, p1, p2) ->
        Printf.sprintf "%sSubstitution of `%s' at\n%s%sfor original `%s' at\n%s"
          prefix n (sprint_prov (prefix^"  ") p2) 
          prefix n (sprint_prov (prefix^"  ") p1) *)
end

let dprov = Prov.Root(Lexing.dummy_pos, Lexing.dummy_pos)
