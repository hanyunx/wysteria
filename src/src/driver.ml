open Ast
open Testtyp

let tenvref = ref []

let eval ast =
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

(*
  	let modend = astnd Global.dprov Par in
	let topplcnd = astnd Global.dprov Pl_top in
	let plabc = astnd Global.dprov (Pl_ps(modend, ablitvnd)) in
	print_string "typing the place"; print_newline ();
	(*let tplabc = Typchk.typplcnd plabc topplcnd [] in*)
	let tplabc = topplcnd in
*)
        let place = Parsehelp.parse_place "" (!Global.whoarewe) in
        let place = Typchk.typplcnd place (astnd Global.dprov Pl_top) [] in
        let tplabc = place in
        
	print_string "typing the program under "; Pretty.pp_place_nd tplabc; print_newline ();
	let typde, eff, nenv = Typchk.typex e tplabc !tenvref in
	tenvref := nenv;
	print_string "done with type checking the program"; print_newline ();
	S.initial typde
      | _ -> raise (Invalid_argument "eval")
  in
  (*print_string "running small-step operational semantics:\n" ; print_newline ();*)
  let rec driver_loop cfg =
    if !Global.debug_opsem then
      ( S.pp_cfg cfg ; print_string "\n\n" )
    else () 
    ;
    if S.is_halted cfg then
      cfg
    else 
      driver_loop (S.step cfg)
  in
  (*let time1 = Unix.gettimeofday () in*)
  Random.self_init ();
  let final_cfg = driver_loop cfg in
  (*let time2 = Unix.gettimeofday () in*)
  (*print_string "halted, successfully.\n";*)
  
  begin
    match final_cfg.S.expr.data with
      | E_value(vnd) ->
	let cvnd = S.close_value final_cfg vnd in
	print_string ((!Global.whoami)^" output: "); Pretty.pp_value_nd cvnd;
	print_newline ();
	()

      (*| E_value({data=V_var({data=v})}) ->
	let cvnd = (Env.find v final_cfg.S.env).value in*)

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

let setargs (s:string array) :unit =
  Arg.parse_argv ?current:(Some (ref 0)) s Global.args (fun _ -> ()) "usage: m3pc [options]";
  Global.parse_princ_nat_mapping ()

module type TOPLEVEL_EVAL = sig

  exception Eval_exception
  val eval : expr_nd -> value_nd

  exception Stack_exception
  val push : unit -> unit
  val pop  : unit -> unit
end

(* Create a "top-level" expression evaluator. *)
let make_toplevel_eval : unit -> (module TOPLEVEL_EVAL) = fun _ ->
  let module Toplevel_eval = struct
    exception Eval_exception
    exception Stack_exception
    
    (* Construct S, an operational-semantics module for the local principal. *)
    (* Notice that the global variable Global.whoami is read below. *)
    module Flags = struct 
      let variant = `Single_party_protocol (!Global.whoami)
    end

    module S = Opsem.Make(Flags)

    (* The environments below (for typing and evaluation) constitute
       the _internal state_ of this module.  

       For each (typing and evaluation) we maintain an active
       environment that is extended by successive uses of eval.
       
       We also maintain an (inactive, possibly empty) stack of
       environments that are manipulated by uses of push and
       pop. These stack operations are optional; they are useful when
       the meta-level client wishes to impose a meta-level stack
       discipline across successive uses of eval---that is, to signal
       when to temporarily shadow variables within an inner
       (meta-level) scope, and then later leave this inner
       (meta-level) scope.
    *)
    let type_env = (ref ([], []))
    let eval_env = (ref (Env.empty, []))

    let push : unit -> unit = fun _ -> begin
       type_env := ( fst(!type_env), 
                    fst(!type_env) :: snd(!type_env) ) ;
      eval_env := ( fst(!eval_env), 
                    fst(!eval_env) :: snd(!eval_env) ) ;
      () 
    end

    let pop : unit -> unit = fun _ -> begin
      match snd !type_env, snd !eval_env with
        | (type_env_saved :: type_env_rest),
          (eval_env_saved :: eval_env_rest) -> begin
            type_env := (type_env_saved, type_env_rest) ;
            eval_env := (eval_env_saved, eval_env_rest) ;
            ()
          end
        | _, _ -> raise Stack_exception
    end

    let eval : expr_nd -> value_nd = fun e ->
        (* Produce a machine configuration with an expression that is type-checked. *)
      let cfg = 
        let astnd p d = { prov = p; data = d; info = T_unknown } in
        let topplcnd = astnd Global.dprov Pl_top in
        let tplabc = topplcnd in
    
        print_string "typing the program under "; Pretty.pp_place_nd tplabc; print_newline ();
        let e_typed, eff, nenv = Typchk.typex e tplabc (fst (!type_env)) in
        type_env := (nenv, snd(!type_env));
        print_string "done with type checking the program"; print_newline ();        
        ( S.initial ~env:(fst(!eval_env)) e_typed )
      in
  
      (* Driver loop for small-step semantics: 
         Runs the configuration until it halts. *)
      let rec driver_loop cfg =
	(*if true then
	  ( S.pp_cfg cfg ; print_string "\n\n" )
	else () 
	;*)
        if S.is_halted cfg then
          cfg
        else 
          driver_loop (S.step cfg)
      in
      let final_cfg = (* Do it! *)
        driver_loop cfg 
      in
      (* Save the new evaluation environment *)
      eval_env := (cfg.S.env, snd(!eval_env)) ;

      begin match final_cfg.S.expr.data with
	| E_value(vnd) ->
	  let cvnd = S.close_value final_cfg vnd in
	  print_string ((!Global.whoami)^" output: "); 
          (*Pretty.pp_value_nd cvnd;
	  print_newline () ;*)
	  cvnd (* <-- Final, closed value *)
	  
        (*| E_value({data=V_var({data=v})}) ->
            (* Have final value.  This final value may have free
               variables; so, close it by the current environment. *)
	    let cvnd = (Env.find v final_cfg.S.env).value in
        
            (* Print the final value *)
	    print_string ((!Global.whoami)^" output: "); 
            Pretty.pp_value_nd cvnd;
	    print_newline () ;
	    cvnd (* <-- Final, closed value *)
        *)
        | _ -> (* Something when wrong. *)
	    print_string "final cfg : \n";
	    S.pp_cfg final_cfg; print_newline ();
	    print_newline () ;
	    raise Eval_exception
      end
  end (* Toplevel_eval module *)
  in
  (module Toplevel_eval)

(* let modval = make_toplevel_eval () *)

(* module Top = (val modval) *)
