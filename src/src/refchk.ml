open Ast
open Ast.Library

exception RCheckError of string

type z3Term =
  | True
  | False
  | EmptySet
  | Int of int
  | VarSet of string
  | Singl of z3Term
  | Eq of z3Term * z3Term
  | Update of z3Term * z3Term * z3Term
  | Union of z3Term * z3Term
  | Intersection of z3Term * z3Term
  | Not of z3Term
  | Subset of z3Term * z3Term
  | And of z3Term * z3Term

type zenv = (string * z3Term) list

type facts = z3Term list

let lookupzenv (s:string) (l:zenv) :z3Term = snd (List.find (fun (s1, _) -> s1 = s) l)

let map (s:string) :int =
  if s = "!Alice" then 1
  else if s = "!Bob" then 2
  else if s = "!Charlie" then 3
  else 4

type fvt = { reset : unit -> unit; get : unit -> string}
let freshvar:fvt =
  let r = ref 1 in
  {
    reset = (fun _ -> r := 1);
    get = (fun _ -> let s = ("z3_" ^ (string_of_int !r)) in
		   r := !r + 1;
		   s)
  }
      
let rec bindref (z:z3Term) (rnd:refine_nd) (gettyp:var_nd -> typ) (e:zenv) (f:facts) :(z3Term * zenv * facts) =
  match rnd.data with
    | R_true -> True, e, f
      
    | R_singl -> Singl(z), e, f

    | R_subeq(vnd) ->
      let z1, e1, f1 = bindval vnd gettyp e f in
      Subset(z, z1), e1, f1

    | R_eq(vnd) ->
      let (z1, e1, f1) = bindval vnd gettyp e f in
      Eq(z, z1), e1, f1

    | R_conj(r1, r2) ->
      let z1, e1, f1 = bindref z r1 gettyp e f in
      let z2, e2, f2 = bindref z r2 gettyp e1 f1  in
      And(z1, z2), e2, f2

    | R_not(r) ->
      let z1, e1, f1 = bindref z r gettyp e f in
      Not(z1), e1, f1

and bindval (vnd:value_nd) (gettyp:var_nd -> typ) (e:zenv) (f:facts) :(z3Term * zenv * facts) =
  match vnd.data with
    | V_princ(pnd) ->
      let z1 = VarSet(freshvar.get ()) in
      let z2 = VarSet(freshvar.get ()) in
      let f1 = Eq(z1, EmptySet) in
      let f2 = Eq(z2, Update(z1, Int(map pnd.data), True)) in
      let f3 = Singl(z2) in
      z2, (("", z1)::("", z2)::e), f @ [ f1; f2; f3 ]
    (*z, (("", z)::e), f @ [ Mem(Int(map pnd.data), z) ]*)
	
    | V_ps_lit(l) ->
      let fold_fun (z, e, f) v =
	let z', e', f' = bindval v gettyp e f in
	Union(z, z'), e', f'
      in
      List.fold_left fold_fun (EmptySet, e, f) l

    | V_ps_union(v1, v2) ->
      let z1, e1, f1 = bindval v1 gettyp e f in
      let z2, e2, f2 = bindval v2 gettyp e1 f1 in
      Union(z1, z2), e2, f2

    | V_var(varnd) ->
      let varname = varname varnd in
      begin
	try
	  lookupzenv varname e, e, f
	with
	  | Not_found ->
	    let z = VarSet("z3_" ^ varname) in
	    let r = match (gettyp varnd) with
	      | T_ps(r') -> r'
	      | _ -> raise (RCheckError "Value has non-T_ps type")
	    in
	    let z1, e1, f1 = bindref z r gettyp e f in
	    z, ((varname, z)::e1), f1 @ [z1]
      end

    | V_paren(v) -> bindval v gettyp e f

    | _ -> raise (RCheckError "Value type not supported")


let rec dumpTerm (z:z3Term) (out:out_channel) :unit =
  let ps s = output_string out s in
  let psi n = output_string out (string_of_int n) in

  match z with
    | True -> ps "true"
    | False -> ps "false"
    | EmptySet -> ps "emp"      
    | Int(n) -> psi n
    | VarSet(s) -> ps s
    | Singl(z1) -> ps "(singl "; dumpTerm z1 out; ps ")"
    | Eq(z1, z2) ->
      ps "(= "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps ")"
    | Union(z1, z2) ->
      ps "(union "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps ")"
    | Intersection(z1, z2) ->
      ps "(intersect "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps ")"
    | Not(z1) -> ps "(not "; dumpTerm z1 out; ps ")"
    | Subset(z1, z2) ->
      ps "(subset "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps ")"
    | And(z1, z2) ->
      ps "(and "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps ")"
    | Update(z1, z2, z3) ->
      ps "(store "; dumpTerm z1 out; ps " "; dumpTerm z2 out; ps " ";
      dumpTerm z3 out; ps ")"

let rec dumpFact (z:z3Term) (out:out_channel) :unit =
  let ps s = output_string out s in
  begin
    match z with
      | Singl(_)
      | Eq(_)
      | Subset(_)
      | Not(_)
      | True      
      | And(_) -> ps "(assert "; dumpTerm z out; ps ")"
      | _ -> dumpTerm z stdout; raise (RCheckError "Invalid fact")
  end    

let dumpPreamble (out:out_channel) :unit =
  let ps s = output_string out s in

  ps "(define-sort Set () (Array Int Bool))"; ps "\n";
  ps "(declare-fun singl (Set) Bool)"; ps "\n";
  ps "(declare-const emp Set)"; ps "\n";
  ps "(assert (= emp ((as const Set) false)))"; ps "\n";
  ()

let dumpEnv (env:zenv) (out:out_channel) :unit =
  let ps s = output_string out s in

  List.iter (fun (_, z) ->
    match z with
      | VarSet(s) -> ps ("(declare-const " ^ s ^ " Set)"); ps "\n"
      | _ -> raise (RCheckError "Env contains non var terms ?")
  ) env

let runz3 fname =
  let inc = Unix.open_process_in ("./z3 -smt2 " ^ fname) in
  let line = input_line inc in
  let _ = Unix.close_process_in inc in
  line = "unsat"
  
let verify r1 r2 (gettyp:var_nd -> typ) =
  freshvar.reset ();

  let z = VarSet(freshvar.get ()) in
  let e = ["", z] in
  let f = [] in
  
  let z1, e1, f1 = bindref z r1 gettyp e f in
  let z2, e2, f2 = bindref z r2 gettyp e1 f1 in

  let pnobang = String.sub (!Global.whoami) 1 (String.length (!Global.whoami) - 1) in
  let fname = "z3_" ^ pnobang ^ ".smt" in
  let fout = open_out fname in
  let ps s = output_string fout s in

  dumpPreamble fout;
  dumpEnv e2 fout;
  List.iter (fun z -> dumpFact z fout; ps "\n") f2;
  dumpFact z1 fout;
  ps "\n";
  dumpFact (Not(z2)) fout;
  ps "\n";
  ps "(check-sat)";
  ps "\n";
  close_out fout;
  runz3 fname
