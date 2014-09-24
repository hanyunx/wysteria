open Ast
open Ast.Pretty
open Ast.Library

open Cktlib

exception CGenError of string

type envw = (string * wrange) list

type envt = (string * typ) list

(* TODO: this will go away once par mode closes types also *)
(*let refenv = ref Ast.Env.empty*)

let gmwsock = ref Unix.stdin
let gmwsockset = ref false

let lookupenv (env:(string * 'a) list) (s:string) :'a = snd (List.find (fun elt -> (fst elt) = s) env)

type walloct = { getn : int -> wrange; currid : unit -> int; reset : unit -> unit; }
let walloc : walloct =    
  let r = ref 1 in    (* wires 0,1 are reserved by implementation *)
  
  {
    getn = (fun k ->
      if k <= 0 then
	raise (CGenError ("Allocation must be demanded only for non-zero size: " ^
	(string_of_int k)))
      else
	let p = (!r + 1, !r + k) in
	r := !r + k;
	p);
    
    currid = (fun _ -> !r);

    reset = (fun _ -> r := 1);
  }

type perft =
    {
      t_dumpinput   : float;
      t_cktgen      : float;
      t_cktdump     : float;
      t_configdump  : float;
      t_gmwrun      : float;
      t_parseoutput : float;
      num_gates     : int;
    }
type gather = { length : unit -> int; add : perft -> unit; collect : unit -> perft }
let gatherperformance =
  let l = ref [] in
  
  {
    length = (fun _ -> List.length !l);

    add = (fun r ->
      (*print_string "time in dumping inputs: "; print_float r.t_dumpinput; print_newline ();
      print_string "time in circuit generation: "; print_float r.t_cktgen; print_newline ();
      print_string "time in circuit dumping: "; print_float r.t_cktdump; print_newline ();
      print_string "time in config dumping: "; print_float r.t_configdump; print_newline ();
      print_string "time in GMW runs: "; print_float r.t_gmwrun; print_newline ();
      print_string "time in parsing outputs: "; print_float r.t_parseoutput; print_newline ();*)
      l := r::(!l);
      ()
    );
    
    collect = (fun _ ->
      List.fold_left (fun r r' ->
	{
	  t_dumpinput = r.t_dumpinput +. r'.t_dumpinput;
	  t_cktgen = r.t_cktgen +. r'.t_cktgen;
	  t_cktdump = r.t_cktdump +. r'.t_cktdump;
	  t_configdump = r.t_configdump +. r'.t_configdump;
	  t_gmwrun = r.t_gmwrun +. r'.t_gmwrun;
	  t_parseoutput = r.t_parseoutput +. r'.t_parseoutput;
	  num_gates = r.num_gates + r'.num_gates;
	}
      ) { t_dumpinput = 0.0;
	  t_cktgen = 0.0;
	  t_cktdump = 0.0;
	  t_configdump = 0.0;
	  t_gmwrun = 0.0;
	  t_parseoutput = 0.0;
	  num_gates = 0;
	} !l);
  }

(*let is_closedprincs_env vnd  =
  print_string "closing vnd: "; pp_value_nd vnd; print_newline ();
  let closevnd = AstMap.close_value !refenv vnd in    
  let rec helper = function
    | V_princ(pnd) -> [pnd]
    | V_ps_union(v1, v2) -> helper v1.data @ helper v2.data
    | V_ps_lit(l) -> List.flatten (List.map (fun v -> helper v.data) l)
    | _ -> pp_value_nd vnd; print_newline (); raise (CGenError "Princs not closed")
  in
  helper closevnd.data*)

let princstolist vnd  =
  let rec helper = function
    | V_princ(pnd) -> [pnd]
    | V_ps_union(v1, v2) -> helper v1.data @ helper v2.data
    | V_ps_lit(l) -> List.flatten (List.map (fun v -> helper v.data) l)
    | _ -> pp_value_nd vnd; print_newline (); raise (CGenError "Princs not closed")
  in
  helper vnd.data

let rec size_env t =
  match t with
  | T_unit -> unitsize
  | T_bool -> boolsize
  | T_nat -> natsize
  | T_sum([_, [t1]; _, [t2]]) -> max (size_env t1.data) (size_env t2.data) + 1
  | T_row(l) -> List.fold_left (fun s elt -> (size_env (snd elt).data) + s) 0 l
  | T_wire(vnd, tnd) ->
    (*let cvnd = AstMap.close_value !refenv vnd in*)
    let cvnd = vnd in
    (List.length (is_closedprincs cvnd)) * (size_env tnd.data)
  | T_sh(_, tnd) -> size_env tnd.data
  | T_ps(_) -> natsize
  | t -> raise (CLibError "Unimplemented type in size_env")

let sizetl_env tl = List.fold_left (fun sz t -> sz + size_env t) 0 tl

(*
 * princs: parties running circuit
 * env: type environment for the secure block, all types are either wires or shares
 * return: circuit for input set up, mapping of variable in the env to their wires
 *)
let setupinp (princs:string list) (env:(string * typ) list) :(circuit * (string * wrange) list) =
  
  (*
   * fold function to map each party to (varname, wirerange type) list
   * for a wire bundle, a party may or may not have input to provide
   * further, this information will be used to allocate contiguous wires for
   * each party's inputs
   *)
  let getprincinp (map:(string * typ) list StringMap.t) (varname, vartyp) = match vartyp with
    | T_wire(vnds, typnd) ->
      let vprincsnds = princstolist vnds in

      let g mapprinc princnd =
	let prev =
	  if StringMap.mem princnd.data mapprinc then
	    StringMap.find princnd.data mapprinc
	  else
	    []
	in
	StringMap.add princnd.data (prev @ [varname, typnd.data]) mapprinc
      in

      List.fold_left g map vprincsnds	

    | T_sh(_, _) -> map    (* shares are handled later *)

    | _ ->
      Pretty.pp_typ vartyp; print_newline ();
      raise (CGenError "Unimplemented input type to secure blocks")
  in
  
  let tmap = List.fold_left getprincinp StringMap.empty env in
  
  let sortedprincs = List.sort (fun s1 s2 -> compare s1 s2) princs in
  
  
  (*
   * for each principal, allocate wires needed to store their input, use tmap
   * inpckts is the circuit of input elements, wmap maps each princ to its
   * allocated wire range
   *)
  let (inpckts, wmap) =
    List.fold_left (fun (c, m) p ->
      if StringMap.mem p tmap then
	let sz = sizetl_env (snd (List.split (StringMap.find p tmap))) in
	if sz > 0 then
	  let r = walloc.getn sz in
	  c @ [Input(p, r)], StringMap.add p r m
	else
	  c, m
      else
	c, m
    ) ([], StringMap.empty) sortedprincs
  in
 
  (*
   * now allocate wires for shares
   * there cannot be wires of shares or vice versa
   *)
  let totalshsz =
    List.fold_left (fun sz (_, vtyp) -> match vtyp with
      | T_sh(_, typnd) -> sz + size_env typnd.data
      | _ -> sz
    ) 0 env in

  (*
   * if total shares size is 0, and some party does not have an input
   * to contribute to this secure block, raise an error: GMW requires
   * each party to have some input
   *)
  let _ =
    try
      let p = List.find
	(fun p -> not(StringMap.mem p wmap)) sortedprincs in
      
      raise (CGenError ("GMW library requires each party to have inputs to the secure blocks. Principal " ^ p ^ " does not contribute to the secure block"))
	
    with
      | Not_found -> ()
  in  
  
  (*
   * create circuit elements for share inputs
   * wshenv is a list of (variable of share type, its allocated wire range)
   *)
  let (inpwithshckts, wshenv) =
    if totalshsz = 0 then
      (inpckts, [])
    else
      let (i, j) = walloc.getn totalshsz in
      
      (* c is the current index in (i, j) *)
      let f (c, w) (varname, vartyp) = match vartyp with
	| T_sh(_, typnd) ->
	  let sz = size_env typnd.data in
	  if sz > 0 then
	    c + sz, (w @ [varname, (c, c + sz - 1)])
	  else
	    c, w
	      
	| _ -> c, w
      in

      inpckts @ [ShInput(i, j)], (snd (List.fold_left f (i, []) env))
  in
  
  (* we now need to stitch parties input wires to input variables of wire types *)

  (*
   * recall that we had a map of princ |-> (inputvarname, typ) list
   * and we allocated a contiguous set of wires per princ depending on
   * sum of all the typ sizes
   * this function finds starting offset of a particular varname given the list
   * and the varname
   * the offset is 0 based
   *)
  let wireoffset varname l =
    let rec index n = function
      | (s, _ ):: l' -> if s = varname then n else index (n + 1) l'
      | [] -> raise Not_found
    in
    
    let n = index 1 l in
    let l' = firstn l (n - 1) in
    
    sizetl_env (snd (List.split l'))
  in
  
  (*
   * ckt is the accum ckt, wenv is (varname, wrange) list
   * wenv is finally mapping each input variable to its wire range
   *)
  let f (ckt, wenv) (varname, vartyp) = match vartyp with
    | T_wire(vnds, typnd) ->
      let vprincsnds = sortprincs (princstolist vnds) in
      let sz = size_env typnd.data in

      if sz > 0 then
	(*
	 * allocate wires for this variable and copy from each party's input to
	 * its wire bundle component
	 *)
	let g (c, curr) princnd =
	  let envl = StringMap.find princnd.data tmap in
	  let offset = wireoffset varname envl in

	  let (i, _) = StringMap.find princnd.data wmap in
	  c @ [Copy((curr, curr + sz - 1), (i + offset, i + offset + sz - 1))], curr + sz
	in
	
	let (i, j) = walloc.getn (List.length vprincsnds * sz) in
	fst (List.fold_left g (ckt, i) vprincsnds), wenv @ [varname, (i, j)]
      else
	ckt, wenv

    | T_sh(_, _) -> (ckt, wenv)    (* shares are already handled *)

    | _ -> raise (CGenError "Error")
  in

  List.fold_left f (inpwithshckts, wshenv) env

(*
 * set up output part of the circuit
 * TODO: this code is unnecessarily complex since GMW lib says output
 * per party should be contiguous wires, change GMW to handle non-contiguous
 *)
let rec setupout (princs:string list) (r:wrange) (t:typ) env =
  
  (*
   * size of t typ output for principal princ
   * this is for clear output, shares will be handled later
   *)

  let rec outsize t princ = match t with
    | T_unit -> unitsize
    | T_nat -> natsize
    | T_bool -> boolsize
    | T_sum([_, [t1]; _, [t2]]) -> max (outsize t1.data princ) (outsize t2.data princ) + 1
    | T_row(l) -> List.fold_left (fun s elt -> (outsize (snd elt).data princ) + s) 0 l
    | T_wire(vnd, tnd) ->
      let cvnd = princstolist vnd  in
      if existsprinc cvnd princ then
	outsize tnd.data princ
      else
	0
    | T_sh(_, tnd) -> 0
    | T_ps(_) -> natsize
    | _ ->
      Pretty.pp_typ t; print_newline ();
      raise (CGenError "Unimplemented type in outsize")
  in

  (* allocate output wires for each principal, wmap : principal |-> fst r *)
  let (ckt, wmap) = List.fold_left (fun (c, w) princ ->
    let sz = outsize t princ in
    if sz > 0 then
      let r = walloc.getn sz in
      c @ [Output(princ, r)], StringMap.add princ (fst r) w
    else
      c, w
  ) ([], StringMap.empty) princs in  
  
  let rec totalshsz = function
    | T_nat -> 0
    | T_unit -> 0
    | T_bool -> 0
    (* TODO: Shares inside sums ? *)
    | T_sum(_) -> 0
    | T_wire(_) -> 0
    | T_ps(_) -> 0    
    | T_row(l) ->
      List.fold_left (fun sz (_, tnd) -> sz + totalshsz tnd.data) 0 l
    | T_sh(_, tnd) -> size_env tnd.data
    | T_array(tnd) -> totalshsz tnd.data
    | _ -> raise (CGenError "Totalshsz does not identify input type")
  in

  let (shi, shj) =
    let tshsz = totalshsz t in
    if tshsz > 0 then
      walloc.getn tshsz
    else
      (-1, -1)
  in

  let cktwithsh =
    if rsize (shi, shj) > 0 then
      ckt @ [ShOutput(shi, shj)]
    else
      ckt
  in
  
  (*
   * copy from share component of r to share wires
   *)
  let rec copyshwires (t:typ) (i:int) (si:int) (ckt:circuit) :(circuit * int * int) =
    let sz = size_env t in
    match t with
      | T_nat
      | T_sum(_)
      | T_wire(_)
      | T_ps(_)
      | T_bool
      | T_unit -> ckt, i + sz, si

      | T_row(l) -> 
	let sortedl = List.map (fun (f, t) -> (f, t.data)) (sortfields l) in
	
	List.fold_left (fun (c, i', si') (_, ftyp) ->
	  copyshwires ftyp i' si' c
	) (ckt, i, si) sortedl

      | T_sh(_, tnd) ->
	if sz > 0 then
	  ckt @ [Copy((si, si + sz - 1), (i, i + sz - 1))], i + sz, si + sz
	else
	  ckt, i, si
      | _ ->
	Pretty.pp_typ t; print_newline ();
	raise (CGenError "Copyshwires: unexpected type")
  in

  let (cktwithshcopy, _, _) = copyshwires t (fst r) shi cktwithsh in

  let rec copyotherwires (princs:string list) (t:typ) (i:int) (wmap:int StringMap.t) (ckt:circuit) :(circuit * int * int StringMap.t) =
    let sz = size_env t in
    match t with
      | T_nat ->
	(* copy this nat to all principals output wires *)
	let (ckt', wmap') = List.fold_left (fun (c, w) princ ->
	  let i' = StringMap.find princ w in
	  c @ [Copy((i', i' + sz - 1), (i, i + sz - 1))],	  
	  StringMap.add princ (i' + sz) w
	) (ckt, wmap) princs in
	(ckt', i + sz, wmap')

      | T_bool ->
	(* copy this bool to all principals output wires *)
	let (ckt', wmap') = List.fold_left (fun (c, w) princ ->
	  let i' = StringMap.find princ w in
	  c @ [Copy((i', i' + sz - 1), (i, i + sz - 1))],	  
	  StringMap.add princ (i' + sz) w
	) (ckt, wmap) princs in
	(ckt', i + sz, wmap')

      | T_ps(_) ->
	(* copy this princ to all principals output wires *)
	let (ckt', wmap') = List.fold_left (fun (c, w) princ ->
	  let i' = StringMap.find princ w in
	  c @ [Copy((i', i' + sz - 1), (i, i + sz - 1))],	  
	  StringMap.add princ (i' + sz) w
	) (ckt, wmap) princs in
	(ckt', i + sz, wmap')

      | T_row(l) ->
	let sortedl = List.map (fun (f, t) -> (f, t.data)) (sortfields l) in
	
	List.fold_left (fun (c, i', w) (_, ftyp) ->
	  copyotherwires princs ftyp i' w c
	) (ckt, i, wmap) sortedl

      (*| T_sum(_) ->
	let (ckt', wmap') = List.fold_left (fun (c, w) princ ->
	  let i' = StringMap.find princ w in
	  c @ [Copy((i', i' + sz - 1), (i, i + sz - 1))],	  
	  (StringMap.add princ (i' + sz) w)
	) (ckt, wmap) princs in
	(ckt', i + sz, wmap')*)
	  
      | T_wire(vnd, typnd) ->
	let sortedl = sortprincs (princstolist vnd) in

	List.fold_left (fun (c, i', w) princnd ->
	  copyotherwires [princnd.data] typnd.data i' w c
	) (ckt, i, wmap) sortedl
	
      | T_unit -> ckt, i, wmap

      | T_sh(_, tnd) -> ckt, (i + sz), wmap

      | _ -> raise Not_found
  in
  
  let (finalckt, _, _) =  copyotherwires princs t (fst r) wmap cktwithshcopy in
  finalckt

(*
 * generate circuit for this value,
 * return the circuit and range of wires for this value
 *)
let rec genv (env:(string * wrange) list) (vnd:value_nd) :(circuit * wrange) =
  match vnd.data with
    | V_var(varnd) -> [], lookupenv env (varname varnd)

    | V_unit -> [], unitrange

    | V_nat(n) ->
      let r = walloc.getn natsize in
      [Const(r, n)], r

    | V_bool(b) ->
      let bit = if b then 1 else 0 in
      let r = walloc.getn (size_env T_bool) in
      [Const(r, bit)], r
	
    (*| V_inj(consnd, [injvnd]) ->
      let (c, r) = genv env injvnd in
      
      (* support for only Left t + Right t *)
      let getinjbit consnd = match consnd.data with
	| Cons(id) ->
	  if id = "Left" then 0
	  else if id = "Right" then 1
	  else raise (CGenError "Unimplemented sum constructor in getinjbit")
      in
      let b = getinjbit consnd in

      let injvsz = rsize r in
      let (i, j) = walloc.getn (size_env vnd.info) in

      let c1 = Const((i, i), b) in
      let c2, c3 = 
	if injvsz > 0 then
	  [Copy((i + 1, i + injvsz), r)],
	  if i + injvsz = j then [] else [Const((i + injvsz + 1, j), 0)]
	else
	  [], []
      in
      (c @ [c1] @ c2 @ c3, (i, j))*)

    | V_row(l) ->
      let sortedl = sortfields l in
      let subckts = List.map (fun (_, fvnd) -> genv env fvnd) sortedl in
      
      let sz = size_env vnd.info in

      if sz > 0 then
	let (i, j) = walloc.getn sz in

	let copy_field (c, curr) (_, r) =
	  let n = rsize r in
	  if n > 0 then
	    c @ [Copy((curr, curr + n - 1), r)], curr + n
	  else
	    c, curr
	in
	let (ckt, _) = List.fold_left copy_field (List.concat (fst (List.split subckts)), i) subckts in      
	(ckt, (i, j))
      else
	[], unitrange
	  	 
    | V_princ({data=pname}) ->
      let n = Global.p_to_n pname in
      let r = walloc.getn natsize in
      [Const(r, n)], r

    | V_paren(v) ->
      genv env v

    | _ ->
      Pretty.pp_value_nd vnd; print_newline ();
      raise (CGenError "Unimplemented value in genv")

(*
 * generate circuit for expression,
 * return circuit, wrange
 *)
let rec genex (env:(string * wrange) list) (exnd:expr_nd) :(circuit * wrange) =
  (*print_string "gen ckt for: ";
  pp_expr_nd exnd;
  print_newline ();*)
  match exnd.data with
    | E_value(vnd) -> genv env vnd

    | E_cond(e1, e2, e3) ->
      (*print_string "Cond, e1 = "; pp_expr_nd e1; print_string ", e2 = ";
      pp_expr_nd e2; print_string ", e3 = "; pp_expr_nd e3; print_newline ();*)
      let (ckt1, r1) = genex env e1 in
      let (ckt2, r2) = genex env e2 in
      let (ckt3, r3) = genex env e3 in

      let (ckt, r) =
	if rsize r2 > 0 then
	  let r = walloc.getn (rsize r2) in
	  [Mux(r, r3, r2, r1)], r
	else
	  [], unitrange
      in
      ckt1 @ ckt2 @ ckt3 @ ckt, r
      
    (*| E_case(cexnd, [ (consnd1, [pnd1], e1); (consnd2, [pnd2], e2) ]) ->
      let (x1, x2) = match (pnd1.data, pnd2.data) with
	| P_var(x), P_var(y) -> x, y
	| _ -> raise (CGenError "Case pattern not Var(_) in genex")
      in
      
      let (sz1, sz2) = match cexnd.info with
	| T_sum([_, [t1]; _, [t2]]) -> size_env t1.data, size_env t2.data
	| _ -> raise (CGenError "Case value non two-sum type in genex")
      in
      
      let (cktv, (i, j)) = genex env cexnd in
      assert(i = j);
      
      let env1 =
	if sz1 > 0 then
	  (varname x1, (i + 1, i + sz1))::env
	else
	  (varname x1, unitrange)::env
      in
      let (ckt1, r1) = genex env1 e1 in

      let env2 =
	if sz2 > 0 then
	  (varname x2, (i + 1, i + sz2))::env
	else
	  (varname x2, unitrange)::env
      in
      let (ckt2, r2) = genex env2 e2 in
      
      let (ckt, r) =
	if rsize r1 > 0 then
	  let r = walloc.getn (rsize r1) in
	  [Mux(r, r1, r2, (i, i))], r
	else
	  [], unitrange
      in

      (cktv @ ckt1 @ ckt2 @ ckt, r)*)

    | E_proj(fldnd, rexnd) ->
      let l = match rexnd.info with
	| T_row(l1) -> l1
	| _ -> raise (CGenError "Record projection not from a record type in genex")
      in

      let Field(fname) = fldnd.data in
      
      let sortedl = sortfields l in
      let (index, typnd) = getfieldindex sortedl fname in
      let sz = size_env typnd.data in

      let prevflds = firstn sortedl (index - 1) in
      let offset = size_env (T_row(prevflds)) in
      
      let (ckt1, (i, j)) = genex env rexnd in
      
      if sz > 0 then
	(ckt1, (i + offset, i + offset + sz - 1))
      else
	(ckt1, unitrange)
	
    | E_let(patnd, _, e1, e2) ->
      let (ckt1, r1) = genex env e1 in
      
      let env1 = (patvarname patnd, r1)::env in
      let (ckt2, r2) = genex env1 e2 in

      (ckt1 @ ckt2, r2)

    | E_natop(opnd, e1, e2) ->
      let (ckt1, r1) = genex env e1 in
      let (ckt2, r2) = genex env e2 in
   
      let r =
	if opnd.data = Natop_plus || opnd.data = Natop_sub || opnd.data = Natop_mult || opnd.data = Natop_div then
	  walloc.getn natsize
	else
	  walloc.getn boolsize
      in
      
      (ckt1 @ ckt2 @ [Gate(opnd.data, r, r1, r2)], r)
	
    | E_wire(e1, e2) ->
      let (vprincsnd, vnd) = match (e1.data, e2.data) with
	| E_value(v1), E_value(v2) -> v1, v2
	| _ -> raise (CGenError "Non ANF form in E_wire in genex")
      in
      let pndl = princstolist vprincsnd in
      
      let (ckt1, r1) = genv env vnd in
      
      let totalsz = (List.length pndl) * (rsize r1) in

      if totalsz > 0 then
	let (i, j) = walloc.getn totalsz in
	
	let f (c, curr) _ =
	  let i1 = i + ((curr - 1) * rsize r1) in
	  let j1 = i1 + rsize r1 - 1 in
	  c @ [Copy((i1, j1), r1)], curr + 1
	in
	fst (List.fold_left f (ckt1, 1) pndl), (i, j)
      else
	ckt1, unitrange

    | E_wproj(e1, e2) ->
      let (vnd1, vnd2) = match (e1.data, e2.data) with
	| E_value(v1), E_value(v2) -> v1, v2
	| _ -> raise (CGenError "Non ANF form in E_wproj in genex")
      in
      
      let pnd = match vnd2.data with
	| V_princ(p) -> p
	| _ -> raise (CGenError "Non closed princ in E_wproj in genex")
      in

      let (vprincsnd, typnd) = match vnd1.info with
	| T_wire(v, t) -> (v, t)
	| _ -> raise (CGenError "Projection from a non wire type in genex")
      in
      
      let (ckt1, (i, _)) = genv env vnd1 in
      let sz = size_env typnd.data in
      
      if sz > 0 then
	let offset = getprincoffset (sortprincs (princstolist vprincsnd)) pnd.data in
	
	let i1 = i + (offset - 1) * sz in
	let j1 = i1 + sz - 1 in

	ckt1, (i1, j1)
      else
	ckt1, unitrange

    | E_wcat(e1, e2) ->
      (*let (vnd1, vnd2) = match (e1.data, e2.data) with
	| E_value(v1), E_value(v2) -> v1, v2
	| _ -> raise (CGenError "Non ANF form in E_wcat in genex")
      in*)
      
      let (vprincsnd1, vprincsnd2, vprincsnd3, typnd) = match (e1.info, e2.info, exnd.info) with
	|T_wire(v1, t), T_wire(v2, _), T_wire(v3, _) -> (v1, v2, v3, t)
	| _ ->
	  raise (CGenError "Ill typed wire concatenation in genex")
      in
      
      let (ckt1, (i1, _)) = genex env e1 in
      let (ckt2, (i2, _)) = genex env e2 in

      let sz = size_env typnd.data in
      
      if sz > 0 then	
	let sortedpndl1 = sortprincs (princstolist vprincsnd1) in
	let sortedpndl2 = sortprincs (princstolist vprincsnd2) in
	let sortedpndl3 = sortprincs (princstolist vprincsnd3) in

	let (i3, j3) = walloc.getn (sz * List.length sortedpndl3) in

	let f (c, curr) pnd =
	  let (l, i) =
	    if existsprinc sortedpndl1 pnd.data then sortedpndl1, i1
	    else sortedpndl2, i2
	  in
	  let offset = getprincoffset l pnd.data in
	  let i4 = i + (offset - 1) * sz in
	  let j4 = i4 + sz - 1 in
	  (c @ [Copy((curr, curr + sz - 1), (i4, j4))], curr + sz)
	in
	fst (List.fold_left f (ckt1 @ ckt2, i3) sortedpndl3), (i3, j3)
      else
	ckt1 @ ckt2, unitrange

    | E_sh(e) -> genex env e

    | E_comb(e) -> genex env e

    | E_paren(e) -> genex env e
	
    | _ -> Pretty.pp_expr_nd exnd; print_newline (); raise (CGenError "Unimplemented expression in genex")

(*
 * given a circuit, return a boolean circuit
 * the constructions are in the GMW paper
 *)
let getbooleanckt elt =
  let get_add_circuit r1 r2 r3 =
    let l1 = rangetolist r1 in
    let l2 = rangetolist r2 in
    let l3 = rangetolist r3 in

    let f (ckt, out, c) b1 b2 =
      let (t1, _) = walloc.getn 1 in
      let g1 = XOR(t1, b1, c) in
      let (t2, _) = walloc.getn 1 in
      let g2 = XOR(t2, b2, c) in
      let (t3, _) = walloc.getn 1 in
      let g3 = AND(t3, t1, t2) in
      let (c1, _) = walloc.getn 1 in
      let g4 = XOR(c1, t3, c) in
      let (t4, _) = walloc.getn 1 in
      let g5 = XOR(t4, b1, b2) in
      let (s, _) = walloc.getn 1 in
      let g6 = XOR(s, t4, c) in
      (*(ckt @ [g1; g2; g3; g4; g5; g6], out @ [s], c1)*)
      g6::g5::g4::g3::g2::g1::ckt, s::out, c1
    in

    (*let (bckt, out, _) = List.fold_left2 f ([], [], wirezero) l1 l2 in*)
    let (rbckt, rout, _) = List.fold_left2 f ([], [], wirezero) l1 l2 in	
    let (bckt, out) = List.rev_append rbckt [], List.rev_append rout [] in
    
    (*let f ckt b1 b2 = ckt @ [copy b1 b2] in*)
    let f ckt b1 b2 = (copy b1 b2)::ckt in
    let l = List.rev_append (List.fold_left2 f bckt l3 out) [] in
    l
  in

  match elt with
    | Input(s, r) -> [INPUT(s, r)]
    | Output(s, r) -> [OUTPUT(s, r)]
    | ShInput(r) -> [SHINPUT(r)]
    | ShOutput(r) -> [SHOUTPUT(r)]
    | Copy(r1, r2) ->
      if rsize r1 > 0 then
	let l1 = rangetolist r1 in
	let l2 = rangetolist r2 in
	
	(*List.fold_left2 (fun c i1 i2 -> c @ [copy i1 i2]) [] l1 l2*)
	List.rev_append (List.fold_left2 (fun c i1 i2 -> (copy i1 i2)::c) [] l1 l2) []
      else
	[]
    | Const(r, n) ->
      if rsize r > 0 then
	let l1 = rangetolist r in
	let l2 =
	  if rsize r = natsize then
	    inttobin n
	  else
	    [n]	    
	in
	
	(*List.fold_left2 (fun c i1 i2 -> c @ [copy i1 i2]) [] l1 l2*)
	List.rev_append (List.fold_left2 (fun c i1 i2 -> (copy i1 i2)::c) [] l1 l2) []
      else
	[]      
    | Mux(r3, r1, r2, r4) ->    
      let l1 = rangetolist r1 in
      let l2 = rangetolist r2 in
      let l3 = rangetolist r3 in

      let f (c, out) b1 b2 =
	let t1 = wireone in
	let (t2, _) = walloc.getn 1 in
	let g1 = XOR(t2, t1, (fst r4)) in
	let (t3, _) = walloc.getn 1 in
	let g2 = XOR(t3, b1, b2) in
	let (t4, _) = walloc.getn 1 in
	let g3 = AND(t4, t2, t3) in
	let (t5, _) = walloc.getn 1 in
	let g4 = XOR(t5, t4, b2) in
	(*(c @ [g1; g2; g3; g4], out @ [t5])*)
	g4::g3::g2::g1::c, t5::out
      in

      (*let (bckt, out) = List.fold_left2 f ([], []) l1 l2 in*)
      let (rbckt, rout) = List.fold_left2 f ([], []) l1 l2 in
      let (bckt, out) = List.rev_append rbckt [], List.rev_append rout [] in
      
      (*let f ckt b1 b2 = ckt @ [copy b1 b2] in*)
      let f ckt b1 b2 = (copy b1 b2)::ckt in
      List.rev_append (List.fold_left2 f bckt l3 out) []

    | Gate(op, r3, r1, r2) ->
      match op with
	| Natop_plus ->
	  get_add_circuit r1 r2 r3

	| Natop_div ->
	  (* ignore r2 *)

	  (*
	   * l1 is the list we are copying from, l2 is the list we are copying to
	   *)
	  let rec f l1 l2 ckt =
	    if l1 = [] then
	      ckt @ [copy (List.hd l2) 0]
	    else
	      f (List.tl l1) (List.tl l2) (ckt @ [copy (List.hd l2) (List.hd l1)])
	  in

	  f (List.tl (rangetolist r1)) (rangetolist r3) []
	  
	    
	| Natop_mult -> 
	  let l1 = rangetolist r1 in
	  let l2 = rangetolist r2 in
	  let l3 = rangetolist r3 in

	  (*
	   * ckt is the circuit to which we append to
	   * l is the list of wire ranges that we need to add later on
	   * index is the current bit index in l2 (starts from 0)
	   * b is the actual bit
	   *)	   
	  let f (ckt, l, index) b =
	    (* allocate nat width number of wires *)
	    let r' = walloc.getn natsize in
	    let l' = rangetolist r' in
	    
	    (*
	     * iterate over l2
	     * if i < index, pass on, copy 0 to corresponding bit in l'
	     * if i >= index, add an AND gate with inp as b and l2[curr]
	     * copy it to corresponding bit in l'
	     *)
	    let rec iter (_l2: int list) (_l': int list) (i: int) (_ckt: booleanckt list) =
	      if i = natsize then
		_ckt
	      else if i < index then
		iter _l2 (List.tl _l') (i + 1) (_ckt @ [(copy (List.hd _l') 0)])
	      else
		iter (List.tl _l2) (List.tl _l') (i + 1) (_ckt @ [(AND(List.hd _l', b, List.hd _l2))])
	    in

	    (ckt @ (iter l2 l' 0 []), l @ [r'], index + 1)
	  in
	  
	  let (rowckts, rlist, _) = List.fold_left f ([], [], 0) l1 in

	  (* now we need to have add circuits for ranges in rlist *)
	  
	  let rinit = walloc.getn natsize in
	  let rinitckts = List.map (fun i -> copy i 0) (rangetolist rinit) in
	  
	  let f (ckt, accum) r =
	    let rout = walloc.getn natsize in
	    let _ckt = get_add_circuit accum r rout in
	    (ckt @ _ckt, rout)
	  in

	  let (addckts, accum) = List.fold_left f ([], rinit) rlist in

	  let f ckt b1 b2 = ckt @ [(copy b1 b2)] in

	  let outckts = List.fold_left2 f [] l3 (rangetolist accum) in

	  rowckts @ rinitckts @ addckts @ outckts

	| Natop_sub ->
	  let l1 = rangetolist r1 in
	  let l2 = rangetolist r2 in
	  let l3 = rangetolist r3 in

	  let f (ckt, out, c) b1 b2 =
	    let (t1, _) = walloc.getn 1 in
	    let g1 = XOR(t1, b1, c) in
	    let (t2, _) = walloc.getn 1 in
	    let g2 = XOR(t2, b2, c) in
	    let (t3, _) = walloc.getn 1 in
	    let g3 = AND(t3, t1, t2) in
	    let (c1, _) = walloc.getn 1 in
	    let g4 = XOR(c1, t3, b1) in
	    let (t4, _) = walloc.getn 1 in
	    let g5 = XOR(t4, b1, b2) in
	    let (t5, _) = walloc.getn 1 in
	    let g6 = XOR(t5, t4, c) in
	    let (s, _) = walloc.getn 1 in
	    let g7 = XOR(s, t5, wireone) in
	    (*(ckt @ [g1; g2; g3; g4; g5; g6; g7], out @ [s], c1)*)
	    g7::g6::g5::g4::g3::g2::g1::ckt, s::out, c1
	  in

	  (*let (bckt, out, _) = List.fold_left2 f ([], [], wireone) l1 l2 in*)
	  let (rbckt, rout, _) = List.fold_left2 f ([], [], wireone) l1 l2 in
	  let bckt, out = List.rev_append rbckt [], List.rev_append rout [] in

	  (*let f ckt b1 b2 = ckt @ [copy b1 b2] in*)
	  let f ckt b1 b2 = (copy b1 b2)::ckt in
	  List.rev_append (List.fold_left2 f bckt l3 out) []

	| Natop_gt -> 
	  let l1 = rangetolist r1 in
	  let l2 = rangetolist r2 in

	  let f (ckt, c) b1 b2 =
	    let (t1, _) = walloc.getn 1 in
	    let g1 = XOR(t1, b1, c) in
	    let (t2, _) = walloc.getn 1 in
	    let g2 = XOR(t2, b2, c) in
	    let (t3, _) = walloc.getn 1 in
	    let g3 = AND(t3, t1, t2) in
	    let (c1, _) = walloc.getn 1 in
	    let g4 = XOR(c1, t3, b1) in
	    (*(ckt @ [g1; g2; g3; g4], c1)*)
	    g4::g3::g2::g1::ckt, c1
	  in

	  (*let (bckt, c) = List.fold_left2 f ([], wirezero) l1 l2 in*)
	  let (rbckt, c) = List.fold_left2 f ([], wirezero) l1 l2 in
	  
	  (*bckt @ [copy (fst r3) c]*)
	  List.rev_append ((copy (fst r3) c)::rbckt) []

	| Natop_equals -> 
	  let l1 = rangetolist r1 in
	  let l2 = rangetolist r2 in

	  let f (ckt, c) b1 b2 =
	    let (t1, _) = walloc.getn 1 in
	    let g1 = XOR(t1, b1, b2) in
	    let (t2, _) = walloc.getn 1 in
	    let g2 = copy t2 1 in
	    let (t3, _) = walloc.getn 1 in
	    let g3 = XOR(t3, t1, t2) in
	    let (c1, _) = walloc.getn 1 in
	    let g4 = AND(c1, t3, c) in
	    (*(ckt @ [g1; g2; g3; g4], c1)*)
	    g4::g3::g2::g1::ckt, c1
	  in
	  
	  (*let (bckt, c) = List.fold_left2 f ([], wireone) l1 l2 in*)
	  let (rbckt, c) = List.fold_left2 f ([], wireone) l1 l2 in
	  
	  (*bckt @ [copy (fst r3) c]*)
	  List.rev_append ((copy (fst r3) c)::rbckt) []

(*
 * dumps bit representation of v in the file
 * caller has already taken out value from a wire bundle before calling it
 * also shares are handled separately
 *)
let rec dumpval (t:typ) (v:value) (file:out_channel) :unit =
  let ps s = output_string file s in

  match (t, v) with
  | T_nat, V_nat(n) ->
    let l = inttobin n in
    List.iter (fun b -> ps (string_of_int b); ps " ") l
      
  | T_unit, V_unit -> ()

  | T_row(tl), V_row(vl) ->
    let stl, svl = sortfields tl, sortfields vl in

    List.iter2 (fun te ve -> dumpval (snd te).data (snd ve).data file) stl svl

  | T_bool, V_bool(b) -> if b then ps "1" else ps "0"

  (*| T_sum([consnd1, [t1]; consnd2, [t2]]), V_inj(consnd, [vnd]) ->
    begin
      let tv =
	if consname consnd1 = consname consnd then
	  t1.data
	else
	  t2.data
      in
      
      let diff = size_env t - (size_env tv + 1) in
      
      begin
	if consname consnd = "Left" then
	  ps "0 "	    
	else
	  ps "1 "
      end;

      dumpval tv vnd.data file;
      
      for i = 1 to diff do ps "0 " done;
    end*)

  | T_ps(_), V_princ({data = pname}) ->
    let n = Global.p_to_n pname in
    let l = inttobin n in
    List.iter (fun b -> ps (string_of_int b); ps " ") l

  | _ -> raise (CGenError "Unimplemented dumping of value type")

(*
 * parse gmw output into value
 * parse a value of type t from the arr (bits) and sharr (shares),
 * and return the remaining bits array and shares array
 *)
let rec parseoutput (arr:string array) (sharr:char array) (princ:string) (t:typ) :(value_nd * string array * char array) =
  let astnd d = { prov = dprov; data = d; info = dummyinfo } in
  let tastnd d t = { prov = dprov; data = d; info = t } in
  
  match t with
    | T_nat ->    
      let n = bintoint (Array.to_list (Array.sub arr 0 natsize)) in
      tastnd (V_nat(n)) t,
      Array.sub arr natsize (Array.length arr - natsize), sharr

    | T_unit -> tastnd V_unit t , arr, sharr

    | T_bool ->
      let b = Array.get arr 0 in
      tastnd (V_bool(b = "1")) t,
      Array.sub arr boolsize (Array.length arr - boolsize), sharr

    | T_row(l) ->
      let sortedl = sortfields l in

      let (arr', sharr', vlist) = List.fold_left (fun (a, s, l) (_, t1) ->
	let (v, a1, s1) = parseoutput a s princ t1.data in
	(a1, s1, l @ [v])
      ) (arr, sharr, []) sortedl in
      
      let fvl = List.map2 (fun (fldnd, _) vnd -> (fldnd, vnd)) sortedl vlist in
      
      tastnd (V_row(fvl)) t, arr', sharr'
	
    | T_wire(vprincsnd, typnd) ->
      if existsprinc (princstolist vprincsnd) princ then
	let (vnd, a, s) = parseoutput arr sharr princ typnd.data in
	tastnd (V_wires[astnd princ, vnd]) t, a, s
      else
	tastnd (V_wires([])) t, arr, sharr

    (*| T_sum([consnd1, [t1]; consnd2, [t2]]) ->
      let b = Array.get arr 0 in
      let (consnd, vt) =
	if b = "0" then { prov = dprov; data = Cons("Left"); info = dummyinfo }, t1
	else { prov = dprov; data = Cons("Right"); info = dummyinfo }, t2
      in
      
      let (vnd, a, s) = parseoutput (Array.sub arr 1 (Array.length arr - 1)) sharr princ vt.data in
      
      { prov = dprov; data = V_inj(consnd, [vnd]); info = t }, a, s*)

    | T_sh(_, tnd) ->
      let sz = size_env tnd.data in
      if sz > 0 then	
	tastnd (V_sh(Bytes.init sz (fun i -> sharr.(i)))) t, arr, (Array.sub sharr sz (Array.length sharr - sz))
	(*tastnd (V_sh(Array.to_list (Array.sub sharr 0 sz))) t, arr, (Array.sub sharr sz (Array.length sharr - sz))*)
      else
	tastnd (V_sh(Bytes.empty)) t, arr, sharr
	(*tastnd (V_sh([])) t, arr, sharr*)

    | T_ps(_) ->
      let n = bintoint (Array.to_list (Array.sub arr 0 natsize)) in
      let s = Global.n_to_p n in
      tastnd (V_princ(astnd s)) t,
      Array.sub arr natsize (Array.length arr - natsize), sharr

    | _ -> raise (CGenError "Unimplemented output parser for typ")

(*
 * run the mpc.exe program, and return the value from the secure computation
 *)
let rec rungmw (princ:string) (conff:string) (outf:string) (shoutf:string) (outtyp:typ) :(value_nd * float * float) =
  
  let t_begin = Unix.gettimeofday () in

  begin
    if !gmwsockset = false then
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let gmwaddr = Unix.ADDR_INET(Unix.inet_addr_of_string !Global.gmwaddr, !Global.gmwport) in
      let _ = Unix.connect s gmwaddr in
      gmwsock := s; gmwsockset := true
    else
      ()
  end;

  let written = Unix.write !gmwsock conff 0 (String.length conff) in
  if not (written  = String.length conff) then
    raise (CGenError "Cannot write to the main server socket")
  else
    let statusstr = String.make 5 '.' in
    (*print_string "Waiting for GMW"; print_newline ();*)
    let statusn = Unix.read !gmwsock statusstr 0 4 in
    assert(statusn = 4);
    (*let status = int_of_string (String.sub statusstr 0 statusn) in*)
    (*print_string "status = "; print_int status; print_newline ();*)
    (*let status = 0 in*)
    (*print_string "GMW returned"; print_newline ();*)
    if true then
      let t_donegmw = Unix.gettimeofday () in

      let inc = open_in outf in
      let shinc = open_in shoutf in
      
      let rec helper l =
	try
	  let c = input_char inc in
	  if c = '0' then
	    helper (l @ ["0"])
	  else if c = '1' then
	    helper (l @ ["1"])
	  else
	    helper l
	with
	  | End_of_file -> close_in inc; l
      in
      
      let l = helper [] in

      let rec helper l =
	try
	  let c = input_char shinc in
	  helper (l @ [c])
	with
	  | End_of_file -> close_in shinc; l
      in
      
      let shl = helper [] in

      (*print_string "parsing output per type: "; Pretty.pp_typ outtyp; print_newline ();*)
      let(v, _, _) = parseoutput (Array.of_list l) (Array.of_list shl) princ outtyp in
      let t_parsedoutput = Unix.gettimeofday () in
      
      v, t_donegmw -. t_begin, t_parsedoutput -. t_donegmw
    else
    (*raise Not_found*)
      rungmw princ conff outf shoutf outtyp

(*
 * run a secure block, called from opsem
 *)
let runsecblk (princ:string) (princs:string list) (renv:env) (tenv:(string * typ) list) (e:expr_nd) :value_nd =

  (*print_string "Running secure block:"; print_newline ();
  pp_expr_nd e;
  print_newline ();
  print_string "with type: ";
  pp_typ e.info;
  print_newline ();*)

  (*refenv := renv;*)
  walloc.reset ();
  
  let sortedprincs = List.sort (fun s1 s2 -> compare s1 s2) princs in
  let map, _ =
    List.fold_left (fun (m, id) s -> (StringMap.add s id m, id + 1)) (StringMap.empty, 0) sortedprincs
  in
  
  let pnobang = String.sub princ 1 (String.length princ - 1) in
  
  let prefix = "" in
  let inpf = prefix^"in_"^pnobang^".txt" in
  let shinpf = prefix^"shin_"^pnobang^".txt" in
  let outf = prefix^"out_"^pnobang^".txt" in
  let shoutf = prefix^"shout_"^pnobang^".txt" in
  let conff = prefix^"config"^(String.sub pnobang 0 1)^".txt" in
  let circf = prefix^"circuit_"^pnobang^".txt" in
  
  let fout = open_out inpf in  

  let shfout = open_out shinpf in

  (* dump this input and collect the size of input for this party *)
  let dumpinp n (varname, t) =
    (*print_string ("\ntype env varname :  " ^ varname ^ " : "); Pretty.pp_typ t; print_newline ();*)
    let rec getmyvalue = function
      | ({data = princ1}, v1)::l -> if princ1 = princ then v1 else getmyvalue l
      | _ -> raise (CGenError "Cannot find my value in this wire bundle")
    in
      
    match t with
      | T_wire(vprincsnd, typnd) ->
	if existsprinc (princstolist vprincsnd) princ then
	  let vnd = (Env.find (Var(varname)) renv).value in
	  let _ = match vnd.data with
	    (*| V_wires([_, vnd1]) ->
	      dumpval typnd.data vnd1.data fout;*)
	    | V_wires(l) ->
	      (*print_string "value: "; pp_value_nd (getmyvalue l); print_newline ();*)
	      dumpval typnd.data (getmyvalue l).data fout;
	    | _ -> Pretty.pp_value_nd vnd; print_newline (); raise (CGenError "Non wire value for a wire type")
	  in
	  n + size_env typnd.data
	else
	  n

      | T_sh(_, typnd) ->
	let vnd = (Env.find (Var(varname)) renv).value in
	begin
	  match vnd.data with
	    | V_sh(b) ->
	      Bytes.iter (fun c -> output_char shfout c) b;
	      n    (* share input don't count in input size *)

	    | _ -> raise (Not_found)
	end

      | _ -> raise (CGenError "Unimplemented non-wire type input to sec block")
  in
  
  let startdumpinput = Unix.gettimeofday () in

  let n = List.fold_left dumpinp 0 tenv in
  
  close_out fout;
  close_out shfout;
  (*print_string "dumped input file"; print_newline ();*)

  let t_inputdumped = Unix.gettimeofday () in

  let (inpckts, wenv) = setupinp princs tenv in

  (*print_string "setting input done"; print_newline ();*)

  let (ckt, r) = genex wenv e in

  (*print_string "circuit generation done"; print_newline ();*)

  let outckt = setupout princs r e.info renv in

  (*print_string "setting output done"; print_newline ();*)

  let aggckt = inpckts @ ckt @ outckt in
    
  let bckt = List.flatten (List.map (fun elt -> getbooleanckt elt) aggckt) in

  let t_cktgenerated = Unix.gettimeofday () in

  let fout = open_out circf in

  let numgates = walloc.currid () + 1 in
  
  (*print_string "dumping circuit: "; print_newline (); flush stdout;
  Cktlib.printckt aggckt; print_newline ();*)
  let _ = Gmwimpl.dumpgmwckt map bckt numgates fout in
  (*print_string "dumped circuit"; print_newline (); flush stdout;*)

  let t_cktdumped = Unix.gettimeofday () in
  
  close_out fout;
  
  let fout = open_out conff in

  let ps s = output_string fout s in
  (*let psi n = output_string fout (string_of_int n) in
  
  ps "num_parties "; ps (string_of_int (List.length princs)); ps "\n";*)

  (*ps "pid "; ps (string_of_int id); ps "\n";*)

  (*ps "address-book addresses.txt\n";*)
  
  ps ("load-circuit " ^ circf ^ "\n");

  ps ("input " ^ inpf ^ "\n");

  ps ("output " ^ outf ^ "\n");
  
  ps ("shinput " ^ shinpf ^ "\n");

  ps ("shoutput " ^ shoutf ^ "\n");

  (*ps ("perfoutput " ^ perff ^ "\n");*)

  ps "num_input "; ps (string_of_int n); ps "\n";
  
  close_out fout;
  (*print_string "dumped config file"; print_newline ();*)

  let t_configfiledumped = Unix.gettimeofday () in
  
  let (v, t_rungmw, t_parseoutput) = rungmw princ conff outf shoutf e.info in
  
  gatherperformance.add
    {
      t_dumpinput   = t_inputdumped -. startdumpinput;
      t_cktgen      = t_cktgenerated -. t_inputdumped;
      t_cktdump     = t_cktdumped -. t_cktgenerated;
      t_configdump  = t_configfiledumped -. t_cktdumped;
      t_gmwrun      = t_rungmw;
      t_parseoutput = t_parseoutput;
      num_gates     = numgates;
    };
  v
