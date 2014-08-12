open Ast
open Ast.Library
open Ast.Pretty

exception TypError of string

type envbnd = { t : typ; p : place_nd }

type tenv = (string * envbnd) list

let pp_tenv (env:tenv) :unit =
  List.iter (fun (s, bnd) ->
    print_string (s^" : { "); pp_typ bnd.t; print_string "; "; pp_place_nd bnd.p;
    print_string "},  ";
  ) env;
  print_newline ()

let err prov (s:string) =
  raise (TypError ((Global.Prov.sprint_prov "" prov) ^ ": " ^ s))

let errf prov (f:unit -> unit) =
  print_string (Global.Prov.sprint_prov "" prov); print_string ": ";
  f ();
  print_newline ();
  raise (TypError "")

let astnd p d = { prov = p; data = d; info = T_unknown }

let tastnd p d t = { prov = p; data = d; info = t; }

let lookupenv (env:tenv) (varnd:var_nd) :envbnd =
  let s = varname varnd in
  try
    snd (List.find (fun elt -> (fst elt) = s) env)
  with
    | Not_found -> err varnd.prov ("Variable not found: " ^ s)

let isprinctyp (v:value_nd) :bool = match v.info with
  | T_ps(_) -> true

  | T_unknown ->
    errf v.prov (fun _ ->
      print_string "Value "; pp_value_nd v; print_string " has unknown type")

  | _ -> false

let getreft (t:typ) prov :refine_nd = match t with
  | T_ps(r) -> r

  | _ ->
    errf prov (fun _ ->
      print_string "Type "; pp_typ t; print_string " is not a T_ps type")

let getref (v:value_nd) :refine_nd = getreft v.info v.prov

let mode plc = match plc.data with
  | Pl_top -> Par
  | Pl_ps({data = m}, _) -> m

let satref (r1:refine_nd) (r2:refine_nd) (env:tenv) :bool =  
  Refchk.verify r1 r2 (fun varnd -> (lookupenv env varnd).t)

let issubsetval (r1:refine_nd) (v2:value_nd) (env:tenv) :bool =
  let r2 = astnd v2.prov (R_subeq(v2)) in
  satref r1 r2 env

let issubsetplc (r:refine_nd) (plc:place_nd) (env:tenv) :bool =
  match plc.data with
    | Pl_top -> true
    | Pl_ps(_, vnd) -> issubsetval r vnd env

let veqprincs (r1:refine_nd) (v2:value_nd) (env:tenv) :bool =
  let r2 = astnd v2.prov (R_eq(v2)) in
  satref r1 r2 env

let eqplc (p1:place_nd) (p2:place_nd) (env:tenv) :bool =
  match p1.data, p2.data with
    | Pl_top, Pl_top -> true
    | Pl_ps(m1, v1), Pl_ps(m2, v2) -> m1.data = m2.data && veqprincs (getref v1) v2 env
    | _ -> false

let rec eqeff (eff1:eff_nd) (eff2:eff_nd) (env:tenv) :bool =
  match eff1.data, eff2.data with
    | Ef_emp, Ef_emp -> true
    | Ef_place(p1), Ef_place(p2) -> eqplc p1 p2 env
    | Ef_cat(eff1, eff2), Ef_cat(eff3, eff4) -> eqeff eff1 eff3 env && eqeff eff2 eff4 env
    | _ -> false

let rec subtyp (t1:typ) (t2:typ) (plc:place_nd) (env:tenv) :bool = match t1, t2 with
  | T_unit,   T_unit
  | T_nat,    T_nat
  | T_proc,   T_proc
  | T_string, T_string
  | T_bool,   T_bool -> true

  | T_sum(l1), T_sum(l2) ->
    begin
      try
	let fold_tl b t1 t2 = b && subtyp t1.data t2.data plc env in
	let fold_l b (c1, tl1) (c2, tl2) =
	  b && (consname c1 = consname c2)
	  && (List.fold_left2 fold_tl true tl1 tl2)
	in
	List.fold_left2 fold_l true l1 l2
      with
	| Invalid_argument(_) -> false
    end

  | T_row(l1), T_row(l2) ->
    begin
      try
	let fold_l b (f1, t1) (f2, t2) =
	  b && fldname f1 = fldname f2 && subtyp t1.data t2.data plc env
	in
	List.fold_left2 fold_l true (sortfields l1) (sortfields l2)
      with
	| Invalid_argument(_) -> false
    end

  | T_ps(r1), T_ps(r2) -> satref r1 r2 env

  | T_array(t1), T_array(t2) -> eqtyp t1.data t2.data plc env

  | T_arr(varndopt1, arg1, eff1, ret1), T_arr(varndopt2, arg2, eff2, ret2) ->
    begin
      match varndopt1, varndopt2 with
	| None, None ->
	  (* TODO: Effect checking is commented for now (PSI example) *)
	  subtyp arg2.data arg1.data plc env  (*&& eqeff eff1 eff2 env*) && subtyp ret1.data ret2.data plc env
	| Some({data = Var(s1)}), Some({data = Var(s2)}) when s1 = s2 ->
	  subtyp arg2.data arg1.data plc env &&
	    let env1 = (s1, {t = arg2.data; p = plc})::env in
	    (*eqeff eff1 eff2 env1 &&*) subtyp ret1.data ret2.data plc env1
	| _ -> false
    end

  | T_sh(v1, t1), T_sh(v2, t2) -> veqprincs (getref v1) v2 env && eqtyp t1.data t2.data plc env
    
  | T_wire(v1, t1), T_wire(v2, t2) ->
    issubsetval (getref v2) v1 env && subtyp t1.data t2.data plc env

  | (t1, t2) when t1 = T_unknown || t2 = T_unknown ->
    print_string "Warning: Subtyping query for "; pp_typ t1; print_string " and "; pp_typ t2; print_newline ();
    false

  | _ -> false

and eqtyp (t1:typ) (t2:typ) (plc:place_nd) (env:tenv) :bool =
  subtyp t1 t2 plc env && subtyp t2 t1 plc env

let rec styp (x:string) (v:value_nd) (t:typ) :typ = match t with
  | T_unit
  | T_nat
  | T_bool -> t

  | T_row(l) ->
    T_row(List.map
	    (fun (f, typnd) -> (f, astnd typnd.prov (styp x v typnd.data))) l)

  | T_sum(l) ->
    let maptl tl = List.map (fun t -> astnd t.prov (styp x v t.data)) tl in
    let l' = List.map (fun (c, tl) -> (c, maptl tl)) l in
    T_sum(l')

  | T_arr(varndopt, t1, eff, t2) ->
    begin
      match varndopt with
	| None ->
	  T_arr(None,
		astnd t1.prov (styp x v t1.data),
		seff x v eff,
		astnd t2.prov (styp x v t2.data))
	| Some(varnd) when not (varname varnd = x) ->
	  T_arr(varndopt,
		astnd t1.prov (styp x v t1.data),
		seff x v eff,
		astnd t2.prov (styp x v t2.data))
	| _ -> t
    end

  | T_ps(refnd) -> T_ps(sref x v refnd)

  | T_array(t1) -> T_array(astnd t1.prov (styp x v t1.data))

  | T_sh(vnd, tnd) -> T_sh(sval x v vnd, astnd tnd.prov (styp x v tnd.data))

  | T_wire(vnd, tnd) -> T_wire(sval x v vnd, astnd tnd.prov (styp x v tnd.data))

  | _ ->
    print_string "Warning: Could not substitute "; pp_value_nd v; print_string (" for "^x^" in "); pp_typ t; print_newline();
    t

and seff (x:string) (v:value_nd) (eff:eff_nd) :eff_nd = match eff.data with
  | Ef_emp -> eff
  | Ef_place(pnd) -> astnd eff.prov (Ef_place(spl x v pnd))
  | Ef_cat(e1, e2) -> astnd eff.prov (Ef_cat(seff x v e1, seff x v e2))

and spl (x:string) (v:value_nd) (plc:place_nd) :place_nd = match plc.data with
  | Pl_top -> plc
  | Pl_ps(plm, vnd) -> astnd plc.prov (Pl_ps(plm, sval x v vnd))

and sval (x:string) (v:value_nd) (vnd:value_nd) :value_nd =
  match vnd.data with
    | V_princ(_) -> vnd

    | V_ps_lit(l) ->    
      tastnd vnd.prov (V_ps_lit(List.map (fun vnd' -> sval x v vnd') l)) (styp x v vnd.info)

    | V_ps_union(v1, v2) ->
      tastnd vnd.prov (V_ps_union(sval x v v1, sval x v v2)) (styp x v vnd.info)

    | V_paren(v1) ->
      tastnd vnd.prov (V_paren(sval x v v1)) (styp x v vnd.info)

    | V_var(varnd) -> if varname varnd = x then v else vnd

    | _ ->
      errf vnd.prov (fun _ ->
	print_string "Cannot substitute "; pp_value_nd v; print_string (" for "^x^" in "); pp_value_nd vnd)

and sref (x:string) (v:value_nd) (refnd:refine_nd) :refine_nd = match refnd.data with
  | R_true -> refnd
  | R_singl -> refnd
  | R_eq(vnd) -> astnd refnd.prov (R_eq(sval x v vnd))
  | R_subeq(vnd) -> astnd refnd.prov (R_subeq(sval x v vnd))  
  | R_conj(r1, r2) -> astnd refnd.prov (R_conj(sref x v r1, sref x v r2))
  | R_not(r) -> astnd refnd.prov (R_not(sref x v r))

let rec firstorder = function
  | T_unit
  | T_nat
  | T_bool
  | T_ps(_) -> true

  | T_array(t)
  | T_wire(_, t)
  | T_sh(_, t) -> firstorder t.data

  | T_sum(l) ->
    List.fold_left (fun b (_, tl) ->
      b && List.fold_left (fun b' t -> firstorder t.data) b tl
    ) true l

  | T_row(l) ->
    List.fold_left (fun b (_, t) -> b && firstorder t.data) true l

  | t ->
    print_string "Warning: firstorder called for :"; pp_typ t;
    false

let rec secin = function
  | T_arr(_, t1, {data = eff}, t2) -> secin t1.data && empeff eff && secin t2.data
  | t -> firstorder t

and empeff = function
  | Ef_emp -> true
  | Ef_cat(eff1, eff2) -> empeff eff1.data && empeff eff2.data
  | Ef_place(_) -> false

let rec wirefree = function
  | T_unit
  | T_nat
  | T_bool
  | T_ps(_) -> true

  | T_array(t)
  | T_sh(_, t) -> wirefree t.data

  | T_sum(l) ->
    List.fold_left (fun b (_, tl) ->
      b && List.fold_left (fun b' t -> wirefree t.data) b tl
    ) true l

  | T_row(l) ->
    List.fold_left (fun b (_, t) -> b && wirefree t.data) true l

  | T_arr(_, t1, _, t2) -> wirefree t1.data && wirefree t2.data

  | t ->
    print_string "Warning: wirefree called for:"; pp_typ t; print_newline ();
    false

let rec sharefree = function
  | T_unit
  | T_nat
  | T_bool
  | T_ps(_) -> true

  | T_array(t)
  | T_wire(_, t) -> sharefree t.data

  | T_sum(l) ->
    List.fold_left (fun b (_, tl) ->
      b && List.fold_left (fun b' t -> sharefree t.data) b tl
    ) true l

  | T_row(l) ->
    List.fold_left (fun b (_, t) -> b && sharefree t.data) true l

  | T_arr(_, t1, _, t2) -> sharefree t1.data && sharefree t2.data

  | t ->
    print_string "Warning: sharefree called for :"; pp_typ t;
    false

let rec del (p:place_nd) (q:place_nd) (env:tenv) :bool =
  eqplc p q env ||
  match p.data, q.data with
    | Pl_top, _ -> true

    (*| Pl_ps({data = Par}, pvnd), Pl_ps({data = Sec}, qvnd) -> veqprincs (getref pvnd) qvnd env*)
    | Pl_ps({data = Par}, _), Pl_ps({data = Sec}, qvnd)
    | Pl_ps({data = Par}, _), Pl_ps({data = Par}, qvnd) -> issubsetplc (getref qvnd) p env

    | _ -> false

and effdel (p:place_nd) (eff:eff_nd) (env:tenv) :bool = match eff.data with
  | Ef_emp -> true

  | Ef_place(q) -> del p q env

  | Ef_cat(eff1, eff2) -> effdel p eff1 env && effdel p eff2 env

(* well formed functions fill in types info for value nodes in types *)
and wellformedplace (p:place_nd) (plc:place_nd) (env:tenv) :(place_nd * bool) =
  match p.data with
    | Pl_top -> p, true
    | Pl_ps(m, v) ->
      begin
	let tv = typv v plc env in
	match tv.info with
	  | T_ps(_) -> astnd p.prov (Pl_ps(m, tv)), true
	  | _ -> p, false
      end

and wellformedeffect (eff:eff_nd) (plc:place_nd) (env:tenv) :(eff_nd * bool) =
  match eff.data with
    | Ef_emp -> eff, true
    | Ef_place(p) ->
      let tp, b = wellformedplace p plc env in
      astnd eff.prov (Ef_place(tp)), b
    | Ef_cat(eff1, eff2) ->
      let teff1, b1 = wellformedeffect eff1 plc env in
      let teff2, b2 = wellformedeffect eff2 plc env in
      astnd eff.prov (Ef_cat(teff1, teff2)), (b1 && b2)

and normalizeeff (eff:eff_nd) (plc:place_nd) (env:tenv) :eff_nd =
  match eff.data with
    | Ef_emp -> eff
    | Ef_place(p) ->
      begin
	try
	  let tp, b = wellformedplace p plc env in
	  if b then
	    astnd eff.prov (Ef_place(tp))
	  else
	    astnd eff.prov (Ef_place(plc))
	with
	  | _ -> astnd eff.prov (Ef_place(plc))
      end
    | Ef_cat(eff1, eff2) ->
      if eff1.data = Ef_emp then
	normalizeeff eff2 plc env
      else if eff2.data = Ef_emp then
	normalizeeff eff1 plc env
      else	
	astnd eff.prov (Ef_cat(normalizeeff eff1 plc env,normalizeeff eff2 plc env))

and wellformedref (r:refine_nd) (plc:place_nd) (env:tenv) :(refine_nd * bool) = match r.data with
  | R_true
  | R_singl -> r, true

  | R_subeq(v) ->
    begin
      let tv = typv v plc env in
      match tv.info with
	| T_ps(_) -> astnd r.prov (R_subeq(tv)), true
	| _ -> r, false
    end

  | R_eq(v) ->
    begin
      let tv = typv v plc env in
      match tv.info with
	| T_ps(_) -> astnd r.prov (R_eq(tv)), true
	| _ -> r, false
    end

  | R_conj(r1, r2) ->
    let tr1, b1 = wellformedref r1 plc env in
    let tr2, b2 = wellformedref r2 plc env in
    if b1 && b2 then
      astnd r.prov (R_conj(tr1, tr2)), true
    else
      r, false

  | R_not(r1) ->
    let tr1, b = wellformedref r1 plc env in
    if b then
      astnd r.prov (R_not(tr1)), true
    else
      r, false

and wellformedtyp (t:typ) (plc:place_nd) (env:tenv) :(typ * bool) = match t with
  | T_unit
  | T_nat
  | T_string
  | T_proc
  | T_bool -> t, true

  | T_sum(l) ->
    let fold_tl ((tl, b):(typ_nd list * bool)) (t:typ_nd) :(typ_nd list * bool) =
      if not b then
	tl @ [t], false
      else
	let td, b1 = wellformedtyp t.data plc env in
	let b2 = sharefree t.data && wirefree t.data in
	tl @ [astnd t.prov td], b1 && b2
    in
    let fold_ctl (l, b) (c, tl) =
      if not b then
	l @ [c, tl], false
      else
	let tl', b = List.fold_left fold_tl ([], true) tl in
	l @ [c, tl'], b
    in
    let l', b = List.fold_left fold_ctl ([], true) l in
    T_sum(l'), b

  | T_row(l) ->
    let fold_tl ((tl, b):((field_nd * typ_nd) list * bool)) ((f, t):(field_nd * typ_nd)) :((field_nd * typ_nd) list * bool) =
      if not b then
	tl @ [f, t], false
      else
	let td, b1 = wellformedtyp t.data plc env in
	tl @ [f, astnd t.prov td], b1
    in
    let l', b = List.fold_left fold_tl ([], true) l in
    T_row(l'), b

  | T_ps(r) ->
    let r', b = wellformedref r plc env in
    T_ps(r'), b

  | T_arr(varopt, t1, eff, t2) ->
    let t1', b1 = wellformedtyp t1.data plc env in
    if not b1 then
      t, b1
    else
      let env' = match varopt with
	| None -> env
	| Some(varnd) -> (varname varnd, {t=t1.data; p=plc})::env
      in
      let teff, b2 = wellformedeffect eff plc env' in
      if not b2 then
	t, b2
      else
	let t2', b3 = wellformedtyp t2.data plc env' in
	if not b3 then
	  t, b3
	else
	  T_arr(varopt, astnd t1.prov t1', teff, astnd t2.prov t2'), true

  | T_array(t1) ->
    let t1', b = wellformedtyp t1.data plc env in
    T_array(astnd t1.prov t1'), b
      
  | T_sh(v, tnd) ->
    let tv = typv v plc env in
    if not (isprinctyp tv) then
      t, false
    else
      let t', b1 = wellformedtyp tnd.data plc env in
      if not b1 then
	t, b1      
      else
	let b2 = wirefree t' && sharefree t' in
	if not b2 then
	  t, b2
	else
	  T_sh(tv, astnd tnd.prov t'), true

  | T_wire(v, tnd) ->
    let tv = typv v plc env in
    if not (isprinctyp tv) then
      t, false
    else
      let t', b1 = wellformedtyp tnd.data plc env in
      if not b1 then
	t, b1      
      else
	let b2 = wirefree t' && sharefree t' in
	if not b2 then
	  t, b2
	else
	  T_wire(tv, astnd tnd.prov t'), true

  | _ ->
    print_string "Warning: Type "; pp_typ t; print_string " is not well-formed";
    t, false
    
and typv (vnd:value_nd) (plc:place_nd) (env:tenv) :value_nd =
  if not (vnd.info = T_unknown) then
    vnd
  else
    match vnd.data with
      | V_var(varnd) ->
	let s = varname varnd in

	if s = Ast._mevar then
	  (* TODO: check that parallel(p) where singleton p *)
	  tastnd vnd.prov vnd.data T_string
	  
	else
	  let {t = t; p = plc1} = lookupenv env varnd in
	  if not (snd (wellformedtyp t plc env)) then
	    errf vnd.prov (fun _ -> print_string "Type "; pp_typ t;
	      print_string (" for variable "^s^" is not well formed"))
	      
	  else if not (del plc1 plc env) then
	    errf vnd.prov (fun _ -> print_string "Variable "; print_string (s^" is bound at place ");
	      pp_place_nd plc1; print_string " and cannot be used at current place ";
	      pp_place_nd plc)

	  else if mode plc = Sec && not (secin t) then
	    errf vnd.prov (fun _ -> print_string "Sec-in check failed for variable";
	      print_string (s^", type "); pp_typ t; print_string " and place "; pp_place_nd plc)

	  else begin
	    match t with
	      | T_ps(r) ->
		let truerefnd = astnd vnd.prov R_true in
		let refvnd = tastnd vnd.prov vnd.data (T_ps(truerefnd)) in
		let eqrefnd = astnd vnd.prov (R_eq(refvnd)) in
		let conjnd = astnd vnd.prov (R_conj(r, eqrefnd)) in
		tastnd vnd.prov vnd.data (T_ps(conjnd))
		  
	      | _ -> tastnd vnd.prov vnd.data t
	  end

      | V_nat(_) -> tastnd vnd.prov vnd.data T_nat
	
      | V_string(_) -> tastnd vnd.prov vnd.data T_string

      | V_bool(_) -> tastnd vnd.prov vnd.data T_bool

      | V_unit -> tastnd vnd.prov vnd.data T_unit

      | V_row(l) ->
	let (fnds, vnds) = List.split l in
	let tvnds = List.map (fun vnd -> typv vnd plc env) vnds in
	let ftnds = List.map (fun tvnd -> astnd tvnd.prov tvnd.info) tvnds in
	
	tastnd vnd.prov (V_row(List.combine fnds tvnds)) (T_row(List.combine fnds ftnds))
	  
      | V_princ({data=pname}) ->
	let truerefnd = astnd vnd.prov R_true in
	let refvnd = tastnd vnd.prov vnd.data (T_ps(truerefnd)) in
	let refnd = astnd vnd.prov (R_eq(refvnd)) in
	let singlnd = astnd vnd.prov R_singl in
	let conjnd = astnd vnd.prov (R_conj(singlnd, refnd)) in
	tastnd vnd.prov vnd.data (T_ps(conjnd))

      | V_ps_lit(l) ->
	let tl = List.map (fun v -> typv v plc env) l in
	let truerefnd = astnd vnd.prov R_true in
	let refvnd = tastnd vnd.prov (V_ps_lit(tl)) (T_ps(truerefnd)) in
	let refnd = astnd vnd.prov (R_eq(refvnd)) in
	tastnd vnd.prov (V_ps_lit(tl)) (T_ps(refnd))

      | V_ps_union(v1, v2) ->
	let tv1 = typv v1 plc env in
	let tv2 = typv v2 plc env in
	
	let truerefnd = astnd vnd.prov R_true in
	let refvnd = tastnd vnd.prov (V_ps_union(tv1, tv2)) (T_ps(truerefnd)) in
	let refnd = astnd vnd.prov (R_eq(refvnd)) in
	tastnd vnd.prov (V_ps_union(tv1, tv2)) (T_ps(refnd))

      | V_paren(v) ->
	let tv = typv v plc env in
	tastnd vnd.prov (V_paren(tv)) tv.info

      | V_proc(_) -> tastnd vnd.prov vnd.data T_proc

      | _ ->
	errf vnd.prov (fun _ ->
	  print_string "Type checker not implemented for value "; pp_value_nd vnd)

let rec typex (exnd:expr_nd) (plc:place_nd) (env:tenv) :(expr_nd * eff_nd * tenv) =

  let combeff2 eff1 eff2 prov = astnd prov (Ef_cat(eff1, eff2)) in
  let combeff3 eff1 eff2 eff3 prov = astnd prov (Ef_cat(eff1, (astnd prov (Ef_cat(eff2, eff3))))) in

  match exnd.data with
    | E_value(vnd) ->
      let tvnd = typv vnd plc env in
      
      tastnd exnd.prov (E_value(tvnd)) tvnd.info,
      astnd exnd.prov Ef_emp,
      env

    | E_cond(e1, e2, e3) ->
      (*print_string "type checking :"; print_string "Cond, e1 = "; pp_expr_nd e1; print_string ", e2 = ";
      pp_expr_nd e2; print_string ", e3 = "; pp_expr_nd e3; print_newline ();*)

      let te1, eff1, _ = typex e1 plc env in
      let te2, eff2, _ = typex e2 plc env in
      let te3, eff3, _ = typex e3 plc env in

      if not (eqtyp te1.info T_bool plc env) then
	errf exnd.prov (fun _ -> print_string "Condition "; pp_expr_nd te1;
	  print_string " has type "; pp_typ te1.info; print_string " while expected T_bool")

      else
	let finaltyp =
	  if subtyp te2.info te3.info plc env then te3.info
	  else if subtyp te3.info te2.info plc env then te2.info
	  else
	    errf exnd.prov (fun _ ->
	      print_string "Conditional true branch has type "; pp_typ te2.info;
	      print_string ", while the false branch has type "; pp_typ te3.info;
	      print_string ", none of which is a subtype of the other")
	in

	if mode plc = Sec && not (firstorder finaltyp) then
	  errf exnd.prov (fun _ -> print_string "Conditional branches have type ";
	  pp_typ finaltyp; print_string ", higher-order types in branches not allowed sec blocks")

	else
	  tastnd exnd.prov (E_cond(te1, te2, te3)) finaltyp,
	  normalizeeff (combeff3 eff1 eff2 eff3 exnd.prov) plc env,
	  env

    | E_proj(fldnd, rexnd) ->
      let trexnd, eff, _ = typex rexnd plc env in
      
      let fname = fldname fldnd in
      
      let ftyp = match trexnd.info with
	| T_row(l) ->
	  begin
	    try
	      (snd (getfieldindex l fname)).data
	    with
	      | Not_found ->
		errf exnd.prov (fun _ ->
		  print_string ("Field "^fname^" not present in record type ");
		  pp_typ trexnd.info)
	  end
	| _ -> errf exnd.prov (fun _ ->
	  print_string "Ill-typed field projection from type ";
	  pp_typ trexnd.info)
      in
      
      tastnd exnd.prov (E_proj(fldnd, trexnd)) ftyp, normalizeeff eff plc env, env
      
    | E_let(patnd, plndopt, e1, e2) ->
      let getenvplc plc1 plc2 = match plc1.data, plc2.data with	
	| Pl_ps(m1, _), Pl_ps(_, v2) -> astnd exnd.prov (Pl_ps(m1, v2))

	| Pl_top, _ -> plc2
	
	| _ -> raise Not_found
      in

      let plc1, eff = match plndopt with
	| None -> plc, astnd exnd.prov Ef_emp
	| Some(p) ->
	  let tp = typplcnd p plc env in
	  tp, astnd exnd.prov (Ef_place(tp))
      in
      
      if not (del plc plc1 env) then
	errf exnd.prov (fun _ -> print_string "Place "; pp_place_nd plc;
	  print_string " cannot delegate to place "; pp_place_nd plc1)

      else
	let vname = patvarname patnd in
      
	let te1, eff1, _ = typex e1 plc1 env in	
	let te2, eff2, _ = typex e2 plc ((vname, {t=te1.info; p=getenvplc plc plc1})::env) in
	
	tastnd exnd.prov (E_let(patnd, Some(plc1), te1, te2)) te2.info,
	normalizeeff (combeff3 eff eff1 eff2 exnd.prov) plc env,
	env

    | E_natop(opnd, e1, e2) ->
      let te1, eff1, _ = typex e1 plc env in
      let te2, eff2, _ = typex e2 plc env in

      if not (te1.info = T_nat) then
	errf exnd.prov (fun _ -> print_string "Expression ";
	  pp_expr_nd te1; print_string " has type "; pp_typ te1.info;
	  print_string " while expected T_nat")

      else if not (te2.info = T_nat) then
	errf exnd.prov (fun _ -> print_string "Expression ";
	  pp_expr_nd te2; print_string " has type "; pp_typ te2.info;
	  print_string " while expected T_nat")
      
      else
	let optyp = match opnd.data with
	  | Natop_plus -> T_nat
	  | Natop_sub -> T_nat
	  | Natop_gt -> T_bool
	  | Natop_equals -> T_bool
	in
	
	tastnd exnd.prov (E_natop(opnd, te1, te2)) optyp,
	normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	env
	
    | E_wire(e1, e2) ->
      let te1, eff1, _ = typex e1 plc env in

      let ve1 = match te1.data with
	| E_value(v) -> v
	| _ -> errf exnd.prov (fun _ -> print_string "Wire domain ";
	  pp_expr_nd te1; print_string " is not in ANF form")
      in

      if not (isprinctyp ve1) then
	errf exnd.prov (fun _ -> print_string "Wire domain ";
	  pp_expr_nd te1; print_string " has type "; pp_typ te1.info;
	  print_string " while expected a T_princ type")

      else
	let r = getref ve1 in
	if not (issubsetplc r plc env) then
	  errf exnd.prov (fun _ -> print_string "Wire domain refinement ";
	    pp_refine_nd r; print_string " cannot prove that it's a subset of ";
	    pp_place_nd plc)

	else
	  let plc1 =
	    if mode plc = Sec then plc
	    else astnd ve1.prov (Pl_ps(astnd ve1.prov Par, ve1))
	  in
	  let te2, eff2, _ = typex e2 plc1 env in

	  if not (wirefree te2.info && sharefree te2.info) then
	  errf exnd.prov (fun _ -> print_string "Wire range value type ";
	    pp_typ te2.info; print_string " is not wirefree and sharefree")

	else if mode plc = Sec && not (firstorder te2.info) then
	  errf exnd.prov (fun _ -> print_string "Wire range value type ";
	    pp_typ te2.info; print_string " is not firstorder in place ";
	    pp_place_nd plc)

	else
	  tastnd exnd.prov (E_wire(te1, te2)) (T_wire(ve1, (astnd te2.prov te2.info))),
	  normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	  env
	
    | E_wproj(e1, e2) ->
      let te1, eff1, _ = typex e1 plc env in
      let te2, eff2, _ = typex e2 plc env in
      
      let vnddom, vtyp = match te1.info with
	| T_wire(vnd, typnd) -> vnd, typnd.data
	| _ -> errf exnd.prov (fun _ -> print_string "Wire projection from ";
	  pp_expr_nd te1; print_string " having type "; pp_typ te1.info;
	  print_string " while expected a wire bundle type")
      in
      
      let r1 = astnd exnd.prov (R_subeq(vnddom)) in
      let r2 = astnd exnd.prov R_singl in
      let r3 = astnd exnd.prov (R_conj(r1, r2)) in
      let r4 =
	if mode plc = Par then
	  match plc.data with
	    | Pl_top -> err exnd.prov "Wire projection is not allowed in Top place"

	    | Pl_ps(_, vnd) ->
	      let r5 = astnd exnd.prov (R_eq(vnd)) in
	      astnd exnd.prov (R_conj(r5, r3))
	else
	  r3
      in
      
      if not (satref (getreft te2.info te2.prov) r4 env) then
	errf exnd.prov (fun _ -> print_string "Wire projection expression ";
	  pp_expr_nd te2; print_string " with type "; pp_typ te2.info;
	  print_string " cannot prove the refinement "; pp_refine_nd r4;
	  print_string " in current place "; pp_place_nd plc)

      else
	tastnd exnd.prov (E_wproj(te1, te2)) vtyp,
	normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	env
    
    | E_wcat(e1, e2) ->
      let te1, eff1, _ = typex e1 plc env in
      let te2, eff2, _ = typex e2 plc env in

      let (vnd1, vnd2, vtypnd) = match te1.info, te2.info with
	| T_wire(v1, vtyp1), T_wire(v2, vtyp2) ->
	  if not (eqtyp vtyp1.data vtyp2.data plc env) then
	    errf exnd.prov (fun _ -> print_string "Wire concat has range types ";
	      pp_typ te1.info; print_string " and "; pp_typ te2.info;
	      print_string " while expected equal types")

	  else
	    v1, v2, vtyp1

	| _ -> errf exnd.prov (fun _ -> print_string "Wire concatenation of types ";
	  pp_typ te1.info; print_string " and "; pp_typ te2.info)
      in
      
      let vnd = astnd vnd1.prov (V_ps_union(vnd1, vnd2)) in
      let tvnd = typv vnd plc env in
      
      if not (issubsetplc (getref tvnd) plc env) then
	errf exnd.prov (fun _ -> print_string "Wcat final domain is ";
	  pp_value_nd tvnd; print_string " having type "; pp_typ tvnd.info;
	  print_string " which is not a subset of current place ";
	  pp_place_nd plc)
      else

	tastnd exnd.prov (E_wcat(te1, te2)) (T_wire(tvnd, vtypnd)),
	normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	env

    | E_wcopy(e1, e2) ->
      if mode plc = Sec then
	err exnd.prov "Wcopy not allowed in Sec mode"

      else
	let te1, eff1, _ = typex e1 plc env in
	
	let v1 = match te1.data with
	  | E_value(vnd) -> vnd

	  | _ -> errf exnd.prov (fun _ -> print_string "Wcopy first argument ";
	    pp_expr_nd te1; print_string " is not in ANF form")
	in
      
	if not (issubsetplc (getref v1) plc env) then
	  errf exnd.prov (fun _ -> print_string "Wcopy first argument ";
	    pp_expr_nd te1; print_string " has type "; pp_typ te1.info;
	    print_string " which is not a subset of current place ";
	    pp_place_nd plc)

	else
	  let plc1 = astnd v1.prov (Pl_ps(astnd v1.prov Par, v1)) in
	  let te2, eff2, _ = typex e2 plc1 env in
	  tastnd exnd.prov (E_wcopy(te1, te2)) te2.info,
	  normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	  env
      
    | E_app(appnd) ->
      let tappnd, eff, _ = typapp appnd plc env in
      tastnd exnd.prov (E_app(tappnd)) tappnd.info,
      normalizeeff eff plc env,
      env

    | E_array(lexnd, vexnd) ->
      let tlexnd, eff1, _ = typex lexnd plc env in
      let tvexnd, eff2, _ = typex vexnd plc env in
      
      if not (eqtyp tlexnd.info T_nat plc env) then
	errf exnd.prov (fun _ -> print_string "Array first argument ";
	  pp_expr_nd tlexnd; print_string " has type "; pp_typ tlexnd.info;
	  print_string " while expected a T_nat")

      else
	let btypnd = astnd exnd.prov tvexnd.info in
	tastnd exnd.prov (E_array(tlexnd, tvexnd)) (T_array(btypnd)),
	normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	env

    | E_select(arrexnd, indexnd) ->
      let tarrexnd, eff1, _ = typex arrexnd plc env in
      let tindexnd, eff2, _ = typex indexnd plc env in

      let bt = 
	match tarrexnd.info with
	  | T_array(btypnd) -> btypnd.data
	  | _ -> errf exnd.prov (fun _ -> print_string "Array projection from ";
	    pp_expr_nd tarrexnd; print_string " having type "; pp_typ tarrexnd.info;
	    print_string " while expected a T_array(_)")
      in
      
      if not (eqtyp tindexnd.info T_nat plc env) then
	errf exnd.prov (fun _ -> print_string "Array projection index ";
	  pp_expr_nd tindexnd; print_string " has type "; pp_typ tindexnd.info;
	  print_string " while expected a T_nat")

      else
	tastnd exnd.prov (E_select(tarrexnd, tindexnd)) bt,
	normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	env
	
    | E_update(arrexnd, indexnd, vexnd) ->
      let tarrexnd, eff1, _ = typex arrexnd plc env in
      let tindexnd, eff2, _ = typex indexnd plc env in
      let tvexnd,   eff3, _ = typex vexnd plc env in

      let bt =
	match tarrexnd.info with
	  | T_array(btypnd) -> btypnd.data
	  | _ ->
	    errf exnd.prov (fun _ -> print_string "Array update on ";
	      pp_expr_nd tarrexnd; print_string " which has type ";
	      pp_typ tarrexnd.info; print_string " while expected a T_arr(_)")
      in
      
      if not (eqtyp tindexnd.info T_nat plc env) then
	errf exnd.prov (fun _ -> print_string "Array update index ";
	  pp_expr_nd tindexnd; print_string " has type "; pp_typ tindexnd.info;
	  print_string " while expected a T_nat")

      else if not (subtyp tvexnd.info bt plc env) then
	errf exnd.prov (fun _ -> print_string "Array has base type ";
	  pp_typ bt; print_string " while update value "; pp_expr_nd tvexnd;
	  print_string " has type "; pp_typ tvexnd.info)

      else
	tastnd exnd.prov (E_update(tarrexnd, tindexnd, tvexnd)) T_unit,
	normalizeeff (combeff3 eff1 eff2 eff3 exnd.prov) plc env,
	env

    | E_wapp(vnd, vwireexnd, lamwireexnd) ->
      if not (mode plc = Par) then
	err exnd.prov "Wapp is allowed only in Par mode"
	  
      else
	let tvnd = typv vnd plc env in
	let tvwireexnd, eff1, _ = typex vwireexnd plc env in
	let tlamwireexnd, eff2, _ = typex lamwireexnd plc env in

	if not (isprinctyp tvnd) then
	  errf exnd.prov (fun _ -> print_string "Wapp first argument ";
	    pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
	    print_string " while expected a T_ps(_)")

	else	
	  let vwiretyp =
	    match tvwireexnd.info with
	      | T_wire(vdomnd, vtypnd) ->
		if not (issubsetval (getref tvnd) vdomnd env) then
		  errf exnd.prov (fun _ -> print_string "Wapp domain ";
		    pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
		    print_string " which is not a subset of value wire domain ";
		    pp_value_nd vdomnd; print_string " with type "; pp_typ vdomnd.info)
		else
		  vtypnd.data

	      | _ -> errf exnd.prov (fun _ -> print_string "Wapp second argument ";
		pp_expr_nd tvwireexnd; print_string " has type "; pp_typ tvwireexnd.info;
		print_string " while expected a T_wire(_)")
	  in

	  let argtyp, rettypnd =
	    match tlamwireexnd.info with
	      | T_wire(vdomnd, lamtyp) ->
		if not (issubsetval (getref tvnd) vdomnd env) then
		  errf exnd.prov (fun _ -> print_string "Wapp domain ";
		    pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
		    print_string " which is not a subset of function wire domain ";
		    pp_value_nd vdomnd; print_string " with type "; pp_typ vdomnd.info)
		else
		  begin
		    match lamtyp.data with
		      | T_arr(_, t1, eff, t2) ->
			if not (empeff eff.data) then
			  errf exnd.prov (fun _ -> print_string "Wapp function wire has range type ";
			    pp_typ lamtyp.data; print_string " while expected empty effects")
			    
			else
			  t1.data, t2
			
		      | _ -> errf exnd.prov (fun _ -> print_string "Wapp function wire has range type ";
			pp_typ lamtyp.data; print_string " while expected a T_arr(_)")
		  end

	      | _ -> errf exnd.prov (fun _ -> print_string "Wapp second argument ";
		pp_expr_nd tvwireexnd; print_string " has type "; pp_typ tvwireexnd.info;
		print_string " while expected a T_wire(_)")
	  in
	  
	  if not (subtyp vwiretyp argtyp plc env) then
	    errf exnd.prov (fun _ -> print_string "Wapp value wire type ";
	      pp_typ vwiretyp; print_string " is not a subtype of function wire arg type ";
	      pp_typ argtyp)

	  else
	    tastnd exnd.prov (E_wapp(tvnd, tvwireexnd, tlamwireexnd)) (T_wire(tvnd, rettypnd)),
	    normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
	    env

    | E_wfold(vnd, wirexnd, accumexnd, lamnd) ->
      if not (mode plc = Sec) then
	err exnd.prov "Wire fold only allowed in sec mode"

      else
	let tvnd = typv vnd plc env in
	let twireexnd, eff1, _ = typex wirexnd plc env in
	let taccumexnd, eff2, _ = typex accumexnd plc env in
	let tlamnd, eff3, _ = typlam lamnd plc env in

	if not (isprinctyp tvnd) then
	  errf exnd.prov (fun _ -> print_string "Wfold first argument ";
	    pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
	    print_string " while expected a T_ps(_)")

	else
	  let vwiretyp = match twireexnd.info with
	    | T_wire(vdomnd, vtyp) ->
	      if not (issubsetval (getref tvnd) vdomnd env) then
		errf exnd.prov (fun _ -> print_string "Wfold domain ";
		  pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
		  print_string " which is not a subset of wire domain ";
		  pp_value_nd vdomnd; print_string " with type "; pp_typ vdomnd.info)

	      else
		vtyp.data

	    | _ -> errf exnd.prov (fun _ -> print_string "Wfold argument "; pp_expr_nd twireexnd;
	      print_string " has type "; pp_typ twireexnd.info; print_string " while expected a T_wire(_)")
	  in
	  
	  begin
	    match tlamnd.info with
	      | T_arr(_, {data = t1}, feff1,
		      {
			data = T_arr(_, {data = t2}, feff2,
				     {
				       data = T_arr(_, {data = t3}, feff3,
						    {
						      data = t4
						    })
				     })
		      }) ->
		if not (subtyp taccumexnd.info t1 plc env) then
		  errf exnd.prov (fun _ -> print_string "Wfold accumulator argument ";
		    pp_expr_nd taccumexnd; print_string " has type "; pp_typ taccumexnd.info;
		    print_string " which is not a subtype of wfold function first argument type ";
		    pp_typ t1)

		else if not (empeff feff1.data && empeff feff2.data && empeff feff3.data) then
		  errf exnd.prov (fun _ -> print_string "Wfold function effects are ";
		    pp_eff_nd feff1; print_string " and "; pp_eff_nd feff2; print_string " and "; pp_eff_nd feff3;
		    print_string " while expected empty effects")

		else
		  let r1 = astnd exnd.prov R_singl in
		  let r2 = astnd exnd.prov (R_subeq(tvnd)) in
		  let refinend = astnd exnd.prov (R_conj(r1, r2)) in
		  if not (subtyp (T_ps(refinend)) t2 plc env) then
		    errf exnd.prov (fun _ -> print_string "Wfold function second argument type ";
		      pp_typ t2; print_string " is not a supertype of ";
		      pp_typ (T_ps(refinend)))

		  else if not (subtyp vwiretyp t3 plc env) then
		    errf exnd.prov (fun _ -> print_string "Wfold wire value ";
		      pp_expr_nd twireexnd; print_string " has value type ";
		      pp_typ vwiretyp; print_string " which is not a subtype of wfold function third argument type ";
		      pp_typ t3)

		  else
		    let finaltyp =
		      if subtyp t4 taccumexnd.info plc env then taccumexnd.info
		      else if subtyp taccumexnd.info t4 plc env then t4
		      else
			errf exnd.prov (fun _ -> print_string "Wfold function return type ";
			  pp_typ t4; print_string " is not a subtype/supertype of accumulator ";
			  pp_expr_nd taccumexnd; print_string " with  type ";
			  pp_typ taccumexnd.info)
		    in
		    tastnd exnd.prov (E_wfold(tvnd, twireexnd, taccumexnd, tlamnd)) finaltyp,
		    normalizeeff (combeff3 eff1 eff2 eff3 exnd.prov) plc env,
		    env

	      | _ -> errf exnd.prov (fun _ -> print_string "Wfold function type ";
		pp_typ tlamnd.info; print_string " is not t1 -> t2 -> t3 -> t4")
	  end

    | E_waps(vnd, wirexnd, lamnd) ->
      if not (mode plc = Sec) then
	err exnd.prov "Waps only allowed in sec mode"

      else
	let tvnd = typv vnd plc env in
	let twireexnd, eff1, _ = typex wirexnd plc env in
	let tlamnd, eff2, _ = typlam lamnd plc env in

	if not (isprinctyp tvnd) then
	  errf exnd.prov (fun _ -> print_string "Waps first argument ";
	    pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
	    print_string " while expected a T_ps(_)")

	else
	  let vwiretyp = match twireexnd.info with
	    | T_wire(vdomnd, vtyp) ->
	      if not (issubsetval (getref tvnd) vdomnd env) then
		errf exnd.prov (fun _ -> print_string "Waps domain ";
		  pp_value_nd tvnd; print_string " has type "; pp_typ tvnd.info;
		  print_string " which is not a subset of wire domain ";
		  pp_value_nd vdomnd; print_string " with type "; pp_typ vdomnd.info)

	      else
		vtyp.data

	    | _ -> errf exnd.prov (fun _ -> print_string "Waps argument "; pp_expr_nd twireexnd;
	      print_string " has type "; pp_typ twireexnd.info; print_string " while expected a T_wire(_)")
	  in

	begin
	  match tlamnd.info with
	    | T_arr(_, {data = t1}, eff, t2) ->
	      if not (subtyp vwiretyp t1 plc env) then
		errf exnd.prov (fun _ -> print_string "Waps wire value type ";
		  pp_typ vwiretyp; print_string " is not a subtype of function argument type ";
		  pp_typ t1)

	      else if not (empeff eff.data) then
		errf exnd.prov (fun _ -> print_string "Waps function type has effect ";
		  pp_eff_nd eff; print_string " while expected empty effect")
		  
	      else
		tastnd exnd.prov (E_waps(tvnd, twireexnd, tlamnd)) (T_wire(tvnd, t2)),
		normalizeeff (combeff2 eff1 eff2 exnd.prov) plc env,
		env

	    | _ -> errf exnd.prov (fun _ -> print_string "Waps function argument has type ";
	      pp_typ tlamnd.info; print_string " while expected a t1 -> t2")
	end
	  
    | E_sh(vexnd) ->
      let tvexnd, eff, _ = typex vexnd plc env in
      let btypnd = astnd exnd.prov tvexnd.info in

      if not (mode plc = Sec) then
	err exnd.prov "Shares can only be created in Par mode"

      else if not (firstorder btypnd.data && sharefree btypnd.data &&
		     wirefree btypnd.data) then
	errf exnd.prov (fun _ -> print_string "Cannot create shares of type ";
	  pp_typ btypnd.data)

      else
	begin
	  match plc.data with
	    | Pl_ps(_, vnd) ->
	      tastnd exnd.prov (E_sh(tvexnd)) (T_sh(vnd, btypnd)), eff, env
	    | _ -> err exnd.prov "Impossible to reach here"
	end

    | E_comb(vexnd) ->
      let tvexnd, eff, _ = typex vexnd plc env in
      
      if not (mode plc = Sec) then
	err exnd.prov "Shares can only be combined in Sec mode"

      else
	begin
	  match tvexnd.info with
	    | T_sh(vnd, btypnd) ->
	      begin
		match plc.data with
		  | Pl_ps(_, vnd1) ->
		    if not (veqprincs (getref vnd) vnd1 env) then
		      errf exnd.prov (fun _ -> print_string "Share domain value ";
			pp_value_nd vnd; print_string " has type "; pp_typ vnd.info;
			print_string " which is not equal to current place value ";
			pp_value_nd vnd1; print_string " having type ";
			pp_typ vnd1.info)

		    else
		      tastnd exnd.prov (E_comb(tvexnd)) btypnd.data, eff, env
			
		  | _ -> err exnd.prov "Impossible code"
	      end

	    | _ -> errf exnd.prov (fun _ -> print_string "Comb argument ";
	      pp_expr_nd tvexnd; print_string " has type "; pp_typ vexnd.info;
	      print_string " while expected a T_sh(_) type")
	end

    | E_paren(e) ->
      let te, eff, _ = typex e plc env in
      tastnd exnd.prov (E_paren(te)) te.info, eff, env

    | E_cast(e, t) ->
      let te, eff, _ = typex e plc env in

      if not (subtyp te.info t.data plc env) then
	errf te.prov (fun _ ->
	  print_string "cast type error: type "; pp_typ te.info; print_string " is not a subtype of "; pp_typ t.data)

      else
	tastnd exnd.prov (E_cast(te, t)) t.data, eff, env

    | E_print e ->
      let te, eff, _ = typex e plc env in
      tastnd exnd.prov (E_print(te)) T_unit, eff, env

    | E_sysop(varnd, typop, l) ->
      let opname = varname varnd in

      if opname = "rand" then
	match typop, l with
	  | None, [v] ->
	    let tv = typv v plc env in
	    if not (tv.info = T_nat) then
	      errf tv.prov (fun _ ->
		print_string "rand expects an argument of type T_nat while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)

	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv])) T_nat,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "rand expects no type and a single argument of type T_nat while found ";
	    print_int (List.length l); print_string " arguments")

      else if opname = "run" then
	match typop, l with
	  | None, [v] ->
	    let tv = typv v plc env in
	    if not (tv.info = T_string) then
	      errf tv.prov (fun _ ->
		print_string "run expects an argument of type T_string while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)

	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv])) T_proc,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "run expects no type and a single argument of type T_string while found ";
	    print_int (List.length l); print_string " arguments")

      else if opname = "send" then
	match typop, l with
	  | None, v::l' ->
	    let tv = typv v plc env in
	    if not (tv.info = T_proc) then
	      errf tv.prov (fun _ ->
		print_string "send expects first argument of type T_proc while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)
	    else
	      let l' = List.map (fun v -> typv v plc env) l' in
	      tastnd exnd.prov (E_sysop(varnd, typop, tv::l')) T_unit,
	      astnd exnd.prov Ef_emp,
	    env	      
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "send expects no type and list of values")	    

      else if opname = "recv" then
	match typop, l with
	  | Some(typ), [v] ->
	    let tv = typv v plc env in
	    if not (tv.info = T_proc) then
	      errf tv.prov (fun _ ->
		print_string "recv expects a type and an argument of type T_proc while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)

	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv])) typ.data,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "recv expects a type and a single argument of type T_proc while found ";
	    print_int (List.length l); print_string " arguments")

      else if opname = "ignore" then
	match typop, l with
	  | None, [v] ->
	    let tv = typv v plc env in
	    if not (tv.info = T_proc) then
	      errf tv.prov (fun _ ->
		print_string "ignore expects an argument of type T_proc while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)

	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv])) T_unit,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "ignore expects a single argument of type T_proc while found ";
	    print_int (List.length l); print_string " arguments")

      else if opname = "stop" then
        match typop, l with
          | None, [v] ->
	    let tv = typv v plc env in
	    if not (tv.info = T_proc) then
	      errf tv.prov (fun _ ->
		print_string "stop expects a type and an argument of type T_proc while value ";
		pp_value_nd tv; print_string " has type "; pp_typ tv.info)

	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv])) T_unit,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "stop expects a single argument of type T_proc while found ";
	    print_int (List.length l); print_string " arguments")

      else if opname = "strcat" then
	match typop, l with
	  | None, [v1; v2] ->
	    let tv1 = typv v1 plc env in
	    let tv2 = typv v2 plc env in
	    if not (tv1.info = T_string && tv2.info = T_string) then
	      errf tv1.prov (fun _ ->
		print_string "strcat expects arguments of type T_string while value ";
		pp_value_nd tv1; print_string " has type "; pp_typ tv1.info; print_string " and value "; pp_value_nd tv2; print_string " has type "; pp_typ tv2.info)
	    else
	      tastnd exnd.prov (E_sysop(varnd, typop, [tv1; tv2])) T_string,
	      astnd exnd.prov Ef_emp,
	      env
	  | _ -> errf exnd.prov (fun _ ->
	    print_string "strcat expects a two arguments of type T_string while found ";
	    print_int (List.length l); print_string " arguments")	    

      else
	errf exnd.prov (fun _ -> print_string ("Sysop " ^ opname ^ " not supported"))
      
    | _ -> errf exnd.prov (fun _ -> print_string "Type checker could not type ";
      pp_expr_nd exnd)

and typlam (lamnd:lam_nd) (plc:place_nd) (env:tenv) :(lam_nd * eff_nd * tenv) =
  match lamnd.data with
    | L_lam(patnd, exnd) ->
      begin
	match patnd.data with
	  | P_cast(xpatnd, typnd) ->
	    let varnd = match xpatnd.data with
	      | P_var(varnd) -> varnd

	      | _ -> errf lamnd.prov (fun _ -> print_string "Pattern ";
		pp_pat_nd patnd; print_string " is not supported in type checker")
	    in

	    let t, b = wellformedtyp typnd.data plc env in

	    if not b then
	      errf lamnd.prov (fun _ -> print_string "Type annotation ";
		pp_typ typnd.data; print_string " is not well-formed")

	    else
	      let ttypnd = astnd typnd.prov t in
	      let tpatnd = astnd patnd.prov (P_cast(xpatnd, ttypnd)) in
	      let varname = varname varnd in
	      let texnd, eff, _ = typex exnd plc ((varname, {t=t; p=plc})::env) in
	      let rettypnd = astnd lamnd.prov texnd.info in
	      tastnd lamnd.prov (L_lam(tpatnd, texnd)) (T_arr(Some(varnd), ttypnd, eff, rettypnd)), astnd lamnd.prov Ef_emp, env

	  | _ -> errf lamnd.prov (fun _ -> print_string "Pattern ";
	    pp_pat_nd patnd; print_string " is not supported in type checker (missing type annotation?)")
      end

    | L_fix(varnd, typnd, patnd, exnd) ->
      let fvarname = varname varnd in
      let tmplamnd = astnd lamnd.prov (L_lam(patnd, exnd)) in
      
      let t, b = wellformedtyp typnd.data plc env in
      if not b then
	errf lamnd.prov (fun _ -> print_string "Fix type annotation ";
	  pp_typ typnd.data; print_string " is not well-formed")

      else
	let tlamnd, eff, _ = typlam tmplamnd plc ((fvarname, {t = t; p = plc})::env) in
	if not (eqtyp tlamnd.info t plc env) then
	  errf lamnd.prov (fun _ -> print_string "Fix annotation type ";
	    pp_typ t; print_string " is not same as inner lambda type ";
	    pp_typ tlamnd.info)
	    
	else
	  begin
	    match t with
	      | T_arr(_, _, eff, _) ->
		if not (effdel plc eff env) then
		  errf lamnd.prov (fun _ -> print_string "Current place ";
		    pp_place_nd plc; print_string " cannot perform fix effects ";
		    pp_eff_nd eff)

		else
		  let tpatnd, texnd =
		    match tlamnd.data with
		      | L_lam(p, e) -> p, e
		      | _ -> err lamnd.prov "Impossible code"
		  in
		  tastnd lamnd.prov (L_fix(varnd, astnd typnd.prov t, tpatnd, texnd)) t, eff, env

	      | _ -> errf lamnd.prov (fun _ -> print_string "Fix inner type ";
		pp_typ t; print_string " is not an arrow type")
	  end

and typapp (appnd:app_nd) (plc:place_nd) (env:tenv) :(app_nd * eff_nd * tenv) =
  let combeff3 eff1 eff2 eff3 prov = astnd prov (Ef_cat(eff1, (astnd prov (Ef_cat(eff2, eff3))))) in

  match appnd.data with
    | A_var(varnd) ->
      let vnd = astnd appnd.prov (V_var(varnd)) in
      let tvnd = typv vnd plc env in
      tastnd appnd.prov appnd.data tvnd.info, astnd appnd.prov Ef_emp, env

    | A_lam(lamnd) ->
      let tlamnd, eff, _ = typlam lamnd plc env in
      tastnd appnd.prov (A_lam(tlamnd)) tlamnd.info, eff, env

    | A_app(appnd, exnd) ->      
      let tappnd, eff1, _ = typapp appnd plc env in
      let texnd, eff2, _ = typex exnd plc env in

      let argvnd = match texnd.data with
	| E_value(vnd) -> vnd

	| _ -> err appnd.prov "Non ANF arguments in function applications are not supported"
      in

      begin
	match tappnd.info with
	  | T_arr(varndopt, {data = t1}, eff, {data = t2}) ->
	    let neff, nret = match varndopt with
	      | None -> eff, t2
	      | Some(varnd) ->
		let x = varname varnd in		
		seff x argvnd eff, styp x argvnd t2
	    in

	    if not (subtyp texnd.info t1 plc env) then
	      errf appnd.prov (fun _ -> print_string "Argument "; pp_expr_nd texnd;
		print_string " has type "; pp_typ texnd.info;
		print_string " which is not a subtype of "; pp_typ t1)

	    else if not (effdel plc neff env) then
	      errf appnd.prov (fun _ -> print_string "Current place ";
		pp_place_nd plc; print_string " cannot perform delegations ";
		pp_eff_nd neff)

	    else
	      tastnd appnd.prov (A_app(tappnd, texnd)) nret,
	      normalizeeff (combeff3 eff1 eff2 neff appnd.prov) plc env,
	      env
	      
	  | _ -> errf appnd.prov (fun _ -> print_string "Application function ";
	    pp_app_nd tappnd; print_string " has type "; pp_typ tappnd.info;
	    print_string " while expected a T_arr(_)")

      end

    | A_paren(app) ->
      let tapp, eff, _ = typapp app plc env in
      tastnd appnd.prov (A_paren(tapp)) tapp.info, eff, env

and typplcnd plcnd plc env = match plcnd.data with
  | Pl_top -> plcnd
  | Pl_ps(plmnd, vnd) -> astnd plcnd.prov (Pl_ps(plmnd, typv vnd plc env))
