type princ  = string
type arrloc = int

exception NYI
exception Impossible

module Proc = Sysproc.Proc

  (* Set of principals *)
module Ps = Set.Make(String)

  (* Variable names; distinct from constructor names, like in caml. *)
type var = Var of string

(*
 * this variable is available to Wysteria programs, it can be
 * accessed in parallel(p) mode where p is a singleton principal.
 *)
let _mevar = "__me__"
    
  (* Constructor names; lexically distinct from variable names, like in caml. *)
type cons = Cons of string
    
type field = Field of string

  (* Environments; used to represent closures. *)
module Env = Map.Make(
  struct 
    type t = var
    let compare (Var i) (Var j) = compare i j
  end)
  
type 'a astnd = { prov : Global.Prov.prov;
		  data : 'a;
		  info : typ; }

and var_nd = var astnd
and cons_nd = cons astnd
and field_nd = field astnd
and princ_nd = princ astnd
    
and file_nd = file astnd
and file = 
  | F_body    of expr_nd
  | F_defn    of defn_nd * file_nd
      
and defn_nd = defn astnd
and defn =
  | D_fundefn of var_nd * pat_nd list * expr_nd
  | D_funtyp  of var_nd * typ_nd

and varval = value_nd * var_nd
and value_nd = value astnd
and value =
  | V_princ    of princ_nd
  | V_var      of var_nd
  | V_arrloc   of arrloc
  | V_bool     of bool
  | V_nat      of int
  | V_string   of string
  | V_unit
  | V_inj      of cons_nd * value_nd list
  | V_row      of ( field_nd * value_nd ) list
  | V_emp
  | V_ps_lit   of value_nd list
  | V_ps_union of value_nd * value_nd
  | V_paren    of value_nd        
  | V_clos     of clos
  | V_wires    of (princ_nd * value_nd) list
  | V_sh       of Bytes.t
  | V_proc     of Proc.t

  (* closures *)
and clos = { clos_env : env ;
             clos_lam : lam_nd ; }

  (* environments *)
and env = bnd Env.t

  (* (environment) bindings *)
and bnd = { var   : var_nd ;
            place : place_nd ;
            value : value_nd ; }

(* Place *)
and place_nd = place astnd
and place =
  | Pl_top 
  | Pl_ps of pl_mode_nd * value_nd

and pl_mode_nd = pl_mode astnd
and pl_mode = Par | Sec

  (* Refinements *)
and refine_nd = refine astnd
and refine =
  | R_true
  | R_singl
  | R_subeq of value_nd
  | R_eq    of value_nd
  | R_conj  of refine_nd * refine_nd
  | R_not   of refine_nd

  (* Effects *)
and eff_nd = eff astnd
and eff =
  | Ef_emp
  | Ef_place of place_nd
  | Ef_cat   of eff_nd * eff_nd
      
  (* Types *)
and typ_nd = typ astnd
and typ =
  | T_unknown
  (*| T_var  of var_nd*)
  | T_wire of value_nd * typ_nd
  | T_sh   of value_nd * typ_nd
  (*| T_appv of typ_nd * value_nd*)
  (*| T_appt of typ_nd * typ_nd*)
  | T_unit
  | T_nat
  | T_bool
  | T_arr of var_nd option * typ_nd * eff_nd * typ_nd
  | T_sum of ( cons_nd * ( typ_nd list ) ) list
  | T_row of ( field_nd * typ_nd ) list
  | T_ps  of refine_nd
  | T_array of typ_nd
  | T_proc
  | T_string

  (* Lambdas; they appear in both [[expr]] and [[app]] sub-grammars. *)
and lam_nd = lam astnd
and lam =
  | L_fix of var_nd * typ_nd * pat_nd * expr_nd (* recursive functions *)
  | L_lam of pat_nd * expr_nd (* anonymous functions *)

  (* Patterns; destruct products, bind variables and perform casts. *)
and pat_nd = pat astnd
and pat =
  | P_var   of var_nd
  | P_cast  of pat_nd * typ_nd
  | P_row   of (field_nd * pat_nd) list

  (* Expressions *)
  (* Note: Not (yet) in ANF: all sub-structures are allowed to be expressions. *)
and expr_nd = expr astnd
and expr =
  | E_value  of value_nd
  | E_case  of expr_nd * ( cons_nd * pat_nd list * expr_nd ) list
  | E_cond  of expr_nd * expr_nd * expr_nd
  | E_let   of pat_nd * place_nd option * expr_nd * expr_nd
  | E_app   of app_nd (* completed sequence of (partial) applications. *)
  | E_proj  of field_nd * expr_nd (* generalizes fst and snd to rows of data *)
  | E_natop of natop_nd * expr_nd * expr_nd (* Naturals -- operations *)
  | E_print of expr_nd (* printing *)
  | E_sysop of var_nd * typ_nd option * (value_nd list)

  (* Arrays *)
  | E_array  of expr_nd * expr_nd
  | E_select of expr_nd * expr_nd
  | E_update of expr_nd * expr_nd * expr_nd

  (* Shares *)
  | E_sh   of expr_nd
  | E_comb of expr_nd

  (* Wires --  operations *)
  | E_wire  of expr_nd * expr_nd (* wire bundle introduction *)
  | E_wcat  of expr_nd * expr_nd (* wire bundle concatenation *)
  | E_wcopy of expr_nd * expr_nd (* wire bundle copy *)
  | E_wproj of expr_nd * expr_nd (* wire bundle projection *)
  | E_wapp  of value_nd * expr_nd * expr_nd          (* wire application *)
  | E_waps  of value_nd * expr_nd * lam_nd          (* secure wire application *)
  | E_wfold of value_nd * expr_nd * expr_nd * lam_nd (* wire folding *)
  | E_paren of expr_nd

  | E_cast   of expr_nd * typ_nd
  | E_subset of expr_nd * expr_nd * expr_nd

and natop_nd = natop astnd
and natop =
  | Natop_plus
  | Natop_sub
  | Natop_gt
  | Natop_equals

  (* Applications *)
and app_nd = app astnd
and app =
  | A_var   of var_nd
  | A_lam   of lam_nd
  | A_app   of app_nd * expr_nd
  | A_paren of app_nd

let dummyinfo = T_unknown

module Macros = struct

  let next_id = ref 0

  let pat_of_var v t = 
    {data=P_cast({data=P_var v;info=T_unknown;prov=Global.Prov.Synth},t);
     info=T_unknown; prov=Global.Prov.Synth}

  let fresh_var : unit -> var_nd = fun _ -> 
    let i = !next_id in incr next_id ;
    {data=Var ("$"^(string_of_int i)); 
     info=dummyinfo;
     prov=Global.Prov.Synth}

  let g    x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let gv   x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let gt   x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let geff x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let gl   x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let ga   x = {data=x; info=T_unknown; prov=Global.Prov.Synth}
  let gno  x = {data=x; info=T_unknown; prov=Global.Prov.Synth}

  let fresh_var_pat' : typ -> var_nd * pat_nd = fun typ ->
    let v = fresh_var () in
    v, pat_of_var v (gt typ)

  let fresh_var_pat : unit -> var_nd * pat_nd = fun _ ->
    let v = fresh_var () in
    v, {data=P_var v; info=dummyinfo; prov=Global.Prov.Synth}
    
  let forloop 
      : var_nd -> value_nd -> value_nd -> expr_nd -> expr 
    = fun var v1 v2 body ->
    (* Macro exansion idea: *)
    (* 
     * let loop = fix loop : (var:nat) -> unit.
     *    if var > v2 then ()
     *    else ( let _    = body    in 
     *           let var' = var + 1 in
     *           loop var'          )
     * in (loop v1)
     *)
    let dumvar, dumpat = fresh_var_pat () in
    let idxvar, idxpat = var, (pat_of_var var (gt T_nat)) in
    let jdxvar, jdxpat = fresh_var_pat' T_nat in
    let loopvar        = fresh_var () in
    let looptyp        = gt (T_arr(Some idxvar, gt(T_nat), geff(Ef_emp), gt(T_unit))) in
    let fixexpr = 
      let fixbody = 
        g(E_cond ( 
            g(E_natop (gno Natop_gt, 
                       g(E_value (gv(V_var idxvar))),
                       g(E_value v2)
                      )),
            g(E_value (gv(V_unit))),
            g(E_let (jdxpat, None, g(E_natop (gno Natop_plus, 
                                              g(E_value (gv(V_var idxvar))), 
                                              g(E_value (gv(V_nat 1)))
                                             )),
                     g(E_let (dumpat, None, body, 
                              g(E_app (ga(A_app ( 
                                            ga(A_var loopvar),
                                            g(E_value (gv(V_var jdxvar)))
                                          ))))
                             ))
                    ))
          ))
      in
      g(E_app (
          ga(A_lam (gl(L_fix (loopvar, looptyp, idxpat, fixbody))))
        ))
    in    
    E_let( pat_of_var loopvar looptyp, None,
           fixexpr,
           g( E_app (ga (A_app( ga(A_var loopvar),
                                g(E_value v1)
                              ) )))                              
         )


  let forloopf
      : var_nd -> value_nd -> value_nd -> var_nd -> expr_nd -> expr 
    = fun var v1 v2 fixvar body ->
    (* Macro exansion idea: *)
    (* 
     * let loop = fix fixvar : (var:nat) -> unit.
     *    if var > v2 then () 
     *                else body  --- NOTE: body is responsible for explicitly looping via fixvar
     * in (loop v1)
     *)
    let idxvar, idxpat = var, (pat_of_var var (gt T_nat)) in
    let loopvar        = fresh_var () in
    let looptyp        = gt (T_arr(None, gt(T_nat), geff(Ef_emp), gt(T_unit))) in
    let fixexpr = 
      let fixbody = 
        g(E_cond ( 
            g(E_natop (gno Natop_gt, 
                       g(E_value (gv(V_var idxvar))),
                       g(E_value v2)
                      )),
            g(E_value (gv(V_unit))), 
            body 
          ))
      in
      g(E_app (
          ga(A_lam (gl(L_fix (fixvar, looptyp, idxpat, fixbody))))
        ))
    in    
    E_let( pat_of_var loopvar looptyp, None,
           fixexpr,
           g( E_app (ga (A_app( ga(A_var loopvar),
                                g(E_value v1)
                              ) )))                              
         )

end

(* Sets of variables -- used to enforce uniqueness of variables in collections. *)
module Varvals = Set.Make 
  (struct
    type t = varval
    let compare x y =
         (* compare based on variable names *)
      compare (snd x).data (snd y).data
   end)
  
  (* -- -- -- -- Mapping values -- -- -- --. *)
  
  (* value_maps -- a structure that gives the operations for
     specializing a parametric tree traversal/map; read 'b as the type
     of information from "below" (the result type of the mapping
     operation) *)
type 'b value_maps = {
  v_arrloc   : arrloc -> 'b;
  v_princ    : princ_nd -> 'b;
  v_var      : var_nd -> 'b ;
  v_bool     : bool -> 'b ;
  v_nat      : int -> 'b ;
  v_unit     : unit -> 'b ;
  v_string   : string -> 'b ;
  v_inj      : cons_nd -> 'b list -> 'b;
  v_row      : ( field_nd * 'b ) list -> 'b;
  v_emp      : unit -> 'b ;
  v_ps_lit   : 'b list -> 'b ;
  v_ps_union : 'b -> 'b -> 'b ;
  v_paren    : 'b -> 'b ;
  v_clos     : clos -> 'b ;
  v_wires    : (princ_nd * 'b) list -> 'b ;
  v_sh       : Bytes.t -> 'b ;
  v_proc     : Proc.t -> 'b
}

exception Missing_case of string
    
module AstMap : sig
  val map_value       : ( value_nd -> 'b value_maps ) -> value_nd -> 'b
  val copy_value      : value_nd -> value_nd
  val close_value_sel : (varval -> value_nd) -> value_nd -> value_nd
  val close_typ_sel   : (varval -> value_nd) -> typ -> typ
  val close_typnd_sel : (varval -> value_nd) -> typ_nd -> typ_nd
  val close_place_sel : (varval -> value_nd) -> place_nd -> place_nd
  val close_value     : env -> value_nd -> value_nd

  val free_vars       : value_nd -> varval list
  val free_vars'      : value_nd -> (varval list -> varval list)
  val free_var_set    : value_nd -> Varvals.t

  val ps_value_princs'   : value_nd -> value_nd list
  val ps_value_princs    : value_nd -> princ_nd list
  val ps_value_princ_set : value_nd -> Ps.t

  val missing_case_exceptions : 'a value_maps

  val copy_value_mapper  : value_nd -> value_nd value_maps
end =
struct
  let missing_case_exceptions : 'a value_maps = {
    v_arrloc   = begin fun _      -> raise (Missing_case "v_arrloc") end ;
    v_princ    = begin fun _      -> raise (Missing_case "v_princ") end ;
    v_var      = begin fun _      -> raise (Missing_case "v_var") end ;
    v_bool     = begin fun _      -> raise (Missing_case "v_bool") end ;
    v_nat      = begin fun _      -> raise (Missing_case "v_nat") end ;
    v_unit     = begin fun _      -> raise (Missing_case "v_unit") end ;
    v_string   = begin fun _      -> raise (Missing_case "v_string") end ;
    v_inj      = begin fun _ _    -> raise (Missing_case "v_inj") end ;
    v_row      = begin fun _      -> raise (Missing_case "v_row") end ;
    v_emp      = begin fun _      -> raise (Missing_case "v_emp") end ;
    v_ps_lit   = begin fun _      -> raise (Missing_case "v_ps_lit") end ;
    v_ps_union = begin fun _ _    -> raise (Missing_case "v_ps_union") end ;
    v_paren    = begin fun _      -> raise (Missing_case "v_paren") end ;
    v_clos     = begin fun _      -> raise (Missing_case "v_clos") end ;
    v_wires    = begin fun _      -> raise (Missing_case "v_wires") end ;
    v_sh       = begin fun _      -> raise (Missing_case "v_sh") end ;
    v_proc     = begin fun _      -> raise (Missing_case "v_proc") end ;
  }

  let rec map_value = fun f v -> 
    let f' = f v in
    match v.data with
      | V_arrloc l        -> f'.v_arrloc l
      | V_princ p         -> f'.v_princ p
      | V_var var         -> f'.v_var var
      | V_bool b          -> f'.v_bool b
      | V_nat i           -> f'.v_nat i
      | V_unit            -> f'.v_unit ()
      | V_string s        -> f'.v_string s
      | V_inj(c,vs)       -> f'.v_inj c (List.map (fun v -> map_value f v) vs)
      | V_row row         -> f'.v_row (List.map (fun (fld,v) -> (fld, map_value f v)) row)
      | V_emp             -> f'.v_emp ()
      | V_ps_lit vs       -> f'.v_ps_lit (List.map (fun v -> map_value f v) vs)
      | V_ps_union(v1,v2) -> f'.v_ps_union (map_value f v1) (map_value f v2)
      | V_paren v         -> f'.v_paren (map_value f v)
      | V_clos c          -> f'.v_clos c
      | V_sh l            -> f'.v_sh l
      | V_wires pvs       -> f'.v_wires (List.map (fun (p,v) -> (p, map_value f v)) pvs)
      | V_proc p          -> f'.v_proc p

  let copy_value_mapper value_nd =
    let astnd data = { value_nd with data = data } in {
      v_arrloc   = begin fun l     -> astnd ( V_arrloc l ) end ;
      v_princ    = begin fun p     -> astnd ( V_princ p ) end ;
      v_var      = begin fun v     -> astnd ( V_var v ) end ;
      v_bool     = begin fun b     -> astnd ( V_bool b ) end ;
      v_nat      = begin fun i     -> astnd ( V_nat i ) end ;
      v_unit     = begin fun _     -> astnd ( V_unit ) end ;
      v_string   = begin fun s     -> astnd ( V_string s ) end ;
      v_inj      = begin fun c vs  -> astnd ( V_inj (c, vs) ) end ;
      v_row      = begin fun row   -> astnd ( V_row row ) end ;
      v_emp      = begin fun _     -> astnd ( V_emp ) end ;
      v_ps_lit   = begin fun vs    -> astnd ( V_ps_lit vs ) end ;
      v_ps_union = begin fun v1 v2 -> astnd ( V_ps_union(v1,v2) ) end ;
      v_paren    = begin fun v     -> astnd ( V_paren v ) end ;
      v_clos     = begin fun c     -> astnd ( V_clos c ) end ;
      v_sh       = begin fun l     -> astnd ( V_sh l ) end ;
      v_wires    = begin fun pvs   -> astnd ( V_wires pvs ) end ;
      v_proc     = begin fun p     -> astnd ( V_proc p ) end ;
    }

  let copy_value value_nd =
    map_value copy_value_mapper value_nd

    (* Close a value, selectively, using a custom function. *)
  (*let close_value_sel closef value_nd =
    let mapper value_nd =
      { (copy_value_mapper value_nd) with
        v_var = begin fun v -> closef (value_nd, v) end }
    in
    map_value mapper value_nd*)

  let rec close_value_sel closef value_nd =
    let closed_typ = close_typ_sel closef value_nd.info in
    let cvdata =
      match value_nd.data with
	| V_princ(_)
	| V_bool(_)
	| V_nat(_)
	| V_unit
        | V_string(_)
	| V_emp
	| V_clos(_)
	| V_sh(_)
        | V_proc(_)
	| V_arrloc(_) -> value_nd.data
	| V_var(vnd) -> (closef (value_nd, vnd)).data
	| V_inj(cnd, l) -> V_inj(cnd, List.map (fun vnd -> close_value_sel closef vnd) l)
	| V_row(l) -> V_row(List.map (fun (fnd, vnd) -> (fnd, close_value_sel closef vnd)) l)
	| V_ps_lit(l) -> V_ps_lit(List.map (fun vnd -> close_value_sel closef vnd) l)
	| V_ps_union(v1, v2) -> V_ps_union(close_value_sel closef v1, close_value_sel closef v2)
	| V_paren(v) -> V_paren(close_value_sel closef v)
	| V_wires(l) -> V_wires(List.map (fun (pnd, vnd) -> (pnd, close_value_sel closef vnd)) l)
    in
    { prov = value_nd.prov; data = cvdata; info = closed_typ }
      
  and close_typ_sel closef typ =
    let ctdata = match typ with
      | T_unknown
      | T_unit
      | T_nat
      | T_string
      | T_proc
      | T_bool -> typ
      | T_wire(vnd, tnd) -> T_wire(close_value_sel closef vnd, close_typnd_sel closef tnd)
      | T_sh(vnd, tnd) -> T_sh(close_value_sel closef vnd, close_typnd_sel closef tnd)
      | T_arr(vopt, tnd_arg, effnd, tnd_ret) -> T_arr(vopt, close_typnd_sel closef tnd_arg,
						      close_eff_sel closef effnd,
						      close_typnd_sel closef tnd_ret)
      | T_sum(l) ->
	T_sum(List.map (fun (cnd, tnd_l) -> (cnd, List.map (fun tnd -> close_typnd_sel closef tnd) tnd_l)) l)
      | T_row(l) -> T_row(List.map (fun (fnd, tnd) -> (fnd, close_typnd_sel closef tnd)) l)
      | T_ps(rnd) -> T_ps(close_ref_sel closef rnd)
      | T_array(tnd) -> T_array(close_typnd_sel closef tnd)
    in
    ctdata

  and close_typnd_sel closef typ_nd = { typ_nd with data = close_typ_sel closef typ_nd.data }

  and close_ref_sel closef ref_nd =
    let crdata = match ref_nd.data with
      | R_true
      | R_singl -> ref_nd.data
      | R_subeq(vnd) -> R_subeq(close_value_sel closef vnd)
      | R_eq(vnd) -> R_eq(close_value_sel closef vnd)
      | R_conj(r1, r2) -> R_conj(close_ref_sel closef r1, close_ref_sel closef r2)
      | R_not(r) -> R_not(close_ref_sel closef r)
    in
    { ref_nd with data = crdata }
	
  and close_eff_sel closef eff_nd =
    let cedata = match eff_nd.data with
      | Ef_emp -> Ef_emp
      | Ef_place(plnd) -> Ef_place(close_place_sel closef plnd)
      | Ef_cat(eff1, eff2) -> Ef_cat(close_eff_sel closef eff1, close_eff_sel closef eff2)
    in
    { eff_nd with data = cedata }

  and close_place_sel closef pl_nd =
    let cpldata = match pl_nd.data with
      | Pl_top -> Pl_top
      | Pl_ps(mode_nd, vnd) -> Pl_ps(mode_nd, close_value_sel closef vnd)
    in
    { pl_nd with data = cpldata }

    (* Close a value by an environment (completely). *)
  let close_value env value_nd =
    close_value_sel (fun (val_nd,var_nd) -> 
      try (Env.find var_nd.data env).value (* replace variable *)
      with Not_found -> val_nd (* leave free variable in place. *)
    ) value_nd
      
    (* collect a list of free variable occurrences in the given value *)
  let free_vars' value_nd =
    let emp     = fun tl -> tl in
    let singl x = fun tl -> x :: tl in
    let app x y = fun tl -> x (y tl) in
    let free_vars_mapper value_nd = {
      v_arrloc   = begin fun l     -> emp end ;
      v_var      = begin fun v     -> singl (value_nd, v) end ;
      v_princ    = begin fun p     -> emp end ;
      v_bool     = begin fun b     -> emp end ;
      v_nat      = begin fun i     -> emp end ;
      v_unit     = begin fun _     -> emp end ;
      v_string   = begin fun s     -> emp end ;
      v_inj      = begin fun c vs  -> List.fold_left app emp vs end ;
      v_row      = begin fun row   -> List.fold_left app emp (List.map snd row) end ;
      v_emp      = begin fun _     -> emp end ;
      v_ps_lit   = begin fun vs    -> List.fold_left app emp vs end ;
      v_ps_union = begin fun v1 v2 -> app v1 v2 end ;
      v_paren    = begin fun v     -> v   end ;
      v_clos     = begin fun c     -> emp end ;
      v_sh       = begin fun l     -> emp end ;
      v_wires    = begin fun pvs   -> List.fold_left app emp (List.map snd pvs) end ;
      v_proc     = begin fun p     -> emp end ;
    } in
    (map_value free_vars_mapper value_nd)

  let free_vars value_nd = free_vars' value_nd []
    
    (* collect a list of free variable occurrences in the given value *)
  let free_var_set value_nd =
    let emp     = Varvals.empty in
    let singl x = Varvals.singleton x in
    let app x y = Varvals.union x y in
    let free_vars_mapper value_nd = {
      v_arrloc   = begin fun l     -> emp end ;
      v_var      = begin fun v     -> singl (value_nd, v) end ;
      v_princ    = begin fun p     -> emp end ;
      v_bool     = begin fun b     -> emp end ;
      v_nat      = begin fun i     -> emp end ;
      v_string   = begin fun s     -> emp end ;
      v_unit     = begin fun _     -> emp end ;
      v_inj      = begin fun c vs  -> List.fold_left app emp vs end ;
      v_row      = begin fun row   -> List.fold_left app emp (List.map snd row) end ;
      v_emp      = begin fun _     -> emp end ;
      v_ps_lit   = begin fun vs    -> List.fold_left app emp vs end ;
      v_ps_union = begin fun v1 v2 -> app v1 v2 end ;
      v_paren    = begin fun v     -> v   end ;
      v_clos     = begin fun c     -> emp end ;
      v_sh       = begin fun l     -> emp end ;
      v_proc     = begin fun p     -> emp end ;
      v_wires    = begin fun pvs   -> List.fold_left app emp (List.map snd pvs) end ;
    } in
    (map_value free_vars_mapper value_nd)

    (* Returns the princs (de-contextualized from their ast nodes) *)
  let ps_value_princs v =
    let maps v = 
      { missing_case_exceptions with
        v_arrloc   = begin fun l     -> [] end ;
        v_emp      = begin fun _     -> [] end ;
        v_princ    = begin fun p     -> [p] end ;
        v_ps_lit   = begin fun vs    -> List.flatten vs end ;
        v_ps_union = begin fun xs ys -> xs @ ys end ;
        v_paren    = begin fun xs    -> xs end ;
      }
    in
    map_value maps v

    (* Returns the value nodes (each with a princ inside). *)
  let ps_value_princs' v =
    let maps v = 
      { missing_case_exceptions with
        v_emp      = begin fun _     -> [] end ;
        v_princ    = begin fun p     -> [v] end ;
        v_ps_lit   = begin fun vs    -> List.flatten vs end ;
        v_ps_union = begin fun xs ys -> xs @ ys end ;
        v_paren    = begin fun xs    -> xs end ;
      }
    in
    map_value maps v

    (* Returns the princs as a set (module Ps above). *)
  let ps_value_princ_set v =
    let maps v = 
      { missing_case_exceptions with
        v_emp      = begin fun _     -> Ps.empty end ;
        v_princ    = begin fun p     -> Ps.singleton p.data end ;
        v_ps_lit   = begin fun vs    -> List.fold_right Ps.union vs Ps.empty end ;
        v_ps_union = begin fun xs ys -> Ps.union xs ys end ;
        v_paren    = begin fun xs    -> xs end ;
      }
    in
    map_value maps v
end

  (* -- -- -- -- Folding expressions -- -- -- --. *)
  
  (* expression_folders -- a structure that gives the operations for
     specializing a parametric tree traversal/fold; read 'a as the
     type of information from "above" and 'b as the type of
     information from "below" *)
type ('a,'b) expr_folders' = {
  e_value  : value_nd -> 'b ;
  e_case   : expr_nd * ( cons_nd * pat_nd list * expr_nd ) list -> ('a * 'a list * ('b -> 'b list -> 'b)) ;
  e_cond   : expr_nd * expr_nd * expr_nd -> ('a * 'a * 'a * ('b -> 'b -> 'b -> 'b)) ;
  e_let    : pat_nd * place_nd option * expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_proj   : field_nd * expr_nd -> ('a * ('b -> 'b)) ;
  e_natop  : natop_nd * expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wire   : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wcat   : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wcopy  : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wproj  : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wapp   : value_nd * expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_waps   : value_nd * expr_nd * lam_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_wfold  : value_nd * expr_nd * expr_nd * lam_nd * expr_nd -> ('a * 'a * 'a * ('b -> 'b -> 'b -> 'b)) ;
  e_array  : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_select : expr_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
  e_update : expr_nd * expr_nd * expr_nd -> ('a * 'a * 'a * ('b -> 'b -> 'b -> 'b)) ;
  e_paren  : expr_nd -> ('a * ('b -> 'b)) ;
  e_sh     : expr_nd -> ('a * ('b -> 'b)) ;
  e_comb   : expr_nd -> ('a * ('b -> 'b)) ;
(*  e_app    : app_nd -> 'b ; *)
  e_avar   : var_nd -> 'b ;
  e_lam    : lam_nd * expr_nd -> 'a * ('b -> 'b) ;
  e_app    : app_nd * expr_nd -> ('a * 'a * ('b -> 'b -> 'b)) ;
(* printing -- *)
  e_print  : expr_nd -> ('a * ('b -> 'b)) ;
  e_sysop  : var_nd * typ_nd option * (value_nd list) -> 'b;

  e_cast   : expr_nd * typ_nd -> ('a * ('b -> 'b));
  e_subset : expr_nd * expr_nd * expr_nd -> ('a * 'a * 'a * ('b -> 'b -> 'b -> 'b)) ;
}
    
and ('a, 'b) expr_folders = 'a -> ('a, 'b) expr_folders'

let rec fold_expr : (expr_nd -> ('a,'b) expr_folders) -> expr_nd -> 'a -> 'b
  = 
  fun f e a -> 
    let f' = f e a in
    begin match e.data with
      | E_value v    -> f'.e_value v
      | E_case(e,es) -> 
        let x, xs, cont = f'.e_case (e,es) in 
        let y  = fold_expr f e x in
        let ys = List.fold_left begin fun ys ((_,_,e), x) -> 
          (fold_expr f e x) :: ys 
        end [] (List.combine es xs)
        in
        cont y (List.rev ys)

      | E_cond(e1,e2,e3) ->
        let x1, x2, x3, cont = f'.e_cond (e1,e2,e3) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        let y3 = fold_expr f e3 x3 in
        cont y1 y2 y3

      | E_let(p,pl,e1,e2) -> 
        let x1, x2, cont = f'.e_let (p,pl,e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_proj(fld,e) -> 
        let x, cont = f'.e_proj(fld,e) in
        let y = fold_expr f e x in
        cont y

      | E_natop(op,e1,e2) ->
        let x1, x2, cont = f'.e_natop(op,e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_wire(e1,e2) ->
        let x1, x2, cont = f'.e_wire(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_wcat(e1,e2) ->
        let x1, x2, cont = f'.e_wcat(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_wcopy(e1,e2) ->
        let x1, x2, cont = f'.e_wcopy(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_wproj(e1,e2) ->
        let x1, x2, cont = f'.e_wproj(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_wapp(v,e1,e2) ->
        let x1, x2, cont = f'.e_wapp(v,e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_waps(v,e1,{data=L_fix(_)}) -> raise NYI
      | E_waps(v,e1,({data=L_lam(_,e2)} as l)) -> 
        let x1, x2, cont = f'.e_waps(v,e1,l,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = 
          let x, cont = f'.e_lam(l,e2) in
          let y = fold_expr f e2 x in
          cont y
        in
        cont y1 y2

      | E_wfold(v,e1,e2,{data=L_fix(_)}) -> raise NYI
      | E_wfold(v,e1,e2,({data=L_lam(_,e3)} as l)) ->
        let x1, x2, x3, cont = f'.e_wfold(v,e1,e2,l,e3) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        let y3 = 
          let x, cont = f'.e_lam(l,e3) in
          let y = fold_expr f e2 x in
          cont y
        in
        cont y1 y2 y3

      | E_array(e1,e2) ->
        let x1, x2, cont = f'.e_array(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_select(e1,e2) ->
        let x1, x2, cont = f'.e_select(e1,e2) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        cont y1 y2

      | E_update(e1,e2,e3) ->
        let x1, x2, x3, cont = f'.e_update(e1,e2,e3) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        let y3 = fold_expr f e3 x3 in
        cont y1 y2 y3
          
      | E_paren e ->
        let x, cont = f'.e_paren e in
        let y = fold_expr f e x in
        cont y

      | E_sh e ->
        let x, cont = f'.e_sh e in
        let y = fold_expr f e x in
        cont y

      | E_comb e ->
        let x, cont = f'.e_comb e in
        let y = fold_expr f e x in
        cont y

      | E_app ({data=A_var v}) -> 
          f'.e_avar v
          
      | E_app ({data=A_lam ({data=L_lam(_,e)} as l)}) ->
          let x, cont = f'.e_lam (l, e) in
          let y = fold_expr f e x in
          cont y

      | E_app ({data=A_lam ({data=L_fix(_,_,_,e)} as l)}) ->
          let x, cont = f'.e_lam (l, e) in
          let y = fold_expr f e x in
          cont y

      | E_app ({data=A_app(a, e2)}) ->
          let x1, x2, cont = f'.e_app (a, e2) in
          let y1 = fold_expr f {e with data=E_app a} x1 in
          let y2 = fold_expr f e2 x2 in
          cont y1 y2

      | E_app ({data=A_paren a}) ->
          let x, cont = f'.e_paren {e with data=E_app a} in
          let y = fold_expr f {e with data=E_app a} x in
          cont y

      | E_print(e) ->
          let x, cont = f'.e_print(e) in
          let y = fold_expr f e x in
          cont y

      | E_sysop(v, tyop, l) -> f'.e_sysop(v, tyop (* XXX: Does not visit the type *), l)

      | E_cast(e,t) -> 
        let x, cont = f'.e_cast(e,t) in
        let y = fold_expr f e x in
        cont y

      | E_subset(e1,e2,e3) ->
        let x1, x2, x3, cont = f'.e_subset (e1,e2,e3) in
        let y1 = fold_expr f e1 x1 in
        let y2 = fold_expr f e2 x2 in
        let y3 = fold_expr f e3 x3 in
        cont y1 y2 y3

    end

let expr_copy_folders e : (unit, expr_nd) expr_folders = 
  let g x = {e with data=x} in
  let units xs = List.map (fun _ -> ()) xs in 
  let ga (a:app) : app_nd = { data=a; info=e.info; prov=e.prov } in
  let rec app_of_exp (e:expr_nd) : app_nd = 
    match e.data with
      | E_app a   -> a
      | E_paren e -> app_of_exp e
      | _ -> invalid_arg (Global.Prov.sprint_prov "app_of_exp: " e.prov)
  in
  let expr_of_lam (l:lam_nd) : expr_nd = 
    {data=E_app {data=A_lam l; info=l.info; prov=l.prov};
     info=l.info;
     prov=l.prov}
  in
  let pat_of_lam (l:lam_nd) : pat_nd = 
    match l.data with
      | L_lam(p, _)    -> p
      | L_fix(_,_,p,_) -> p
  in
  let lam_with_body (l:lam_nd) (e:expr_nd) : lam_nd =
    {l with data=L_lam((pat_of_lam l), e)}
  in
  fun _ ->
    {
      e_value  = begin fun v              -> g (E_value v) end ;
      e_case   = begin fun (_,bs)         -> (), (units bs), fun y ys -> g (E_case (y, List.map2 (fun (c,ps,_) y -> (c,ps,y)) bs ys)) end ;      
      e_cond   = begin fun (e1,e2,e3)     -> (), (), (), fun y1 y2 y3 -> g (E_cond (y1, y2, y3)) end ;
      e_let    = begin fun (p,pl,_,_)     -> (), (), fun y1 y2 -> g(E_let (p,pl,y1,y2)) end ;
      e_proj   = begin fun (fld,e)        -> (), fun y -> g(E_proj (fld, y)) end ;      
      e_natop  = begin fun (natop,e1,e2)  -> (), (), fun y1 y2 -> g(E_natop(natop, y1, y2)) end ;
      e_wire   = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_wire(y1, y2)) end ;
      e_wcat   = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_wcat(y1, y2)) end ;
      e_wcopy  = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_wcopy(y1, y2)) end ;
      e_wproj  = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_wproj(y1, y2)) end ;
      e_wapp   = begin fun (v,e1,e2)      -> (), (), fun y1 y2 -> g(E_wapp(v, y1, y2)) end ;
      e_waps   = begin fun (v,e1,l,e2)    -> (), (), fun y1 y2 -> g(E_waps(v, y1, lam_with_body l y2)) end ;
      e_wfold  = begin fun (v,e1,e2,l,e3) -> (), (), (), fun y1 y2 y3 -> g(E_wfold(v, y1, y2, lam_with_body l y3)) end ;
      e_array  = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_array(y1, y2)) end ;
      e_select = begin fun (e1,e2)        -> (), (), fun y1 y2 -> g(E_select(y1, y2)) end ;
      e_update = begin fun (e1,e2,e3)     -> (), (), (), fun y1 y2 y3 -> g(E_update(y1, y2, y3)) end ;
      e_paren  = begin fun e              -> (), fun y -> g(E_paren y) end ;
      e_sh     = begin fun e              -> (), fun y -> g(E_sh y) end ;
      e_comb   = begin fun e              -> (), fun y -> g(E_comb y) end ;
      e_lam    = begin fun (l,e)          -> (), fun y -> expr_of_lam (lam_with_body l y) end ;
      e_app    = begin fun (a,e)          -> (), (), fun y1 y2 -> g(E_app(ga(A_app(app_of_exp y1, y2)))) end ;
      e_avar   = begin fun v              -> g(E_app(ga(A_var v))) end ;
      e_print  = begin fun e              -> (), fun y -> g(E_print y) end ;
      e_sysop  = begin fun (vr,tyop,l)    -> g (E_sysop (vr, tyop, l)) end ;

      e_cast   = begin fun (e,t)          -> (), fun y -> g(E_cast (y,t)) end ;   
      e_subset = begin fun (e1,e2,e3)     -> (), (), (), fun y1 y2 y3 -> g (E_subset (y1, y2, y3)) end ;

    }

(* NOTE!: Assumes that program variables are all unique! *)
(* XXX *)
(* To lift this restriction, feed through a set of bound variables; 
   do not substitute any variable int he bound set. *)
(*let expr_close_sel closef e =
  let folders e x : (unit, expr_nd) expr_folders' = 
    { 
      (expr_copy_folders e x)
      with
        e_value = begin fun v -> 
          {e with data=E_value(AstMap.close_value_sel closef v)} end ;  }
  in
  fold_expr folders e ()*)

let rec expr_close_sel closef e =
  let closed_typ = AstMap.close_typ_sel closef e.info in
  let edata = match e.data with
    | E_value(vnd) -> E_value(AstMap.close_value_sel closef vnd)
    | E_case(e1, l) ->
      E_case(expr_close_sel closef e1,
	     List.map (fun (cnd, plist, e2) -> (cnd, plist, expr_close_sel closef e2)) l)
    | E_cond(e1, e2, e3) -> E_cond(expr_close_sel closef e1, expr_close_sel closef e2, expr_close_sel closef e3)
    | E_let(patnd, plndopt, e1, e2) ->
      let cplndopt = match plndopt with
	| None -> None
	| Some(p) -> Some(AstMap.close_place_sel closef p)
      in
      E_let(pat_close_sel closef patnd, cplndopt, expr_close_sel closef e1, expr_close_sel closef e2)
    | E_app(appnd) -> E_app(app_close_sel closef appnd)
    | E_proj(fnd, e1) -> E_proj(fnd, expr_close_sel closef e1)
    | E_natop(opnd, e1, e2) -> E_natop(opnd, expr_close_sel closef e1, expr_close_sel closef e2)
    | E_print(e1) -> E_print(expr_close_sel closef e1)
    | E_sysop(varnd, tyop, l) -> E_sysop(varnd, tyop, List.map (fun vnd -> AstMap.close_value_sel closef vnd) l)
    | E_array(e1, e2) -> E_array(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_select(e1, e2) -> E_select(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_update(e1, e2, e3) -> E_update(expr_close_sel closef e1, expr_close_sel closef e2, expr_close_sel closef e3)
    | E_sh(e1) -> E_sh(expr_close_sel closef e1)
    | E_comb(e1) -> E_comb(expr_close_sel closef e1)
    | E_wire(e1, e2) -> E_wire(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_wcat(e1, e2) -> E_wcat(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_wcopy(e1, e2) -> E_wcopy(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_wproj(e1, e2) -> E_wproj(expr_close_sel closef e1, expr_close_sel closef e2)
    | E_wapp(v, e1, e2) -> E_wapp(AstMap.close_value_sel closef v, expr_close_sel closef e1, expr_close_sel closef e2)
    | E_waps(v, e1, lamnd) -> E_waps(AstMap.close_value_sel closef v, expr_close_sel closef e1,
				     lam_close_sel closef lamnd)
    | E_wfold(v, e1, e2, lamnd) -> E_wfold(AstMap.close_value_sel closef v, expr_close_sel closef e1,
					   expr_close_sel closef e2, lam_close_sel closef lamnd)
    | E_paren(e1) -> E_paren(expr_close_sel closef e1)

    | E_cast(e1, t) -> E_cast(expr_close_sel closef e1, t)
    | E_subset(e1, e2, e3) -> E_subset(expr_close_sel closef e1, expr_close_sel closef e2, expr_close_sel closef e3)
  in
  { prov = e.prov; data = edata; info = closed_typ }

and app_close_sel closef app =
  let adata = match app.data with
    | A_var(_) -> app.data (* NOTE: is this right or this var should close ? *)
    | A_lam(lamnd) -> A_lam(lam_close_sel closef lamnd)
    | A_app(app1, e) -> A_app(app_close_sel closef app1, expr_close_sel closef e)
    | A_paren(app1) -> A_paren(app_close_sel closef app1)
  in
  { app with data = adata }

and lam_close_sel closef lam =
  let ldata = match lam.data with
    | L_fix(vnd, tnd, patnd, exnd) -> L_fix(vnd, AstMap.close_typnd_sel closef tnd, pat_close_sel closef patnd,
					    expr_close_sel closef exnd)
    | L_lam(patnd, exnd) -> L_lam(pat_close_sel closef patnd, expr_close_sel closef exnd)
  in
  { lam with data = ldata }

and pat_close_sel closef pat =
  let pdata = match pat.data with
    | P_var(v) -> pat.data
    | P_cast(pat1, tnd) -> P_cast(pat_close_sel closef pat1, AstMap.close_typnd_sel closef tnd)
    | P_row(l) -> P_row(List.map (fun (fnd, pat1) -> (fnd, pat_close_sel closef pat1)) l)
  in
  { pat with data = pdata }

let expr_close env e =
  expr_close_sel     
    (fun (value_nd, var_nd) -> 
      try
	(Env.find var_nd.data env).value
      with
	| _ -> value_nd
	  (*begin
	    print_string "not found = ";
	    match var_nd.data with
	      | Var(s) -> print_string s;
	      | _ -> (invalid_arg "")
	  end;
	  raise Not_found*)
    )
    
    e   

(* XXX --- BUG: Let bindings not handled correctly. *)
let expr_free_vars' e : varval list -> varval list = 
  let units xs = List.map (fun _ -> ()) xs in 
  let emp     = fun tl -> tl in
  let app x y = fun tl -> x (y tl) in
  let app3 = fun x y z -> app x (app y z) in
  let folders = fun _ _ -> {
    e_value = begin fun v               -> AstMap.free_vars' v end ;
    e_case  = begin fun (_,bs)          -> (), (units bs), fun y ys -> List.fold_left app y ys end ;
    e_cond   = begin fun (e1, e2, e3)    -> (), (), (), fun y1 y2 y3 -> app y1 (app y2 y3) end ;
    e_let    = begin fun (p,pl,_,_)      -> (), (), fun y1 y2 -> app y1 y2 end ;    
    e_proj   = begin fun (fld, e)        -> (), fun y -> y end ;
    e_natop  = begin fun (natop, e1, e2) -> (), (), app end ;
    e_wire   = begin fun (e1, e2)        -> (), (), app end ;
    e_wcat   = begin fun (e1, e2)        -> (), (), app end ;
    e_wcopy  = begin fun (e1, e2)        -> (), (), app end ;
    e_wproj  = begin fun (e1, e2)        -> (), (), app end ;
    e_wapp   = begin fun (v, e1, e2)     -> (), (), app end ;
    e_waps   = begin fun (v, e1, l, e2)  -> (), (), app end ;
    e_wfold  = begin fun (v,e1,e2,l,e3)  -> (), (), (), app3 end ;
    e_array  = begin fun (e1, e2)        -> (), (), app end ;
    e_select = begin fun (e1, e2)        -> (), (), app end ;
    e_update = begin fun (e1, e2, e3)    -> (), (), (), app3 end ;
    e_paren  = begin fun e               -> (), fun y -> y end ;
    e_sh     = begin fun e               -> (), fun y -> y end ;
    e_comb   = begin fun e               -> (), fun y -> y end ;
    e_lam    = begin fun (l, e)          -> (), fun y -> y end ; (* TODO -- bound variable of lam *)
    e_app    = begin fun (a, e)          -> (), (), app end ;
    e_avar   = begin fun v               -> emp (* TODO -- not adding v *) end ;
    e_print  = begin fun e               -> (), fun y -> y end ;
    e_sysop  = begin fun (vr, tyop, l)   -> List.fold_left (fun y v -> app y (AstMap.free_vars' v)) emp l end;

    e_cast   = begin fun (e, t)          -> (), fun y -> y end ;
    e_subset = begin fun (e1, e2, e3)    -> (), (), (), fun y1 y2 y3 -> app y1 (app y2 y3) end ;
  }
  in
  (fold_expr folders e ())

let expr_free_vars e = expr_free_vars' e []

module BoundVars = Set.Make(String)

let varname varnd =
  let Var(vname) = varnd.data in
  vname

let rec patvarname patnd = match patnd.data with
  | P_var(varnd)  -> varname varnd
  | P_cast(p,typ) -> patvarname p
  | _ -> raise (Invalid_argument "Invalid input pattern to patvarname")    

let bv_add_pat bv pat =
  BoundVars.add (patvarname pat) bv

let bv_filter bv varvals =
  Varvals.filter begin 
      (* filter out all bound variables *)
    fun (_, {data=Var v}) -> not (BoundVars.mem v bv) 
  end varvals

let bv_pats ps bv =
  List.fold_right begin fun p bv ->
    bv_add_pat bv p
  end ps bv

let expr_free_var_set e  = 
  let emp      = Varvals.empty in
  let app x y  = Varvals.union x y in
  let app3     = fun x y z -> app x (app y z) in
  let lam_pat l = match l.data with
    | L_lam (p,_) -> p
    | L_fix (_,_,p,_) -> p
  in
  let folders = fun _ bv -> {
    e_value  = begin fun v               -> bv_filter bv (AstMap.free_var_set v) end ;
    e_case   = begin fun (_,bs)          -> bv, (List.map (fun (_,ps,_) -> bv_pats ps bv) bs), fun y ys -> List.fold_left app y ys end ;
    e_cond   = begin fun (e1, e2, e3)    -> bv, bv, bv, (fun x y z -> app x (app y z)) end ;
    e_let    = begin fun (p,pl,_,_)      -> bv, bv_add_pat bv p, fun y1 y2 -> app y1 y2 end ;
    e_proj   = begin fun (fld, e)        -> bv, fun y -> y end ;
    e_natop  = begin fun (natop, e1, e2) -> bv, bv, app end ;
    e_wire   = begin fun (e1, e2)        -> bv, bv, app end ;
    e_wcat   = begin fun (e1, e2)        -> bv, bv, app end ;
    e_wcopy  = begin fun (e1, e2)        -> bv, bv, app end ;
    e_wproj  = begin fun (e1, e2)        -> bv, bv, app end ;
    e_wapp   = begin fun (v, e1, e2)     -> bv, bv, app end ;
    e_waps   = begin fun (v, e1, l, e2)  -> bv, bv, app end ;
    e_wfold  = begin fun (v,e1,e2,l,e)   -> bv, bv, bv, app3 end ;
    e_array  = begin fun (e1, e2)        -> bv, bv, app end ;
    e_select = begin fun (e1, e2)        -> bv, bv, app end ;
    e_update = begin fun (e1, e2, e3)    -> bv, bv, bv, app3 end ;
    e_paren  = begin fun e               -> bv, fun y -> y end ;
    e_sh     = begin fun e               -> bv, fun y -> y end ;
    e_comb   = begin fun e               -> bv, fun y -> y end ;
    e_lam    = begin fun (l, e)          -> bv_add_pat bv (lam_pat l), fun y -> y end ;
    e_app    = begin fun (a, e)          -> bv, bv, app end ;
    e_avar   = begin fun v               -> emp (* TODO -- not adding v; no value node *) end ;
    e_print  = begin fun e               -> bv, fun y -> y end ;
    e_sysop  = begin fun (vr, tyop, l)   -> bv_filter bv (List.fold_left (fun y v -> app y (AstMap.free_var_set v)) emp l) end;

    e_cast   = begin fun (e, t)        -> bv, fun y -> y end ;
    e_subset = begin fun (e1, e2, e3)    -> bv, bv, bv, (fun x y z -> app x (app y z)) end ;
  }
  in
  (fold_expr folders e BoundVars.empty)

(* no reason for this to be here *)
module Library = struct
    
  exception CLibError of string

  let varname varnd =
    let Var(vname) = varnd.data in
    vname

  let consname consnd =
    let Cons(cname) = consnd.data in
    cname

  let fldname fldnd =
    let Field(cname) = fldnd.data in
    cname

  let patvarname patnd = match patnd.data with
    | P_var(varnd) -> varname varnd
    | P_cast(p,typ) -> patvarname p
    | _ -> raise (CLibError "Invalid input pattern to patvarname")    

  let rec firstn l n = match (l, n) with
    | _, 0 -> []
    | x::l', n -> x::(firstn l' (n - 1))
    | _ -> raise (CLibError "Unexpected error in firstn in genex")
      
  let sortfields l =    (* can be a (f, v) or (f, t) list *)
    let cmp elt1 elt2 =
      match ((fst elt1).data, (fst elt2).data) with
	| Field(id1), Field(id2) -> compare id1 id2
    in
    List.sort cmp l  

  let getfieldindex l s =    (* can be (f, v) or (f, t), and return v or t with offset *)
    let rec helper n = function
      | [] -> raise (CLibError (String.concat "" ["getfieldoffset: field not found: "; s]))
      | (fldnd, x) :: l' ->
	let Field(s1) = fldnd.data in
	if s = s1 then (n, x) else helper (n + 1) l'
    in
    helper 1 l
      
  let is_closedprincs vnd =
    let rec helper = function
      | V_princ(pnd) -> [pnd]
      | V_ps_union(v1, v2) -> helper v1.data @ helper v2.data
      | V_ps_lit(l) -> List.flatten (List.map (fun v -> helper v.data) l)
      | _ -> raise (CLibError "Princs not closed")
    in
    helper vnd.data

  let sortprincs l =
    let cmp pnd1 pnd2 = compare pnd1.data pnd2.data in
    List.sort cmp l
      
  let getprincoffset l s =
    let rec helper n = function
      | [] -> raise (CLibError "getprincoffset: princ not found")
      | pnd :: l' ->	  
	if s = pnd.data then n else helper (n + 1) l'
    in
    helper 1 l

  let existsprinc pndl s =
    List.exists (fun pnd1 -> pnd1.data = s) pndl      
      
end 

(* "Pretty"-print the AST *)
module Pretty = struct
    
  let ps = print_string

  let rec pp_list pr xs sep =      
    match xs with 
      | []    -> ()

      | x::[] -> pr x
      | x::xs -> pr x ; 
        ps sep ; 
        pp_list pr xs sep

  let pp_var (Var s) = ps s
  let pp_cons (Cons s) = ps s

  let pp_field = function
    | Field s -> ps s

  let pp_princ a = ps a

  let pp_var_nd n = pp_var n.data
  let pp_cons_nd n = pp_cons n.data
  let pp_field_nd n = pp_field n.data
  let pp_princ_nd n = pp_princ n.data

  let pp_space _ = ps " "

  let rec pp_file_nd n = pp_file n.data
  and pp_file = function
    | F_body e     -> pp_expr_nd e
    | F_defn (d,f) -> pp_defn_nd d ; 
      ps "\n;\n" ; 
      pp_file_nd f

  and pp_defn_nd n = pp_defn n.data
  and pp_defn = function
    | D_funtyp (v,t) ->
      pp_var_nd v ; ps " : " ;
      pp_typ_nd t 
    | D_fundefn (v,ps,e) -> 
      pp_var_nd v ; pp_space () ;
      pp_list pp_pat_nd ps " " ; pp_space () ;
      pp_expr_nd e

  and pp_value_nd n = pp_value n.data
  and pp_value = function
    | V_var v -> pp_var_nd v
    | V_princ {data=p} -> ps p
    | V_arrloc l -> ps ("%"^(string_of_int l))
    | V_bool true  -> ps "true"
    | V_bool false -> ps "false"
    | V_unit  -> ps "()"
    | V_nat n -> ps (string_of_int n)
    | V_string s -> ps "\"" ; ps s (* XXX: Should escape the string *) ; ps "\""
    | V_inj(c,vs) -> 	
      pp_cons_nd c ;
      ( match vs with [] -> ()
        | vs -> ps " " ; pp_list pp_value_nd vs " " )
    | V_row r -> ps "{" ;
      pp_list (fun (f,v) -> 
        pp_field_nd f ; 
        ps ":" ;
        pp_value_nd v) r "," ;
      ps "}"
    | V_emp -> ps "{}"
    | V_ps_lit vs -> ps "{" ;
      pp_list pp_value_nd vs "," ;
      ps "}"
    | V_ps_union (v1,v2) -> 
      pp_value_nd v1 ; 
      ps " union " ;
      pp_value_nd v2
    | V_paren v -> ps "(" ;
      pp_value_nd v ;
      ps ")"
    | V_clos c ->
      ps "clos(";
      pp_env "" c.clos_env ;
      ps ", ";
      pp_lam_nd c.clos_lam ;
      ps ")" ;
    | V_sh l ->
      ps "sh "; print_int (Bytes.length l);
    | V_wires pvs ->
      ps "[" ;
      pp_list begin fun (p,v) ->
        pp_princ_nd p ;
        ps ":";
        pp_value_nd v
      end pvs ", " ;
      ps "]"
    | V_proc p -> ps "proc<\'" ; ps (Proc.to_string p) ; ps "\'>"

  and pp_env sep_nxt env =
    ps "{" ;
    let _ =
      Env.fold begin fun _ bnd comma_prefix ->
        if comma_prefix then ps (", "^sep_nxt) else () ;
        pp_var_nd bnd.var ;
        ps " : " ;        
        pp_value_nd bnd.value ;
        ps " @ " ;
        pp_place_nd bnd.place ; true
      end env false 
    in () ;
    ps "}" ;       
      
  and pp_place_nd n = pp_place n.data
  and pp_place = function
    | Pl_top -> ps "top"
    | Pl_ps (m,v) -> pp_pl_mode_nd m ;
      ps "(" ;
      pp_value_nd v ;
      ps ")" ;
      
  and pp_pl_mode_nd n = pp_pl_mode n.data
  and pp_pl_mode = function
    | Sec -> ps "sec"
    | Par -> ps "par"

  and pp_refine_nd n = pp_refine n.data
  and pp_refine = function
    | R_true -> ps "true"
    | R_singl -> ps "singl"
    | R_subeq v -> ps "subeq " ; pp_value_nd v
    | R_eq v -> ps "= " ; pp_value_nd v
    | R_conj(r1,r2) -> pp_refine_nd r1 ;
      ps " and " ;
      pp_refine_nd r2
    | R_not r -> ps "not "; pp_refine_nd r

  and pp_eff_nd n = pp_eff n.data
  and pp_eff = function
    | Ef_emp -> ps "emp"
    | Ef_place p -> pp_place_nd p
    | Ef_cat(e1,e2) -> pp_eff_nd e1 ; ps ", " ; pp_eff_nd e2
      
  and pp_typ_nd n = pp_typ n.data
  and pp_typ = function
    | T_unknown -> ps "unknown"
    | T_proc -> ps "proc"
    | T_string -> ps "string"
    | T_nat -> ps "nat"
    | T_bool -> ps "bool"
    | T_unit -> ps "unit"
    (*| T_var v -> pp_var_nd v*)
    | T_wire (v,t) -> ps "W " ;
      pp_value_nd v ; pp_space () ;
      pp_typ_nd t
    | T_sh (v,t) -> ps "Sh " ;
      pp_value_nd v ; ps ":"; pp_typ (v.info); pp_space () ;
      pp_typ_nd t
    | T_array t -> ps "array " ; pp_typ_nd t
    (*| T_appv (t, v) -> pp_typ_nd t ; pp_space () ; pp_value_nd v *)
    (*| T_appt (t1, t2) -> pp_typ_nd t1 ; pp_space () ; pp_typ_nd t2*)
    | T_arr (vo, t1, e, t2) ->      
      ( match vo with
        | None ->
          pp_typ_nd t1 
        | Some v ->
          ps "(" ;
          pp_var_nd v ;
          ps ":" ;
          pp_typ_nd t1 ;
          ps ")" ) ;
      ps " -" ;
      pp_eff_nd e ; ps "-> " ;
      pp_typ_nd t2
    | T_sum cs ->	
      ps "[" ;
      pp_list begin fun (c,ts) ->
        pp_cons_nd c ;
        pp_space () ; pp_list pp_typ_nd ts " " 
      end cs " | " ;
      ps "]" ;
    | T_row rs ->
      ps "{" ;
      pp_list begin fun (f,t) ->
        pp_field_nd f ; ps ":" ; pp_typ_nd t
      end rs ", " ;
      ps "}" ;
    | T_ps r -> 
      ps "ps{" ; 
      pp_refine_nd r ; 
      ps "}"
        
  and pp_lam_nd n = pp_lam n.data
  and pp_lam = function
    | L_fix (v,_,p,e) -> 
      ps "fix " ; pp_var_nd v ; 
      ps "\\" ;  pp_pat_nd p ;
      ps ". " ; pp_expr_nd e
    | L_lam (p,e) ->
      ps "\\" ;  pp_pat_nd p ;
      ps ". " ; pp_expr_nd e

  and pp_pat_nd n = pp_pat n.data
  and pp_pat = function
    | P_var v -> pp_var_nd v
    | P_cast (p,t) -> ps "(" ;
      pp_pat_nd p ;
      ps ":" ;
      pp_typ_nd t ; ps ")"
    | P_row r ->
      ps "{" ;
      pp_list begin fun (f,p) ->
        pp_field_nd f ;
        ps ":" ;
        pp_pat_nd p ;
      end r ", " ;
      ps "}"

  and pp_expr_nd n = pp_expr n.data
  and pp_expr = function
    | E_value v -> pp_value_nd v
    | E_case (e, bs) ->
      ps "match " ;
      pp_expr_nd e ;
      ps " with " ;
      pp_list begin fun (c, prs, e) ->
        pp_cons_nd c ; pp_space () ;
        pp_list pp_pat_nd prs " " ; 
        ps " => " ;
        pp_expr_nd e
      end bs " | " 
      ; ps " end" 
    | E_cond (e1, e2, e3) -> 
      ps "if " ;
      pp_expr_nd e1 ;
      ps " then " ;
      pp_expr_nd e2 ;
      ps " else " ;
      pp_expr_nd e3
    | E_let (p, plo, e1, e2) ->
      ps "let " ;
      pp_pat_nd p ;
      ( match plo with
        | Some pl -> ps " at " ; pp_place_nd pl ;
        | None -> () ) ;
      ps " = " ;
      pp_expr_nd e1 ; ps " in " ;
      pp_expr_nd e2
    | E_app a      -> pp_app_nd a
    | E_proj (f,e) -> 
      pp_expr_nd e ; ps "." ;
      pp_field_nd f
    | E_natop(op, e1, e2) ->
      pp_expr_nd e1 ; 
      ( match op.data with
        | Natop_plus -> ps " + " 
	| Natop_sub -> ps " - "
        | Natop_gt -> ps " > " 
        | Natop_equals -> ps " = " ) ;
      pp_expr_nd e2            
      (* Wire operations *)
    | E_wire (e1,e2) ->
      ps "wire " ;
      pp_expr_nd e1 ;
      ps " : " ;
      pp_expr_nd e2
    | E_wcat (e1, e2) -> 
      pp_expr_nd e1 ;
      ps " ++ " ;
      pp_expr_nd e2 ;
    | E_wcopy (e1, e2) -> 
      ps "wcopy ";
      pp_expr_nd e1 ;
      ps " " ;
      pp_expr_nd e2 ;
    | E_wproj (e1, e2) ->
      pp_expr_nd e1 ;	
      ps "[" ;
      pp_expr_nd e2 ;
      ps "]" 
    | E_wapp (v, e1, e2) ->
      ps "wapp "; 
      pp_value_nd v ;
      ps " [" ;
      pp_expr_nd e1 ;
      ps "; ";
      pp_expr_nd e2 ;
      ps "]"
    | E_waps (v, e, l) ->
      ps "waps "; 
      pp_value_nd v ;
      ps " [" ;
      pp_expr_nd e ;
      ps "; ";
      pp_lam_nd l ;
      ps "]"
    | E_wfold (v, e1, e2, l) ->
      ps "wfold " ;
      pp_value_nd v ;
      ps " [" ;
      pp_expr_nd e1 ;
      ps "; ";
      pp_expr_nd e2 ;
      ps "; ";
      pp_lam_nd l ;
      ps "]"
    | E_array (e1, e2) ->
      ps "array ["; pp_expr_nd e1 ; 
      ps "] of " ; pp_expr_nd e2
    | E_select (e1, e2) ->
      ps "select "; pp_expr_nd e1 ; ps "[" ;
      pp_expr_nd e2 ; ps "]"
    | E_update (e1, e2, e3) ->
      ps "update " ; pp_expr_nd e1 ; ps "[" ;
      pp_expr_nd e2 ; ps "] <- " ;
      pp_expr_nd e3
    | E_paren e ->
      ps "(" ;
      pp_expr_nd e ;
      ps ")"
    | E_sh e ->
      ps "sh " ;
      pp_expr_nd e ;
    | E_comb e ->
      ps "comb " ;
      pp_expr_nd e ;
    | E_print e ->
      ps "print " ;
      pp_expr_nd e
    | E_cast (e, t) -> 
      ps "cast ";
      pp_expr_nd e ; ps " ";
      pp_typ_nd t
    | E_sysop(vr, tyop, l) ->
      ps "sysop "; 
      pp_var (vr.data); ps " ";
      begin match tyop with
        | None -> ()
        | Some t -> (pp_typ_nd t ; ps " ")
      end ;
      List.iter (fun v -> pp_value_nd v; ps " ") l

    | E_subset (e1, e2, e3) -> 
      ps "subset " ;
      pp_expr_nd e1 ;
      ps " " ;
      pp_expr_nd e2 ;
      ps " " ;
      pp_expr_nd e3


  and pp_app_nd n = pp_app n.data
  and pp_app = function
    | A_var v -> pp_var_nd v
    | A_lam l -> pp_lam_nd l
    | A_app (a,e) -> ps "(" ; pp_app_nd a ; ps ")(" ; pp_expr_nd e ; ps ")"
    | A_paren a -> ps "(" ; pp_app_nd a ; ps ")"

  (* ------------------------------------------------ *)
  (* Serializing values as strings: *)
  exception Not_supported

  let rec string_of_value_nd v = 
    string_of_value v.data

  and string_of_value =
    let ps s = s in
    let (@@) a b = a ^ b in    
    let rec string_of_list pr xs sep =
      match xs with 
        | []    -> ""
        | x::[] -> pr x
        | x::xs -> 
            pr x @@
              ps sep @@
              string_of_list pr xs sep
    in
    function
      | V_var v -> (raise Not_supported)
      | V_clos c -> (raise Not_supported)
      | V_arrloc l -> (raise Not_supported)

      | V_princ {data=p} -> ps p
      | V_bool true  -> ps "true"
      | V_bool false -> ps "false"
      | V_unit  -> ps "()"
      | V_nat n -> ps (string_of_int n)
      | V_string s -> ps "\"" @@ ps s (* XXX: Should escape the string *) @@ ps "\""
      | V_inj(c,vs) -> 	
          let (Cons c) = c.data in
          ps c @@
            ( match vs with [] -> ""
                | vs -> ps " " @@ string_of_list string_of_value_nd vs " " )

      | V_row r -> ps "{" @@
          string_of_list (fun (f,v) -> 
                     let (Field s) = f.data in
                     ps s @@ 
                     ps ":" @@
                     string_of_value_nd v) r "," @@
          ps "}"
      | V_emp -> ps "{}"
      | V_ps_lit vs -> ps "{" @@
          string_of_list string_of_value_nd vs "," @@
          ps "}"
      | V_ps_union (v1,v2) -> 
          string_of_value_nd v1 @@ 
          ps " union " @@
          string_of_value_nd v2
      | V_paren v -> ps "(" @@
          string_of_value_nd v @@
          ps ")"
      | V_sh l ->
          (*ps "sh " @@ string_of_int (List.length l)*)
	ps (Bytes.to_string l)
      | V_wires pvs ->
          ps "[" @@
          string_of_list begin fun (p,v) ->
            ps p.data @@
            ps ":"@@
            string_of_value_nd v
          end pvs ", " @@
          ps "]"
      | V_proc p -> ps "proc<\'" @@ ps (Proc.to_string p) @@ ps "\'>"

end (* Pretty module *)
