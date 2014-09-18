type princ = Ast.princ

module type OPSEM_FLAGS = sig
  val variant : [
    `Multi_party_simulation of Ast.princ list
  | `Single_party_protocol  of Ast.princ
  ]
end

module Make (Flags:OPSEM_FLAGS) = struct
  module Flags = Flags
  open Ast

  (* stack frames *)
  type frm = { frm_place : place_nd ;
               frm_cont  : expr_nd -> env * expr_nd ; }

  (* a stack is a list of frames (topmost to bottom-most) *)
  and stk = frm list

  module Store = Map.Make(struct type t = int
                                 let compare = compare end)
  type store = (value_nd array) Store.t

  (* machine configurations that take small steps. *)
  type cfg = {
    nxtloc : int ;
    store : store ;
    env   : env ;
    place : place_nd ;
    expr  : expr_nd ;
    stk   : stk ;
  }

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  exception Stuck_        (* no info *)
  exception Stuck of cfg  (* with full stuck configuration *)
  exception Impossible
  exception NYI           (* Not yet implemented. *)

  exception Parse_error of exn * (int * int * string)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* Missing pieces *)

  let fresh_var      = Ast.Macros.fresh_var
  let fresh_var_pat  = Ast.Macros.fresh_var_pat
  let fresh_var_pat' = Ast.Macros.fresh_var_pat'

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let pat_match : cfg -> pat_nd -> value_nd -> env = fun cfg ->
    let rec pat_match = (* call recursively with a different environment. *)
      fun env p value ->
        begin match p.data with
          | P_var var ->
              Env.add var.data {
                var   = var ;
                place = cfg.place ;
                value = value } env

          | P_cast (p, _) -> pat_match env p value

          | P_row row_pat -> List.fold_right begin fun ({data=Field fld1},fld1_pat) env ->
              match value.data with
                | V_row row_v ->
                    let rec loop row_v = match row_v with
                      | [] -> raise (Stuck cfg)
                      | ({data=Field fld2}, fld2_v) :: _ when fld1 = fld2 -> pat_match env fld1_pat fld2_v
                      | _ :: row_v -> loop row_v
                    in
                    loop row_v
                | _ -> raise (Stuck cfg)
            end row_pat env
        end
    in
    pat_match cfg.env

  let pat_matchs : cfg -> pat_nd list -> value_nd list -> env =
    fun cfg ps vs ->
      let pvs = List.combine ps vs in
      List.fold_left begin fun env (p,v) ->
        pat_match {cfg with env=env} p v
      end cfg.env pvs


  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let cons_eq : cons_nd -> cons_nd -> bool =
    fun c d -> (c.data = d.data)

  let close_value : cfg -> value_nd -> value_nd = fun cfg v ->
    Ast.AstMap.close_value cfg.env v

  let rec close_value_of_expr : cfg -> expr -> value_nd option = fun cfg -> function
      | E_value v -> Some (close_value cfg v)
      | E_paren e -> close_value_of_expr cfg e.data
      | _         -> None

  let rec close_value_of_expr' : env -> expr -> value_nd = fun env -> function
      | E_value v -> Ast.AstMap.close_value env v
      | _         -> raise Impossible

  let rec close_value_of_expr'' : (varval -> value_nd) -> expr -> value_nd =
    fun closf -> function
      | E_value v -> Ast.AstMap.close_value_sel closf v
      | _         -> raise Impossible

  let expr_is_value : cfg -> expr -> bool = fun cfg e ->
    match close_value_of_expr cfg e with
      | Some _ -> true
      | None -> false

  let expr_is_closed_value : cfg -> expr -> bool = fun cfg e ->
    let e_nd = {data=e; info=Ast.T_unknown; prov=Global.Prov.Synth} in
    (expr_is_value cfg e) &&
      ( (expr_free_var_set e_nd) = Ast.Varvals.empty )

  let ps_value_princs : value_nd -> princ_nd list =
    Ast.AstMap.ps_value_princs

  let expr_of_lam (l:lam_nd) : expr_nd =
    {data=E_app {data=A_lam l; info=l.info; prov=l.prov};
     info=l.info;
     prov=l.prov}

  let lam_of_expr (e:expr_nd) : lam_nd =
    match e.data with
      | E_app ({data=A_lam l}) -> l
      | _ -> invalid_arg "lam_of_expr"

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let can_delegate : place_nd -> place_nd -> bool =
    fun p q ->
      match p.data, q.data with
        | _ , Pl_top -> raise Stuck_

        | Pl_top, _  -> raise Stuck_

        | Pl_ps ({data=Sec}, ps_princs), Pl_ps ({data=Par}, qs_princs) ->
            (* No Sec to Par. *)
            raise Stuck_

        | Pl_ps ({data=Par}, ps_princs), Pl_ps ({data=Par}, qs_princs) ->
            let ps = Ast.AstMap.ps_value_princ_set ps_princs in
            let qs = Ast.AstMap.ps_value_princ_set qs_princs in
            ( Ps.subset qs ps )

        | Pl_ps ({data=_}, ps_princs), Pl_ps ({data=Sec}, qs_princs) ->
            let ps = Ast.AstMap.ps_value_princ_set ps_princs in
            let qs = Ast.AstMap.ps_value_princ_set qs_princs in
            ( Ps.equal qs ps )

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let check_place_membership : cfg -> princ -> place_nd ->
    [`Delegate | `Skip | `Sec_block of princ list ]
    =
    fun cfg p pl ->
      match pl.data with
        | Pl_top -> (*raise (Stuck cfg)*) `Delegate

        | Pl_ps ({data=Sec}, ps_princs) ->
            let ps_princs = close_value cfg ps_princs in
            let ps = Ast.AstMap.ps_value_princs ps_princs in
            let ps = List.map begin fun {data=p} -> p end ps in
            `Sec_block ps

        | Pl_ps ({data=Par}, ps_princs)->
            let ps_princs = close_value cfg ps_princs in
            let ps = Ast.AstMap.ps_value_princ_set ps_princs in
            if ( Ps.mem p ps ) then `Delegate else `Skip

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let stk_push': cfg -> expr_nd -> (expr_nd -> env * expr_nd) -> cfg =
    fun cfg expr cont ->
      let frm = { frm_place = cfg.place ;
                  frm_cont  = cont ; } in
      { cfg with
          expr = expr ;
          stk  = frm :: cfg.stk }

  let stk_push : cfg -> expr_nd -> (expr_nd -> expr_nd) -> cfg =
    fun cfg expr cont ->
      stk_push' cfg expr (fun e -> cfg.env, cont e)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let pp_cfg cfg =
    if ! Global.debug_store then begin
    (*if true then begin *)
      print_string "STO= {" ;
      ignore begin
        Store.fold begin fun l array is_first ->
          if not is_first then
            print_string "\n\t"
          else () ;
          Printf.printf "%%%d: [" l ;
          for i = 0 to (Array.length array - 1) do
            if i <> 0 then print_string ", " else () ;
            Ast.Pretty.pp_value_nd (Array.get array i);
          done ;
          Printf.printf "]" ;
          Printf.printf "\n" ;
          false
        end cfg.store true
      end
      ;
      print_string "}\n"
    end
    else
      ()
    ;
    print_string "ENV= " ;
    Ast.Pretty.pp_env "\n      " cfg.env ;
    print_string "\n" ;
    print_string "AST= " ;
    Ast.Pretty.pp_expr_nd cfg.expr ;
    print_string "\n" ;
    print_string (Global.Prov.sprint_prov ">>>> " cfg.expr.prov) ;
    ()

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(*  let initial : (Raw.var_nd * Raw.value_nd) list -> expr_nd -> cfg = fun secrets e ->  *)
  let initial : ?env:env -> expr_nd -> cfg = fun ?env:(env=Env.empty) e ->
    let our_mode = {
      data=Par;
      info=dummyinfo;
      prov=Global.Prov.Synth}
    in
    let value_of_princ p =
      { data=V_princ
          {data=p; info=dummyinfo; prov=Global.Prov.Synth} ;
        info=dummyinfo;
        prov=Global.Prov.Synth }
    in
    let values_of_princs ps =
      List.map value_of_princ ps
    in
    let our_place, princname =
      let princs_value, princname =
        match Flags.variant with
          | `Multi_party_simulation princs ->
              { data=(V_ps_lit (values_of_princs princs));
                info=dummyinfo;
                prov=Global.Prov.Synth }, ""
          | `Single_party_protocol princ ->
              { data=(V_ps_lit [value_of_princ princ]);
                info=dummyinfo;
                prov=Global.Prov.Synth }, princ
      in
      Pl_ps (our_mode, princs_value), princname
    in

    (*
     * add binding for __me__ to the environment.
     * in Multi-party mode, __me__ is bound to "".
     *)
    let mevarnd = { prov = Global.Prov.Synth; data = Var(Ast._mevar); info = T_string } in
    let mevalnd = { prov = Global.Prov.Synth; data = V_string(princname); info = T_string } in
    let env' = Env.add mevarnd.data { var = mevarnd; place = { data=our_place;
							       info=dummyinfo;
							       prov=Global.Prov.Synth
							     };
				      value = mevalnd } env in
    {
      env    = env'  ; (* default is _mevar binding -- earlier was empty env *)
      store  = Store.empty ;
      nxtloc = 0 ;
      expr   = e ;
      place  = { data=our_place;
                 info=dummyinfo;
                 prov=Global.Prov.Synth };
      stk    = [] ;
    }

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let is_halted : cfg -> bool = fun cfg ->
    match cfg.expr.data, cfg.stk with
      | E_value _, [] -> true
      | _             -> false

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)


      (*
       * open this value and return a environment closing the result
       *)
  let openv (vnd:value_nd) (cfg:cfg) :(value_nd * env) =
    let closv = AstMap.close_value cfg.env vnd in
    let cmap = AstMap.copy_value_mapper closv in
    let emp = Env.empty in

    let combine envl =
      List.fold_left
	(fun m env -> Env.fold (fun k v m' -> Env.add k v m') env m) Env.empty envl
    in

    let open_value_mapper value_nd =
      let astnd data = { value_nd with data = data } in {
	v_arrloc   = begin fun l -> cmap.v_arrloc l, emp end;
	v_princ    = begin fun p -> cmap.v_princ p, emp end;
	v_var      = begin fun v -> cmap.v_var v, emp end;
	v_bool     = begin fun b -> cmap.v_bool b, emp end;
	v_nat      = begin fun n -> cmap.v_nat n, emp end;
        v_string   = begin fun s -> cmap.v_string s, emp end;
        v_proc     = begin fun p -> cmap.v_proc p, emp end;
	v_unit     = begin fun x -> cmap.v_unit x, emp end;
	v_inj      = begin fun c vs -> cmap.v_inj c (fst (List.split vs)), emp end;
	v_row      = begin fun l -> let (flds, l') = List.split l in
				    let (vals, envs) = List.split l' in
				    cmap.v_row (List.combine flds vals),
				    combine envs end;
	v_emp      = begin fun x -> cmap.v_emp x, emp end;
	v_ps_lit   = begin fun vs -> cmap.v_ps_lit (fst (List.split vs)), emp end;
	v_ps_union = begin fun v1 v2 -> cmap.v_ps_union (fst v1) (fst v2), emp end;
	v_paren    = begin fun v -> cmap.v_paren (fst v), snd v end;
	v_clos     = begin fun c -> cmap.v_clos c, emp end;
	v_wires    = begin fun pvenvs -> let pvs = List.map (fun (p, (v, env)) -> (p, v)) pvenvs in
					 let varnd = fresh_var () in
					 astnd (V_var(varnd)),
					 Env.add varnd.data { var = varnd;
							      place = cfg.place;
							      value = astnd(V_wires(pvs)) } Env.empty
	end;

	v_sh      = begin fun l -> let varnd = fresh_var () in
				   astnd (V_var(varnd)),
				   Env.add varnd.data { var = varnd;
							place = cfg.place;
							value = value_nd } Env.empty
	end;


      } in
    AstMap.map_value open_value_mapper vnd

  (* open the environment, by opening each value in it *)
  let openenv (env:env) (cfg:cfg) =
    Env.fold (fun k bnd accum ->
      let {value = v} = bnd in
      let (openv, env') = openv v cfg in
      Env.fold
	(fun k' bnd' accum' -> Env.add k' bnd' accum')
	env'
	(Env.add k { bnd with value = openv } accum)) env Env.empty

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)


  let step : cfg -> cfg = fun cfg ->

    let astgen data info =
      { prov = Global.Prov.Stepped cfg.expr.prov ;
        data = data ;
        info = info ; }
    in
    let astgen' data = astgen data cfg.expr.info in
    let g  x  = astgen' x in
    let ga x = astgen x dummyinfo in
    let gv x = astgen x dummyinfo in
    let gv' t x = astgen x t in

    let rec expand_wire_macros e _ : (unit, expr_nd) expr_folders' =
      { (Ast.expr_copy_folders e ()) with
          e_waps =
          begin fun (v, e1, l, le) ->
            (), (), fun y1 y2 ->
              let v_closed = close_value cfg v in
              let ps  = Ast.AstMap.ps_value_princs  v_closed in
              let ps' = Ast.AstMap.ps_value_princs' v_closed in
              begin match ps, ps' with
                | [], _ -> raise Impossible
                | (princ :: _), (princ_value_node :: rest) ->
                    (* Evaluation of wapp via dynamic code generation: *)
                    (* (waps ({p} ∪ w′) [v;l])
                       --->
                       let z1 = v[p] in
                       let z2 = l z1 in
		       let z3 = wire p:z2 in
                       let z4 = waps w′ [v;l] in
                       ( z3 ++ z4 )
                    *)
                    let g = astgen' in
		    let g' t e = {(g e) with info=t} in
		    let ret_typ_of_arr t = match t with
		      | T_arr (_,_,_,t) -> t
		      | _ -> raise Impossible
		    in
		    let wt1 = T_wire (princ_value_node, ret_typ_of_arr l.info) in
		    let wt2 = T_wire (v_closed, ret_typ_of_arr l.info) in
		    (* rest type *)
		    let wt3 = T_wire (astgen (V_ps_lit(rest)) (T_ps(astgen R_true T_unknown)), ret_typ_of_arr l.info) in

                    let princ_expr = g (E_value (princ_value_node)) in
                    let z1, p1 = fresh_var_pat () in
                    let z2, p2 = fresh_var_pat () in
                    let z3, p3 = fresh_var_pat () in
                    let z4, p4 = fresh_var_pat () in
                    let let1 = fun e -> g (E_let (p1, None, g (E_wproj (e1, princ_expr)), e)) in
                    let let2 = fun e -> g (E_let (p2, None, g (E_app (ga(A_app (ga(A_lam (lam_of_expr y2)), g(E_value (gv(V_var z1))))))), e)) in
		    let let3 = fun e -> g (E_let (p3, None, g' wt1 (E_wire (princ_expr, (g(E_value(gv(V_var z2)))))), e)) in
		    if rest = [] then
                      let1 (let2 (let3 (g' wt1 (E_value(gv' wt1 (V_var z3))))))
		    else
                      let let4 =
			let wapp_rest =
                          fold_expr expand_wire_macros
                            (g (E_waps (gv(V_ps_lit rest), e1, l)))
                            ()
			in
			fun e -> g (E_let (p4, None, wapp_rest, e ))
                      in
                      let wcat = (g' wt2 (E_wcat (g' wt1 (E_value (gv' wt1 (V_var z3))), g' wt3 (E_value (gv' wt3 (V_var z4)))))) in
                      let1 (let2 (let3 (let4 wcat)))

                | _ -> raise (Stuck cfg)
              end
          end
          ;

          e_wfold = begin fun (v, e1, e2, l, _) ->
           (), (), (), fun y1 y2 y3 ->
              let v_closed = close_value cfg v in
              let ps  = Ast.AstMap.ps_value_princs  v_closed in
              let ps' = Ast.AstMap.ps_value_princs' v_closed in
              begin match ps, ps' with
                | [], _ -> e2
                | (princ :: _), (princ_value_node :: rest) ->
                    (* Evaluation of wfold via dynamic code generation: *)
                    (* (wfold {p}∪w′ [v1;v2;lam])
                                   e1 = v1 = wires
                                   e2 = v2 = accum
                       --->
                       let z2 = v1[ p ] in
                       let z3 = ((lam v2) p) z2 in
                       wfold w′ [v1;z3;lam]
                    *)
                    let g    = astgen' in
                    let princ_expr = g (E_value (princ_value_node)) in
                    let z2, p2 = fresh_var_pat () in
                    let z3, p3 = fresh_var_pat () in
                    let let2 = fun e -> g (E_let (p2, None, g (E_wproj (e1, princ_expr)), e)) in
                    let let3 = fun e -> g (E_let (p3, None, g (E_app (ga(A_app (ga(A_app (ga(A_app (ga(A_lam l), e2 )), g(E_value (gv(V_princ princ))) )), g(E_value (gv(V_var z2))))))), e)) in
                    let wfold =
                      fold_expr expand_wire_macros
                        (g (E_wfold (gv(V_ps_lit rest), e1, g(E_value (gv (V_var z3))), l)))
                        ()
                    in
                    let2 (let3 wfold)

                | _ -> raise (Stuck cfg)
              end
          end ;
      }
    in

    let rec inline_func_apps cfg e env : (env, expr_nd) expr_folders' =
      let g x = {e with data=x} in
      let envs xs env = List.map (fun _ -> env) xs in
      let rec closf env (value_nd, v) =
        try
          (*let env_value = Env.find v.data cfg.env in*)
          let env_value = Env.find v.data env in
          match env_value.value.data with
            | V_wires _ -> value_nd (* keep as a variable *)
	    | V_sh(_) -> value_nd (* shares also kept as variable *)
            | non_wire  ->
                Ast.AstMap.close_value_sel (closf env) env_value.value
        with
            (* Variable is not bound in the environment. *)
          | Not_found -> value_nd
      in
      {
        (* Wapp and Wfold are gone: *)
        e_wapp   = begin fun _ -> raise Impossible end ;
        e_waps   = begin fun _ -> raise Impossible end ;
        e_wfold  = begin fun _ -> raise Impossible end ;

        (* Congruence cases: *)
        e_value  = begin fun v               -> g (E_value v) end ;
        e_case   = begin fun (_,bs)          -> env, (envs bs env), fun y ys -> g (E_case (y, List.map2 (fun (c,ps,_) y -> (c,ps,y)) bs ys)) end ;
        e_cond   = begin fun (e1,e2,e3)      -> env, env, env, fun y1 y2 y3 -> g (E_cond (y1, y2, y3)) end ;
        e_proj   = begin fun (fld, e)        -> env, fun y -> g(E_proj (fld, y)) end ;
        e_natop  = begin fun (natop, e1, e2) -> env, env, fun y1 y2 -> g(E_natop(natop, y1, y2)) end ;
        e_wire   = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_wire(y1, y2)) end ;
        e_wcat   = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_wcat(y1, y2)) end ;
        e_wcopy  = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_wcopy(y1, y2)) end ;
        e_wproj  = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_wproj(y1, y2)) end ;
        e_array  = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_array(y1, y2)) end ;
        e_select = begin fun (e1, e2)        -> env, env, fun y1 y2 -> g(E_select(y1, y2)) end ;
        e_update = begin fun (e1, e2, e3)    -> env, env, env, fun y1 y2 y3 -> g(E_update(y1, y2, y3)) end ;
        e_paren  = begin fun e               -> env, fun y -> g(E_paren y) end ;
        e_sh     = begin fun e               -> env, fun y -> g(E_sh y) end ;
        e_comb   = begin fun e               -> env, fun y -> g(E_comb y) end ;
        e_avar   = begin fun v               -> e (* e is IGNORED: see e_app. *) end ;
        e_lam    = begin fun (l, e)          -> env, fun y -> e (* e is IGNORED: see e_app. *) end ;
        e_print  = begin fun e               -> env, fun y -> g(E_print y) end ;
        e_sysop  = begin fun (vr, tyop, l)   -> g (E_sysop(vr, tyop, l)) end ;

        e_cast   = begin fun (e, t)          -> env, fun y -> g(E_cast (y, t)) end ;
        e_subset = begin fun (e1,e2,e3)      -> env, env, env, fun y1 y2 y3 -> g (E_subset (y1, y2, y3)) end ;

        (* XFORM case: Application chain ~~> let-binding chain. *)
        e_app = begin fun (apphd, e2) -> env, env, fun _ e2' ->
          let rec decompose_app apphd args : (lam_nd * expr_nd list) =
            match apphd.data with
              | A_paren a    -> decompose_app a args
              | A_app (a, e) -> decompose_app a (e :: args)
              | A_lam l      -> (l, args)
              | A_var v      ->
                  let env_v = Env.find v.data env in
                  begin match env_v.value.data with
                    | V_clos clos ->
                        let lame = Ast.expr_close_sel
                          (closf clos.clos_env) (expr_of_lam clos.clos_lam)
                        in
                        (lam_of_expr lame, args)
                    | _  -> raise Impossible
                  end
          in
          let rec decompose_lam nargs pats expr : (pat_nd list * expr_nd) =
            if nargs = 0 then
              (pats, expr)
            else
              begin match (lam_of_expr expr).data with
                | L_lam(pat, body) -> decompose_lam (nargs-1) (pat::pats) body
                | _                -> raise Impossible
              end
          in
          let rec compose_lets pats args body =
            match pats, args with
              | [], [] -> body
              | pat::pats, arg::args ->
                  let res = compose_lets pats args body in
                  g (E_let (pat, None, arg, res))
              | _, _ -> raise Impossible
          in
          let lam, args  = decompose_app apphd [e2'] in
          let pats, body = decompose_lam (List.length args) [] (expr_of_lam lam) in
          let body       = fold_expr (inline_func_apps cfg) body env in
          compose_lets pats (List.rev args) body
        end ;

        (* XFORM case: Let. *)
        e_let = begin fun (p,plo,e1,e2) ->
          match e1.data with (* Case analysis: First subexpression *)

            (* Var case: *)
            | ( E_value{data=(V_var v)}
              | E_app  {data=(A_var v)} ) -> begin match e1.info with
                | T_arr _ -> (* Case: variable binds a function. *) begin
                    let vbnd = Env.find v.data env in
                    let env' = pat_match {cfg with env=env} p vbnd.value in
                    env, env', fun _ y2 -> y2 (* Let goes away. *)
                  end

                | _ -> (* Case: Variable is not a function binding. *)
                    env, env, fun y1 y2 -> g (E_let (p,plo,y1,y2))
              end

            (* Lam case: *)
            | E_app{data=(A_lam lam)} -> begin
                let clos = {clos_env=env; clos_lam=lam} in
                let env' = pat_match {cfg with env=env} p (gv (V_clos clos)) in
                env, env', fun _ y2 -> y2 (* Let goes away. *)
              end

            (* Not a function-binding *)
            | _ -> env, env, fun y1 y2 -> g (E_let (p,plo,y1,y2))
        end ;
      }
    in

    let parse_value : string -> string -> value_nd =
      let module Lexer = Wylexer in
      let module Parser = Wyparser in
      fun filename string ->
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
        let ast : value_nd =
          try Parser.value Lexer.token lexbuf
          with
            | exn -> begin
                let curr = lexbuf.Lexing.lex_curr_p in
                let line = curr.Lexing.pos_lnum in
                let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                let tok = Lexing.lexeme lexbuf in
                raise (Parse_error (exn, (line, cnum, tok)))
              end
        in
        ast
    in

    let exec_sysop (sys:string) (typ_op:typ_nd option) (vals:value_nd list) :value_nd =
      let module Proc = Sysproc.Proc in
      match sys with
        | "rand" ->
	    begin match typ_op, vals with
	      | None, [{data=V_nat(n)}] ->
	          let r = Random.int n in
	          astgen (V_nat(r)) T_nat
	      | _ -> raise (Stuck cfg)
            end

        | "run" ->
	    begin match typ_op, vals with
	      | None, [{data=V_string(s)}] ->
                  astgen (V_proc (Proc.run s)) T_proc
	      | _ -> raise (Stuck cfg)
            end

        | "send" ->
            begin match typ_op, vals with
              | None, {data=V_proc(p)} :: values ->
                  let ss = List.map Ast.Pretty.string_of_value_nd values in
                  let s  = match ss with
                    | [] -> ""
                    | one :: more ->
                        List.fold_left (fun a b -> a ^ " " ^ b) one more
                  in
                  Proc.send p s ;
                  astgen (V_unit) T_unit

	      | _ -> raise (Stuck cfg)
            end

        | "recv" ->
            begin match typ_op, vals with
	      | Some { data = T_sh(v, t) }, [{data=V_proc(p)}] ->
		(* TODO: type checking shares ????*)
		let str = Proc.recv p in
		let vnd = astgen (V_sh(Bytes.of_string str)) (T_sh(v, t)) in
		vnd
              | Some typ, [{data=V_proc(p)}] ->
                  let str = Proc.recv p in
                  let vnd = parse_value ("sysop recv") str in
                  (* TODO: Type-check vnd and compare its type against typ *)
                  (* TODO: Fail if types do not agree. *)
                  vnd

	      | _ -> raise (Stuck cfg)
            end

        | "ignore" ->
            begin match typ_op, vals with
              | None, [{data=V_proc(p)}] ->
                let _ = Proc.recv p in
                  (*let vnd = parse_value ("sysop recv") str in*)
                  (* TODO: Type-check vnd and compare its type against typ *)
                  (* TODO: Fail if types do not agree. *)
                astgen V_unit T_unit

	      | _ -> raise (Stuck cfg)
            end

        | "stop" ->
            begin match typ_op, vals with
              | None, [{data=V_proc(p)}] ->
                  Proc.stop p ;
                  astgen (V_unit) T_unit

	      | _ -> raise (Stuck cfg)
            end

	| "strcat" ->
	  let strcat s1 s2 =
	    let delim = String.make 1 '"' in
	    parse_value ("sysop strcat") (delim ^ s1 ^ s2 ^ delim)
	  in
	  begin match typ_op, vals with
	    | None, [{data=V_string(s1)}; {data=V_string(s2)}] ->
	      strcat s1 s2
	    | None, [{data=V_nat(s1)}; {data=V_string(s2)}] ->
	      strcat (string_of_int s1) s2
	    | None, [{data=V_string(s1)}; {data=V_nat(s2)}] ->
	      strcat s1 (string_of_int s2)
	    | None, [{data=V_nat(s1)}; {data=V_nat(s2)}] ->
	      strcat (string_of_int s1) (string_of_int s2)
	    | _ -> raise (Stuck cfg)
	  end

	| "strsub" ->
	  begin match typ_op, vals with
	    | None, [{data=V_string(s)}; {data=V_nat(i)}; {data=V_nat(j)}] ->
	      let delim = String.make 1 '"' in
	      parse_value ("sysop strcat") (delim ^ (String.sub s i j) ^ delim)
	    | _ -> raise (Stuck cfg)
	  end

        | _ ->
	    raise (Stuck cfg)
    in

    match cfg.expr.data with

      (* Lambda case; this stack frame is complete. *)
      | E_app({data=A_lam lam}) -> begin
          let closed_v = gv(V_clos({clos_env=cfg.env;
                                    clos_lam=lam;})) in
          match cfg.stk with
            | [] -> cfg (* halt state; cfg steps to itself. *)
            | frm :: stk' -> (* ow: pop the stack *)
                let env, expr = frm.frm_cont (astgen' (E_value closed_v)) in
                { cfg with
                    place = frm.frm_place ;
                    env   = env ;
                    expr  = expr ;
                    stk   = stk' ; }
        end

      (* (Non-lambda) Value case; this stack frame is complete. *)
      | E_value v -> begin
          let closed_v = close_value cfg v in
          match cfg.stk with
            | [] -> cfg (* halt state; cfg steps to itself. *)
            | frm :: stk' -> (* ow: pop the stack *)
                let env, expr = frm.frm_cont (astgen' (E_value closed_v)) in
                { cfg with
                    place = frm.frm_place ;
                    env   = env ;
                    expr  = expr ;
                    stk   = stk' ; }
        end

      (* Scrutinee is a value *)
      | E_case(e, bs) when expr_is_value cfg e.data ->
          begin match close_value_of_expr cfg e.data with
            | Some ({data=V_inj (c, vs)}) ->
                let rec loop = function
                  | (c', ps, e) :: xs when cons_eq c c' ->
                      { cfg with
                          env = pat_matchs cfg ps vs ;
                          expr = e }
                  | _ :: xs -> loop xs
                  | [] -> raise (Stuck cfg)
                in loop bs
          | _ -> raise (Stuck cfg)
          end
      (* Scrutinee is not a value; push the stack to continue *)
      | E_case(e, bs) ->
          stk_push cfg e (fun e -> astgen' (E_case(e, bs)))

      (* Cond: First sub-expression is a value. *)
      | E_cond (e1, e2, e3) when (expr_is_value cfg e1.data) ->
          begin match close_value_of_expr cfg e1.data with
            | Some ({data=V_bool b}) ->
                if b then { cfg with expr = e2 }
                else { cfg with expr = e3 }
            | _ -> raise (Stuck cfg)
          end
      (* Cond: First sub-expression is not a value. *)
      | E_cond (e1, e2, e3) ->
          stk_push cfg e1 (fun e -> astgen' (E_cond (e, e2, e3)))

      (* Let: Perform delegation. *)
      | E_let(pat,Some pl,e1,e2) ->
          begin match Flags.variant with

            (* Simulation -- sanity check: check that delegation is legal. *)
            | `Multi_party_simulation _ ->
                if not (can_delegate cfg.place pl) then
                  raise (Stuck cfg)
                else
                  let cfg =
                    stk_push cfg e1 (fun e -> astgen' (E_let(pat, Some pl, e, e2))) in
                  { cfg with place = pl }

            (* Single party -- check whether to delegate, skip or enter secure block. *)
            | `Single_party_protocol princ ->
                begin match check_place_membership cfg princ pl with
                  | `Delegate ->
                      let cfg =
                        stk_push cfg e1 (fun e -> astgen' (E_let(pat, None, e, e2))) in
                      { cfg with place = pl }

                  | `Skip -> { cfg with expr = e2 }

                  | `Sec_block princs ->
		    (*print_string "environment before opening:"; print_newline ();
		    Pretty.pp_env "\n" cfg.env;
		    print_newline ();*)

		    let openenv = openenv cfg.env cfg in

		    (*print_string "environment after opening:"; print_newline ();
		    Pretty.pp_env "\n" openenv;
		    print_newline ();*)

                    let rec closef (value_nd, v) =
                      try
                        (*let env_value = Env.find v.data cfg.env in*)
                        let env_value = Env.find v.data openenv in
                        match env_value.value.data with
                          | V_wires _ -> value_nd (* keep as a variable *)
			  | V_sh(_) -> value_nd (* shares also kept as variable *)
                          | non_wire  ->
                            Ast.AstMap.close_value_sel closef env_value.value
                        with
                          (* Variable is not bound in the environment. *)
                          | Not_found -> value_nd
                      in
                      (* Expand all of the wire macros into sequenced let bindings. *)
                      (* Note: assumes that the program is in ANF
                         form; otherwise, this expansion will duplicate
                         code. *)

                      (*print_string "before expaning wire macros: "; Pretty.pp_expr_nd e1;
                      print_newline ();*)
                      let e1 = Ast.fold_expr expand_wire_macros e1 () in
		      (*print_string "expanded wire macros: "; Pretty.pp_expr_nd e1;
                      print_newline (); print_newline ();*)

                      let e1 = Ast.fold_expr (inline_func_apps cfg) e1 cfg.env in
                      (*print_string "inlined functions: "; Pretty.pp_expr_nd e1;
                      print_newline (); print_newline ();*)

                      (* close the expression selectively -- do not substitute wire values *)
                      let e1 = Ast.expr_close_sel closef e1 in
		      (*print_string "closed the expression: "; Pretty.pp_expr_nd e1;
                      print_newline ();  print_newline ();*)

                      (* find all the remaining wire variables: *)
                      let wire_vars = Ast.Varvals.elements (Ast.expr_free_var_set e1) in
                      let wire_vars = List.map begin
                        fun ({info=typ}, {data=Var v}) ->
                          (*print_string ("free variable `" ^ v ^ "' has type `");
                          Pretty.pp_typ typ ;
                          print_string "\n" ;*)
                          (v, typ)
                      end wire_vars
                      in
                      (* Gencircuit.runsecblk :
                           string -> string list ->
                           Typd.env -> (string * Raw.typ) list ->
                           Typd.expr_nd -> Typd.value_nd
                      *)
                    (*print_string "SEC-BLOCK BEGIN:" ; print_newline ();
                    (*pp_cfg cfg ;*)
                    print_newline () ;*)
                    (*let v = Gencircuit.runsecblk princ princs cfg.env wire_vars e1 in*)
                    let v = Gencircuit.runsecblk princ princs openenv wire_vars e1 in

                    (*print_string "SEC-BLOCK END; " ;
                    print_string "value=" ;
                    Ast.Pretty.pp_value_nd v ;
                    print_newline ();*)
                    { cfg with expr = astgen' (E_let(pat, None, astgen' (E_value v), e2)) }
                end
          end

      (* Let: First sub-expression is a value. *)
      | E_let(p,plo,e1,e2) when expr_is_value cfg e1.data ->
          (* compute the place of the binding:
             The mode is that of the let expression;
             the party set is that of the first sub-expression. *)
          let bnd_place = match plo with
            | None                 -> cfg.place
            | Some ({data=Pl_top}) -> raise (Stuck cfg)
            | Some ({data=Pl_ps (_, e1_princs)} as pl) ->
                let amb_mode = match cfg.place.data with
                  | Pl_top        -> raise (Stuck cfg)
                  | Pl_ps( m, _ ) -> m
                in
                {pl with data=Pl_ps(amb_mode,e1_princs)}
          in
          begin match close_value_of_expr cfg e1.data with
            | None -> raise Impossible
            | Some v ->
                { cfg with
                    env = pat_match {cfg with place=bnd_place} p v;
                    expr = e2 }
          end
      (* Let: First sub-expression e1 is not a value;
              no inter-place delegation ~~> just run e1. *)
      | E_let(p,None,e1,e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_let(p, None, e, e2)))

      (* Eliminate parenthesis. *)
      | E_app({data=A_paren a}) ->
          { cfg with expr = { cfg.expr with data=E_app a } }
      (* Look up closure. *)
      | E_app({data=A_var v} as a) ->
          let v_bnd = Env.find v.data cfg.env in
          begin match v_bnd.value.data with
            | V_clos c -> { cfg with
                              env = c.clos_env;
                              expr={cfg.expr with
                                      data=E_app({a with data=A_lam(c.clos_lam)})} }
            | _        -> raise (Stuck cfg)
          end
      (* Lambda-elimation/Application: beta-reduction case. *)
      | E_app({data=A_app({data=A_lam lam}, e)}) when expr_is_closed_value cfg e.data ->
          (* Perform beta reduction on a lambda term by applying it to a value. *)
          (* Note: this only makes sense when the argument value is *closed*. *)
          let beta_reduce env lam value =
            match lam.data with
              | L_lam (pat, e) ->
                  let env = pat_match cfg pat value in
                  ( env, e )

              | L_fix (fix_var, _, pat, e) ->
                  let fix_bnd = {
                    value = gv(V_clos({clos_env = env;
                                       clos_lam = lam;}));
                    place = cfg.place ;
                    var   = fix_var ; }
                  in
                  let env = Env.add fix_var.data fix_bnd env in
                  let env = pat_match {cfg with env=env} pat value in
                  ( env, e )
          in
          begin match close_value_of_expr cfg e.data with
            | None -> raise Impossible
            | Some v ->
                let env, expr = beta_reduce cfg.env lam v in
                { cfg with env = env; expr = expr }
          end
      (* Case: Argument is not reduced;

         - Reduce argument first, under current environment.
            (Push stack and reduce argument).
      *)
      | E_app({data=A_app(a1, e_arg)} as a2)
          when not (expr_is_closed_value cfg e_arg.data) ->
          stk_push cfg e_arg begin fun e -> astgen'
            (E_app({a2 with data=A_app(a1, e)}))
          end

      (* Case: Argument *is* fully reduced under current environment,
               but lambda *is not*.

         - Push the stack and reduce the head of the application to a
         lambda, then reconstruct the application term using the head
         closure's environment.
      *)
      | E_app({data=A_app(a, e_arg)}) ->
          stk_push' cfg {cfg.expr with data=E_app(a)}
            begin
              fun e ->
                (* Extract the lambda term from the (reduced) head of an application term. *)
                let rec clos_of_expr expr : clos =
                  match expr.data with
                    | E_value({data=V_clos c}) -> c
                    | _ -> raise (Invalid_argument "lam_of_expr")
                in
                let c = clos_of_expr e in
                (* Note: Reusme by using the outer environment *)
                c.clos_env,
                { cfg.expr with
                    data=E_app(ga(A_app(ga(A_lam c.clos_lam), e_arg))) }
            end

      | E_proj (f, e) when expr_is_value cfg e.data ->
          begin match close_value_of_expr cfg e.data with
            | None -> raise Impossible
            | Some {data=V_row row} ->
                let rec loop_fields xs = begin match xs with
                  | [] -> raise (Stuck cfg)
                  | (fld,v) :: flds ->
                      if fld.data = f.data then v
                      else loop_fields flds
                end
                in
                { cfg with expr = astgen' (E_value (loop_fields row)) }
                  (* Some non-row value means that the projection is stuck. *)
            | Some _ -> raise (Stuck cfg)
          end
      | E_proj (f, e) ->
          stk_push cfg e (fun e -> astgen' (E_proj(f,e)))

      (* Natop: Both sub-expressions are values *)
      | E_natop (o, e1, e2)
          when ( expr_is_value cfg e1.data && expr_is_value cfg e2.data ) ->
          begin match
            close_value_of_expr cfg e1.data,
            close_value_of_expr cfg e2.data
          with
            | Some ({data=V_nat n1; info=i1}),
              Some ({data=V_nat n2; info=i2}) ->
              begin match o.data with
                | Natop_plus ->
                  let v = astgen (V_nat (n1 + n2)) dummyinfo in
                  { cfg with expr = astgen' (E_value v) }
                | Natop_sub ->
		  if n1 >= n2 then
                    let v = astgen (V_nat (n1 - n2)) dummyinfo in
                    { cfg with expr = astgen' (E_value v) }
		  else
		    raise (Stuck cfg)
                | Natop_gt ->
                  let v = astgen (V_bool (n1 > n2)) dummyinfo in
                  { cfg with expr = astgen' (E_value v) }
                | Natop_equals ->
                  let v = astgen (V_bool (n1 = n2)) dummyinfo in
                  { cfg with expr = astgen' (E_value v) }
              end

            | _ -> raise (Stuck cfg)
          end
      (* Natop: First sub-expression is a value. *)
      | E_natop (o, e1, e2) when expr_is_value cfg e1.data ->
          stk_push cfg e2 (fun e -> astgen' (E_natop(o,e1,e)))
      (* Natop: First sub-expression is not a value. *)
      | E_natop (o, e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_natop(o,e,e2)))

      (* Wire: Both sub-expressions are values *)
      | E_wire (e1, e2)
          when (expr_is_value cfg e1.data && expr_is_value cfg e2.data) ->
          begin match close_value_of_expr cfg e1.data, close_value_of_expr cfg e2.data with
            | Some ps_value, Some wire_value ->
                begin try
                  (* First sub-expression consists of a set of principal constants. *)
                  let ps = Ast.AstMap.ps_value_princs ps_value in
                  begin match Flags.variant with
                    | `Multi_party_simulation _ ->
                        let pvs = List.map (fun princ -> princ, wire_value) ps in
                        { cfg with expr = astgen' (E_value (astgen (V_wires pvs) wire_value.info)) }

                    | `Single_party_protocol princ ->
                        let pset = Ast.AstMap.ps_value_princ_set ps_value in
                        if Ps.mem princ pset then
                          let princ_nd = {data=princ; info=dummyinfo; prov=Global.Prov.Synth} in
                          { cfg with expr = astgen' (E_value (astgen (V_wires [(princ_nd, wire_value)]) cfg.expr.info)) }
                        else
                          { cfg with expr = astgen' (E_value (astgen (V_wires [(*empty*)]) cfg.expr.info)) }
                  end
                with
                  | Ast.Missing_case _ -> raise (Stuck cfg)
                end
            | _, _ -> raise Impossible
          end
      (* Wire: First sub-expression is a value. *)
      | E_wire (e1, e2) when (expr_is_value cfg e1.data) ->
          stk_push cfg e2 (fun e -> astgen' (E_wire(e1,e)))
      (* Wire: First sub-expression is not a value. *)
      | E_wire (e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_wire(e,e2)))

      (* Wcat *)
      | E_wcat (e1, e2)
          when ( expr_is_value cfg e1.data && expr_is_value cfg e2.data ) ->
          (* XXX -- concatenating two wire bundles that are not
             disjoint should be an error; the type system should
             prevent this from occuring. The operational semantics
             here should check and get stuck if disjointness
             fails. TODO! *)
          begin match close_value_of_expr cfg e1.data, close_value_of_expr cfg e2.data with
            | Some ({data=V_emp}), Some ({data=V_wires pvs2}) ->
                { cfg with expr = astgen' (E_value (astgen (V_wires pvs2) cfg.expr.info)) }

            | Some ({data=V_wires pvs1}), Some ({data=V_emp}) ->
                { cfg with expr = astgen' (E_value (astgen (V_wires pvs1) cfg.expr.info)) }

            | Some ({data=V_wires pvs1}), Some ({data=V_wires pvs2}) ->
                { cfg with expr = astgen' (E_value (astgen (V_wires (pvs1 @ pvs2)) cfg.expr.info)) }

            | Some v1, Some v2 ->
                print_string "Unhandled wire concat case;\n\t v1=" ;
                Pretty.pp_value_nd v1 ;
                print_string "\n\t v2=" ;
                Pretty.pp_value_nd v2 ;
                print_string "\n" ;
                raise (Stuck cfg)
            | _, _ -> raise (Stuck cfg)
          end

      | E_wcat (e1, e2) when expr_is_value cfg e1.data ->
          stk_push cfg e2 (fun e -> astgen' (E_wcat(e1,e)))
      | E_wcat (e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_wcat(e,e2)))

      | E_wcopy (e1, e2)  -> { cfg with expr = e2 }

      (* Wproj *)
      | E_wproj (e1, e2)
          when ( expr_is_value cfg e1.data && expr_is_value cfg e2.data ) ->
          begin match close_value_of_expr cfg e1.data, close_value_of_expr cfg e2.data with
            | Some ({data=V_wires pvs}), Some ({data=V_princ princ}) ->
                let value_nd =
                  let rec loop_princs pvs =
                    match pvs with
                      | []                -> raise (Stuck cfg)
                      | (qrinc, v) :: pvs' when qrinc.data = princ.data -> v
                      | _          :: pvs' -> loop_princs pvs'
                  in loop_princs pvs
                in
                { cfg with expr = astgen' (E_value value_nd) }
            | _, _ -> raise (Stuck cfg)
          end
      | E_wproj (e1, e2) when expr_is_value cfg e1.data ->
          stk_push cfg e2 (fun e -> astgen' (E_wproj(e1,e)))
      | E_wproj (e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_wproj(e,e2)))

      (* Wfold *)
      | E_wfold (v, e1, e2, l)
          when ( expr_is_value cfg e1.data && expr_is_value cfg e2.data ) ->
          let ps  = Ast.AstMap.ps_value_princs  v in
          let ps' = Ast.AstMap.ps_value_princs' v in
          begin match ps, ps' with
            | [], _ -> { cfg with expr = e2 }
            | (princ :: _), (princ_value_node :: rest) ->
                (* Evaluation of wfold via dynamic code generation: *)
                (* (wfold {p}∪w′ [v1;v2;lam])
                   --->
                     let z1 = lam v2 in
                     let z2 = v1[ p ] in
                     let z3 = z1 z2 in
                     wfold w′ [v1;z3;lam]
                *)
                let g    = astgen' in
                let princ_expr = g (E_value (princ_value_node)) in
                let z1, p1 = fresh_var_pat () in
                let z2, p2 = fresh_var_pat () in
                let z3, p3 = fresh_var_pat () in
                let let1 = fun e -> g (E_let (p1, None, g (E_app (ga(A_app (ga(A_lam l), e2 )))), e)) in
                let let2 = fun e -> g (E_let (p2, None, g (E_wproj (e1, princ_expr)), e)) in
                let let3 = fun e -> g (E_let (p3, None, g (E_app (ga(A_app (ga(A_var z1), g(E_value (gv(V_var z2))))))), e)) in
                let wfold_rest = E_wfold (gv(V_ps_lit ps'), e1, g(E_value (gv (V_var z3))), l) in
                { cfg with expr = let1 (let2 (let3 (g wfold_rest))) }

            | _ -> raise (Stuck cfg)
          end
      | E_wfold (v, e1, e2, l) when expr_is_value cfg e1.data ->
          stk_push cfg e2 (fun e -> astgen' (E_wfold(v, e1,e,l)))
      | E_wfold (v, e1, e2, l) ->
          stk_push cfg e1 (fun e -> astgen' (E_wfold(v, e,e2,l)))

      (* Wapp *)
      | E_wapp (v, e1, e2)
          when ( expr_is_value cfg e1.data && expr_is_value cfg e2.data ) ->

	let local_princ =
	  match Flags.variant with
            | `Single_party_protocol princ -> princ
	    | _ -> raise NYI
	in

        let ps_princs = close_value cfg v in
        let ps = Ast.AstMap.ps_value_princ_set ps_princs in
        if not ( Ps.mem local_princ ps ) then
	  { cfg with expr = astgen' (E_value (astgen (V_wires [(*empty*)]) cfg.expr.info)) }
	else

          let v_closed = close_value cfg v in
          let ps  = Ast.AstMap.ps_value_princs  v_closed in
          let ps' = Ast.AstMap.ps_value_princs' v_closed in
          begin match ps, ps' with
            | [], _ -> { cfg with expr = g(E_value (gv V_emp)) }
            | (princ :: _), (princ_value_node :: rest) ->
                (* Evaluation of wapp via dynamic code generation: *)
                (* Evaluation of wapp via dynamic code generation: *)
                (* (wapp ({p} âª wâ²) [v1;v2])
                   --->
                   let z1 =({p})= v1[p] in
                   let z2 =({p})= v2[p] in
                   let z3 =({p})= z2 z1 in
                   let z4 = wapp wâ² [v1;v2] in
                   ( ( wire p z3 ) ++ z4 )
                *)
                let g    = astgen' in
                let princ_expr = g (E_value (princ_value_node)) in
                let z1, p1 = fresh_var_pat () in
                let z2, p2 = fresh_var_pat () in
                let z3, p3 = fresh_var_pat () in
                let z4, p4 = fresh_var_pat () in
                let synth = Global.Prov.Synth in
                let pl = {data=(Pl_ps ({data=Par; info=T_unknown; prov=synth}, princ_value_node)); info=T_unknown; prov=synth} in
                let let1 = fun e -> g (E_let (p1, Some pl, g (E_wproj (e1, princ_expr)), e)) in
                let let2 = fun e -> g (E_let (p2, Some pl, g (E_wproj (e2, princ_expr)), e)) in
                let let3 = fun e -> g (E_let (p3, Some pl, g (E_app (ga(A_app (ga(A_var z2), g(E_value (gv(V_var z1))))))), e)) in
                let let4 = fun e -> g (E_let (p4, None, g (E_wapp (gv(V_ps_lit rest), e1, e2)), e )) in
                let wval =
                  if princ.data = local_princ then
                    gv(V_wires [(princ,gv(V_var z3))])(*<<FIX-THIS-TYPE??*)
                  else
                    gv(V_wires [(*empty*)])(*<<FIX-THIS-TYPE??*)
                in
                let wcat = (g (E_wcat (g(E_value wval), g(E_value (gv(V_var z4)))))) in
                { cfg with expr = let1 (let2 (let3 (let4 wcat))) }

            | _ -> raise (Stuck cfg)
          end
      | E_wapp (v, e1, e2) when expr_is_value cfg e1.data ->
          stk_push cfg e2 (fun e -> astgen' (E_wapp(v, e1,e)))
      | E_wapp (v, e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_wapp(v, e,e2)))

      (* Waps *)
      | E_waps (v, e, l) when ( expr_is_value cfg e.data ) -> raise NYI
      | E_waps (v, e, l) ->
          stk_push cfg e (fun e -> astgen' (E_waps(v, e, l)))

      | E_array(e1, e2)
          when (expr_is_value cfg e1.data && expr_is_value cfg e2.data) ->
          begin match
            close_value_of_expr cfg e1.data,
            close_value_of_expr cfg e2.data
          with
            | Some ({data=V_nat n}), Some initial_val ->
                let arrloc = cfg.nxtloc in
                let arrarr = Array.make n initial_val in
                { cfg with
                    nxtloc = arrloc + 1;
                    store  = Store.add arrloc arrarr cfg.store ;
                    expr   = astgen' (E_value (gv (V_arrloc arrloc))) ;
                }

            (* Some other values means that the allocation is stuck. *)
            | _ -> raise (Stuck cfg)
          end
      (* Array -- Congruence rules *)
      | E_array(e1, e2)
          when (expr_is_value cfg e1.data) ->
          stk_push cfg e2 (fun e -> astgen' (E_array(e1,e)))
      | E_array(e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_array(e,e2)))

      (* Select -- Reduction rule. *)
      | E_select(e1, e2)
          when (expr_is_value cfg e1.data && expr_is_value cfg e2.data) ->
          begin match
            close_value_of_expr cfg e1.data,
            close_value_of_expr cfg e2.data
          with
            | Some ({data=V_arrloc l}), Some({data=V_nat i}) ->
                let v = try
                  Array.get (Store.find l cfg.store) i
                with
                  | ( Invalid_argument _ | Not_found ) -> raise (Stuck cfg)
                in
                { cfg with expr = astgen' (E_value v) }

            (* Some other values means that the selection is stuck. *)
            | _ -> raise (Stuck cfg)
          end
      (* Select -- Congruence rules *)
      | E_select(e1, e2)
          when (expr_is_value cfg e1.data) ->
          stk_push cfg e2 (fun e -> astgen' (E_select(e1,e)))
      | E_select(e1, e2) ->
          stk_push cfg e1 (fun e -> astgen' (E_select(e,e2)))

      (* Update -- Reduction rule. *)
      | E_update(e1, e2, e3) when
          (expr_is_value cfg e1.data
           && expr_is_value cfg e2.data
           && expr_is_value cfg e3.data) ->
          begin match
            close_value_of_expr cfg e1.data,
            close_value_of_expr cfg e2.data,
            close_value_of_expr cfg e3.data
          with
            | Some ({data=V_arrloc l}),
              Some ({data=V_nat i}),
              Some new_val ->
                let _ = try
                  Array.set (Store.find l cfg.store) i new_val
                with
                  | ( Invalid_argument _ | Not_found ) -> raise (Stuck cfg)
                in
                { cfg with expr = astgen' (E_value (gv (V_unit))) }

            (* Some other values means that the update is stuck. *)
            | _ -> raise (Stuck cfg)
          end
      (* Update -- Congruence rules *)
      | E_update(e1, e2, e3)
          when (expr_is_value cfg e1.data && expr_is_value cfg e2.data) ->
          stk_push cfg e3 (fun e -> astgen' (E_update(e1,e2,e)))
      | E_update(e1, e2, e3) when (expr_is_value cfg e1.data) ->
          stk_push cfg e2 (fun e -> astgen' (E_update(e1,e,e3)))
      | E_update(e1, e2, e3) ->
          stk_push cfg e1 (fun e -> astgen' (E_update(e,e2,e3)))

      (* paren -- vanishes. *)
      | E_paren e -> { cfg with expr = e }

      (* cast -- vanishes. *)
      | E_cast (e, _) -> { cfg with expr = e }

      (* Combine: In simulation mode, so just treat as a no-op. *)
      | E_comb e when (expr_is_value cfg e.data) -> { cfg with expr = e }
      | E_comb e -> stk_push cfg e (fun e -> astgen' (E_comb(e)))

      (* Share: In simulation mode, so just treat as a no-op. *)
      | E_sh e when (expr_is_value cfg e.data) -> { cfg with expr = e }
      | E_sh e -> stk_push cfg e (fun e -> astgen' (E_sh(e)))

      | E_print e when (expr_is_value cfg e.data) ->
	let vndopt = close_value_of_expr cfg e.data in
	begin
	  match vndopt with
	    | None -> raise (Stuck cfg)
	    | Some(v) ->
              print_string "PRINT: " ;
              Ast.Pretty.pp_value_nd v ;
              print_string "\n" ;
              { cfg with expr = g (E_value (gv(V_unit))) }
	end

      | E_print e -> stk_push cfg e (fun e -> astgen' (E_print(e)))

      | E_sysop(vrnd, typ_op, l) -> begin
	let closed_vals = List.map (fun v -> close_value cfg v) l in
	let Var(sysopname) = vrnd.data in
        let v = exec_sysop sysopname typ_op closed_vals in
        { cfg with expr = astgen' (E_value v) }
      end

      | E_subset (e1, e2, e3) when (expr_is_value cfg e1.data && expr_is_value cfg e2.data && expr_is_value cfg e2.data) ->
	begin
	  match (close_value_of_expr cfg e1.data, close_value_of_expr cfg e2.data, close_value_of_expr cfg e3.data) with
	    | Some(v1), Some({data = V_nat(v2)}), Some({data = V_nat(v3)}) when v2 >= 0 && v3 >= 0 && v2 <= v3 ->
	      let l = ps_value_princs v1 in
	      (* get index v2 to v3 in l *)
	      let rec get_index l n1 n2 curr =
		if n1 = n2 + 1 then
		  []
		else if n1 > curr then match l with
		  | [] -> raise (Stuck cfg)
		  | x::l1 -> get_index l1 n1 n2 (curr + 1)
		else match l with
		  | [] -> raise (Stuck cfg)
		  | x::l1 -> x::(get_index l1 (n1 + 1) n2 (curr + 1))
	      in
	      let l1 = get_index l v2 v3 0 in
	      let l2 = List.map (fun p -> astgen' (V_princ p)) l1 in
              { cfg with expr = astgen' (E_value (astgen' (V_ps_lit(l2)))) }
	    | _ -> raise (Stuck cfg)
	end
      | E_subset (e1, e2, e3) when (expr_is_value cfg e1.data && expr_is_value cfg e2.data) ->
          stk_push cfg e3 (fun e -> astgen' (E_subset (e1, e2, e)))
      | E_subset (e1, e2, e3) when (expr_is_value cfg e1.data) ->
          stk_push cfg e2 (fun e -> astgen' (E_subset (e1, e, e3)))
      | E_subset (e1, e2, e3) ->
          stk_push cfg e1 (fun e -> astgen' (E_subset (e, e2, e3)))


end
