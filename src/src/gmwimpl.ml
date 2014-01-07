open Cktlib

exception GMWImplError of string

type gmwckt = booleanckt list

(*
 * collect gates to which wireid is an input to
 *)
let auxinfid (wireid:int) (bckt:gmwckt) :int list =
  let l = 
    List.fold_left (fun l -> function
      | AND(x, y, z) when y = wireid || z = wireid -> x::l
      | XOR(x, y, z) when y = wireid || z = wireid -> x::l
      | _ -> l
    ) [] bckt
  in
  List.rev_append l []

(*
 * build a map, wireid |-> int list
 * i.e. for each wire, which gates (wires) it forms input to
 *)
let auxinf (bckt:gmwckt) :int list IntMap.t =
  let map = IntMap.empty in

  (* entries for 0 and 1 constants *)
  let map0 = IntMap.add 0 (auxinfid 0 bckt) map in
  let map1 = IntMap.add 1 (auxinfid 1 bckt) map0 in

  List.fold_left (fun m -> function
    | AND(x, _, _) -> IntMap.add x (auxinfid x bckt) m
    | XOR(x, _, _) -> IntMap.add x (auxinfid x bckt) m
    | INPUT(_, r) ->
      let l = rangetolist r in
      List.fold_left (fun m' id -> IntMap.add id (auxinfid id bckt) m') m l
    | SHINPUT(r) ->
      let l = rangetolist r in
      List.fold_left (fun m' id -> IntMap.add id (auxinfid id bckt) m') m l
    | _ -> m
  ) map1 bckt


let auxinfalt (bckt:gmwckt) :int list IntMap.t =
  List.fold_left (fun m -> function
    | AND(x, y, z)
    | XOR(x, y, z) ->
      let ym =
	try
	  IntMap.find y m
	with
	  | Not_found -> []
      in
      let zm =
	try
	  IntMap.find z m
	with
	  | Not_found -> []
      in
      
      IntMap.add y (x::ym) (IntMap.add z (x::zm) m)
    | _ -> m
  ) (IntMap.empty) bckt

(*
 * dump gmw circuit in file
 *)
let dumpgmwckt (princidmap:int StringMap.t) (bckt:gmwckt) (numgates:int) (file:out_channel) :int =
  let ps s = output_string file s in
  let psi i = output_string file (string_of_int i) in
  
  let inps = List.filter (fun e -> match e with
    | INPUT(_) -> true
    | SHINPUT(_) -> true
    | _ -> false
  ) bckt in
  let outs = List.filter (fun e -> match e with
    | OUTPUT(_) -> true
    | SHOUTPUT(_) -> true
    | _ -> false
  ) bckt in
  let numxors = List.fold_left (fun n e -> match e with
    | XOR(_) -> n + 1
    | _ -> n
  ) 0 bckt in
  let numands = List.fold_left (fun n e -> match e with
    | AND(_) -> n + 1
    | _ -> n
  ) 0 bckt in
  
  (*print_string "computing aux info\n"; flush stdout;*)
  let auxinfo = auxinfalt bckt in
  (*print_string "computed aux info\n"; flush stdout;*)

  (*let t1 = Unix.gettimeofday () in*)

  ps "n "; psi (StringMap.cardinal princidmap);
  ps "\n";
  
  let last_inp_id = List.fold_left (fun id -> function
    | INPUT(_, (i, j)) -> if id < j then j else id
    | SHINPUT(i, j) -> if id < j then j else id
    | _ -> raise Not_found
  ) 0 inps in

  ps "d "; psi numgates; ps " ";
  psi (last_inp_id + 1); ps " "; psi numxors;
  ps "\n";

  List.iter (fun e -> match e with
    | INPUT(s, (i, j)) ->
      let id = StringMap.find s princidmap in
      ps "i "; psi id; ps " "; psi i;
      ps " "; psi j; ps "\n"
    | SHINPUT(i, j) ->
      ps "s "; psi i; ps " "; psi j; ps "\n"
    | _ -> raise Not_found
  ) inps;

  List.iter (fun e -> match e with
    | OUTPUT(s, (i, j)) ->
      let id = StringMap.find s princidmap in
      ps "o "; psi id; ps " "; psi i;
      ps " "; psi j; ps "\n"
    | SHOUTPUT(i, j) ->
      ps "t "; psi i; ps " "; psi j; ps "\n"
    | _ -> raise Not_found
  ) outs;
  
  List.iter (fun e -> match e with
    | INPUT(s, _) ->
      let id = StringMap.find s princidmap in
      ps "v "; psi id; ps " 1";    (* 1 bit inputs *)
      ps "\n"
    | _ -> ()
  ) inps;

  ps "g 0 0 -1 -1 ";
  let l0 = if IntMap.mem 0 auxinfo then IntMap.find 0 auxinfo else [] in
  psi (List.length l0);
  List.iter (fun i -> ps " "; psi i) l0;
  ps "\n";

  ps "g 1 0 -1 -1 ";
  let l1 = if IntMap.mem 1 auxinfo then IntMap.find 1 auxinfo else [] in
  psi (List.length l1);
  List.iter (fun i -> ps " "; psi i) l1;
  ps "\n";
  
  (* even inputs / share wires are g *)
  begin
    List.iter (fun e -> match e with
      | AND(x, y, z) ->
	ps "g "; psi x; ps " 1 "; psi y;
	ps " "; psi z; ps " ";
	let l = if IntMap.mem x auxinfo then IntMap.find x auxinfo else [] in
	psi (List.length l);      
	List.iter (fun i -> ps " "; psi i) l;
	ps "\n";

      | XOR(x, y, z) ->
	ps "g "; psi x; ps " 2 "; psi y;
	ps " "; psi z; ps " ";
	let l = if IntMap.mem x auxinfo then IntMap.find x auxinfo else [] in
	psi (List.length l);      
	List.iter (fun i -> ps " "; psi i) l;
	ps "\n";

      | INPUT(_, r) ->
	let l = rangetolist r in
	List.iter (fun id ->
	  ps "g "; psi id; ps " 0 -1 -1 ";
	  let l1 = if IntMap.mem id auxinfo then IntMap.find id auxinfo else [] in
	  psi (List.length l1);
	  List.iter (fun i -> ps " "; psi i) l1;
	  ps "\n"
	) l

      | SHINPUT(r) ->
	let l = rangetolist r in
	List.iter (fun id ->
	  ps "g "; psi id; ps " 0 -1 -1 ";
	  let l1 = if IntMap.mem id auxinfo then IntMap.find id auxinfo else [] in
	  psi (List.length l1);
	  List.iter (fun i -> ps " "; psi i) l1;
	  ps "\n"
	) l	

      | _ -> ()
    ) bckt
  end;

  (*let t2 = Unix.gettimeofday () in  
  print_string ("Time to dump: " ^ (string_of_float(t2 -. t1))); print_newline ();*)
  numands
