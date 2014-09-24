open Ast
open Ast.Library

exception CLibError of string

let dprov = Global.Prov.Root(Lexing.dummy_pos, Lexing.dummy_pos)

let natsize = 12

let unitsize = 0

let boolsize = 1

let rec size t =
  match t with
  | T_unit -> unitsize
  | T_nat -> natsize
  | T_bool -> boolsize
  | T_sum([_, [t1]; _, [t2]]) -> max (size t1.data) (size t2.data) + 1
  | T_row(l) -> List.fold_left (fun s elt -> (size (snd elt).data) + s) 0 l
  | T_wire(vnd, tnd) -> (List.length (is_closedprincs vnd)) * (size tnd.data)
  | T_sh(_, tnd) -> size tnd.data
  | t -> raise (CLibError "Unimplemented type in size")

let sizetl tl = List.fold_left (fun sz t -> sz + size t) 0 tl

module StringMap = Map.Make(String)

module IntMap = Map.Make(struct type t = int let compare = compare end)

let inttobin n =
  let rec helper n =
    if n / 2 = 0 then
      [n]
    else
      (n mod 2)::(helper (n / 2))
  in
  let rec pad l =
    if List.length l > natsize then
      raise (CLibError ("natsize not sufficient to contain:"^(string_of_int n)))
    else if List.length l = natsize then
      l
    else
      pad (l @ [0])
  in
  pad (helper n)

(* lsb to msb *)
let bintoint l =
  let l' = List.rev_append l [] in
  let s = String.concat "" l' in
  let sin = Scanf.Scanning.from_string ("0b" ^ s) in
  Scanf.bscanf sin "%i" (fun x -> x)
  
type wrange = (int * int)       

(* high level circuit type *)
type circuitelt =
  | Gate of natop * wrange * wrange * wrange
  | Mux of wrange * wrange * wrange * wrange
  | Copy of wrange * wrange
  | Const of wrange * int	
  | Input of string * wrange
  | Output of string * wrange
  | ShInput of wrange
  | ShOutput of wrange

type circuit = circuitelt list

let rangetolist (i, j) =
  let rec helper k l =
    if k = i then
      k::l
    else
      helper (k - 1) (k::l)
  in
  helper j []

let unitrange = (-1, -1)

let rsize (i, j) = if i = -1 && j = -1 then 0 else j - i + 1

let printwrange (i, j) = print_string "["; print_int i; print_string ", "; print_int j; print_string "]"

let printcktelt = function
  | Gate(op, r1, r2, r3) ->
    begin
      match op with
	| Natop_plus -> print_string "ADD "
	| Natop_sub -> print_string "SUB "
	| Natop_gt -> print_string "GT "
	| Natop_equals -> print_string "EQ "
	| _ -> raise (CLibError "Unsupported circuit gate")
    end;
    printwrange r1; print_string " "; printwrange r2; print_string " "; printwrange r3;
  | Mux(r1, r2, r3, r4) ->
    print_string "MUX ";
    printwrange r1; print_string " "; printwrange r2; print_string " "; printwrange r3;
    print_string " "; printwrange r4
  | Copy(r1, r2) ->
    print_string "COPY "; printwrange r1; print_string " "; printwrange r2
  | Const(r, n) ->
    print_string "CONST "; printwrange r; print_string " "; print_int n
  | Input(s, r) ->
    print_string "INPUT "; print_string s; print_string " "; printwrange r
  | Output(s, r) ->
    print_string "OUTPUT "; print_string s; print_string " "; printwrange r
  | ShInput(r) ->
    print_string "SHINPUT "; printwrange r
  | ShOutput(r) ->
    print_string "SHOUTPUT "; printwrange r
      
let printckt l = List.iter (fun elt -> printcktelt elt; print_newline ()) l

(* low level boolean circuit type *)
type booleanckt =
  | AND of int * int * int
  | XOR of int * int * int
  | INPUT of string * (int * int)
  | OUTPUT of string * (int * int)
  | SHINPUT of (int * int)
  | SHOUTPUT of (int * int)

let printbooleancktelt = function
  | AND(i1, i2, i3) ->
    print_string "AND "; print_int i1; print_string " "; print_int i2; print_string " "; print_int i3
  | XOR(i1, i2, i3) ->
    print_string "XOR "; print_int i1; print_string " "; print_int i2; print_string " "; print_int i3
  | INPUT(s, r) ->
    print_string "INPUT "; print_string s; print_string " "; printwrange r
  | OUTPUT(s, r) ->
    print_string "OUTPUT "; print_string s; print_string " "; printwrange r
  | SHINPUT(r) ->
    print_string "SHINPUT "; printwrange r
  | SHOUTPUT(r) ->
    print_string "SHOUTPUT "; printwrange r

let printbckt l = List.iter (fun elt -> printbooleancktelt elt; print_newline ()) l

let wirezero = 0

let wireone = 1

(* implementation of copy operation, i <- j *)
let copy i j = XOR(i, j, wirezero)
