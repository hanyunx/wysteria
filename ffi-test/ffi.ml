open Printf
open Dynlink

(*
type wsvalue =
  | WSInt of int
  | WSFun of wsvalue -> wsvalue

type wstype =
  | WSTInt
  | WSTFun of wstype * wstype

type ffivalue =
  | FFIInt of int
  | FFFIFun of ffivalue -> ffivalue

let rec ws_to_ffi v t = match (v,t) with
  | (WSInt i, WSTInt) -> FFIInt i
  | (WSFun f, WSTFun (it, ot)) -> 
    fun ffiinput ->
      let wsinput = ffi_to_ws ffiinput it in
      let wsoutput = f wsinput in
      let ffioutput = ws_to_ffi wsoutput ot in
      ffioutput

let rec ffi_to_ws v t = match (v,t) with
  | (FFIInt i, WSTInt) -> WSInt i
  | (FFIFun f, WSTFun (it, ot)) -> 
    fun wsinput ->
      let ffiinput = ws_to_ffi wsinput it in
      let ffioutput = f ffiinput in
      let wsoutput = ffi_to_ws ffioutput ot in
      wsoutput
;;
*)

type value = Obj.t

let box x = Obj.repr x;;
let unbox x = Obj.obj x;;

type ffi_imp = value -> value;;

let load f =
  try loadfile f
  with Error e ->
    eprintf "%s\n%!" (error_message e);
    raise (Error e)

let (ffi_register: (string, ffi_imp) Hashtbl.t) = Hashtbl.create 0;;

let register s f =
  let f v = Obj.repr (f (Obj.obj v)) in
  Hashtbl.replace ffi_register s f

let get_imp s =
  let imp = Hashtbl.find ffi_register s in
  fun d -> Obj.obj (imp (Obj.repr d))

let call s d =
  let ret = (get_imp s) d in
  ret

