open Printf
open Dynlink

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

