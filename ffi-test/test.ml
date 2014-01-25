open Ffi
open Printf

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let () =
  load "extern.cmxs";

  Ffi.call "hello" "Piotr";

  let res = Ffi.call "square" 42 in
  printf "%d * %d = %d\n%!" 42 42 res;

  let imp = Ffi.get_imp "hello" in
  imp "Bob";

  if Array.length Sys.argv > 1 && Sys.argv.(1) = "gfx" then
    begin
      load "extern_gfx.cmxs";
      Ffi.call "gfx_cube" ();
    end
  


