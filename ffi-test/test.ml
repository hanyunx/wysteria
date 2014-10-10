open Ffi
open Printf

let () =
  load "extern.cmxs";

  ignore (Ffi.call "hello" "Piotr");

  let res = Ffi.call "square" 42 in
  printf "%d * %d = %d\n%!" 42 42 res;

  let imp = Ffi.get_imp "hello" in
  ignore (imp "Bob");

  if Array.length Sys.argv > 1 && Sys.argv.(1) = "link" then
    begin
      load "extern_test.cmxs";
      Ffi.call "test" ()
    end;
  
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "gfx" then
    begin
      load "extern_gfx.cmxs";
      Ffi.call "gfx_cube" ()
    end
  


