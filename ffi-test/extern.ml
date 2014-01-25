open Ffi
open Printf

let print_hello name =
  printf "hello there Mr. %s\n%!" name;;

let square i = i * i;;

let () =
  register "hello" print_hello;
  register "square" square;;
