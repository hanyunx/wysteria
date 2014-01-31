open Printf

let () =
  Wsffi.load "poker.cmxs";
  Wsffi.call "ui_start" ()
