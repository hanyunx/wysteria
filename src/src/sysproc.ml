(* A simple abstraction for system processes with which the Wysteria
   interpreter interacts via IPC. *)

module type PROC = sig
  type t
  val run : string -> t
  val send : t -> string -> unit
  val recv : t -> string
  val stop : t -> unit

  val to_string : t -> string    
  val call : t -> string -> string list -> string
end

module Proc : PROC = struct

  type t = (string * 
              in_channel * 
              out_channel )

  let db_out o s = 
    (* Printf.fprintf stderr "output_string `%s'\n" s ; *)
    Pervasives.output_string o s
      
  let run cmd = 
    let i, o = Unix.open_process cmd in
    (cmd, i, o)

  let to_string p = 
    let s, _, _ = p in s

  let recv proc = begin
    let _, i, _ = proc in
    Pervasives.input_line i
  end

  let send proc msg = begin
    let _, _, o = proc in
    (db_out o)  msg ;
    (db_out o) "\n" ;
    flush o ;
  end

  let stop proc = begin
    let _, i, o = proc in
    (* TODO: Do something with the returned process_status ? *)
    ignore (Unix.close_process (i,o))
  end

  let call proc fname fargs = begin
    let _, _, o = proc in
    (* Printf.fprintf stderr "calling: %s %!\n" fname ; *)
    (db_out o) fname ;
    (db_out o) " " ;
    List.iter begin fun farg ->
      (db_out o) farg ;
      (db_out o) " " ;
    end fargs ;
    (db_out o) "\n" ;
    flush o ;
    (* Printf.fprintf stderr "waiting..%!\n" ; *)
    let x = recv proc in
    (* Printf.fprintf stderr "ret'd: %s\n" x ; *)
    x
  end
end
