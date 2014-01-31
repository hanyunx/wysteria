open Sdl
open Printf
open Sdlevent
open Sdlscancode

open Sdldefs

let window_main = ref None

let get_window_main () = Util.get_some !window_main

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let callback_render = ref (fun () -> ());;
let callback_proc_events = ref (fun e -> ());;

let render () = !callback_render ();;

let quit () =
  Sdl.quit ()

let proc_events ev = match ev with
  | KeyDown { scancode = Sdlscancode.ESCAPE } ->
    quit (); exit 0
  | Window_Event { kind = WindowEvent_Resized p } ->
    let w = Util.get_some !window_main in
    w.W.width <- p.win_x;
    w.W.height <- p.win_y;
    w.W.widthf <- float_of_int p.win_x;
    w.W.heightf <- float_of_int p.win_y;
    w.W.aspect <- w.W.widthf /. w.W.heightf;
    Glutil.resize w
      
  | Quit e ->
    quit (); exit 0                                
  | _ -> !callback_proc_events ev
;;  

let rec event_loop () = 
  match Sdlevent.poll_event() with
    | Some ev -> proc_events ev; event_loop ()
    | None -> ()
;;

let init () = 
  printf "sdl = %s\n%!" (string_of_version (Version.get_runtime_version ()));

  Sdl.init [`VIDEO];

  printf "initializing gfx\n%!";

  let w = Window.create
    ~title: "ffi test"
    ~pos: (`centered, `centered) 
    ~dims: (640, 480)
    ~flags: [Sdlwindow.OpenGL;
             Sdlwindow.Resizable
            ] in

  let win = {W.width= 640;
             W.height= 480;
             W.widthf= 640.0;
             W.heightf= 480.0;
             W.aspect=640.0 /. 480.0;
             W.win= w} in

  window_main := Some win;

  let r = Sdl.Render.create_renderer
    ~win: w
    ~index: 0
    ~flags: [Sdl.Render.Accelerated] in

  let c = Sdl.GL.create_context ~win: w in
  ignore (Sdl.GL.make_current ~win: w ~ctx: c);
  let x = Sdl.GL.get_swap_interval () in

  Glutil.init ();
  Glutil.resize win
;;

let loop () =
  let win = (Util.get_some !window_main).W.win in

  while true do
    event_loop ();
    render ();
    Sdl.GL.swap_window win;
    Sdl.Timer.delay (1000 / 60);
  done
;;

