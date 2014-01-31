open Sdl
open Printf
open Sdlevent
open Sdlscancode

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let proc_events ev = match ev with
  | Sdlevent.KeyDown { scancode = Sdlscancode.ESCAPE } ->
    printf "exiting\n%!";
    exit 0
  | _ -> ()
;;  

let rec event_loop () = 
  match Sdlevent.poll_event() with
    | Some ev -> proc_events ev; event_loop ()
    | None -> ()
;;

let window_main = ref None;;

let init () = 
  printf "sdl = %s\n%!" (string_of_version (Version.get_runtime_version ()));

  Sdl.init [`VIDEO];

  printf "initializing gfx\n%!";

  let w = Window.create
    ~title: "ffi test"
    ~pos: (`centered, `centered) 
    ~dims: (640, 480)
    ~flags: [Sdl.Window.OpenGL] in

  window_main := Some w;

  let r = Sdl.Render.create_renderer
    ~win: w
    ~index: 0
    ~flags: [Sdl.Render.Accelerated] in

  let c = Sdl.GL.create_context ~win: w in
  ignore (Sdl.GL.make_current ~win: w ~ctx: c);
  let x = Sdl.GL.get_swap_interval () in

  Glutil.init ();
;;

let quit () =
  Sdl.quit ()

let do_nothing () = ();;

let callback_render = ref do_nothing;;

let render () = !callback_render ();;

let loop () =
  let w = Util.get_some !window_main in

  while true do
    event_loop ();
    render ();
    Sdl.GL.swap_window w;
    Sdl.Timer.delay (1000 / 60);
  done
;;

