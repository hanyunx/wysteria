open Sdl
open Printf
open Sdlevent
open Sdlscancode

open Sdldefs

let target_delay = ref 0

let set_target_fps f =
  target_delay := int_of_float (1000.0 /. f)
  
let () = set_target_fps 60.0

let window_main = ref None

let get_window_main () = Util.get_some !window_main

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let callback_render = ref (fun () -> ());;
let callback_proc_events = ref (fun e -> ());;
let callback_update = ref (fun dt -> ());;

let render () = !callback_render ();;
let update dt = !callback_update dt;;

let quit () =
  Sdl.Audio.quit ();
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

  let dname = (Sdl.Audio.get_drivers ()).(0) in

  printf "initializing audio: %s\n%!" dname;

  Sdl.Audio.init ~driver_name: dname;

  printf "initializing mixer: %d.%d\n%!"
    (Sdlmixer.get_major_version ())
    (Sdlmixer.get_minor_version ());

  Sdlmixer.init [`MP3];
  
  let m = Sdlmixer.load_music "audio/ambiance1.ogg" in

  Sdlmixer.open_audio
    (Sdlmixer.get_default_frequency ())
    (Sdlaudio.AUDIO_S32SYS)
    (Sdlmixer.get_default_channels ())
    1024;

  Sdlmixer.play_music m (-1);

  Glutil.init ();
  Glutil.resize win
;;

let loop () =
  let win = (Util.get_some !window_main).W.win in

  let last_tick = ref 0 in

  while true do
    let delay = (Sdl.Timer.get_ticks ()) - !last_tick in

    if delay <= !target_delay then Sdl.Timer.delay (!target_delay - delay);
 
    let current_tick = Sdl.Timer.get_ticks () in
    let delay = !last_tick - current_tick in
    last_tick := current_tick;

    event_loop ();

    update ((float_of_int delay) *. 0.001);

    render ();

    Sdl.GL.swap_window win;
  done
;;

