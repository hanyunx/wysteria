open Ffi
open Printf

open GL
open Glu
open Glut

open Sdl

let rot1 = ref 0.0;;
let rot2 = ref 0.0;;

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let render () =
  rot1 := !rot1 +. 0.7;
  rot2 := !rot2 +. 1.1;

  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glPushMatrix ();

  glDisable GL_DEPTH_TEST;
  glLineWidth ~width: 1.0;

  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;

  glRotatev ~angle: !rot1 ~vec: (1.0, 0.0, 0.0);
  glRotatev ~angle: !rot2 ~vec: (0.0, 1.0, 0.0);

  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 1.0;
  glutSolidCube ~size: 1.0;

  glColor4 ~r: 1.0 ~g: 1.0 ~b: 1.0 ~a: 1.0;
  glutWireCube ~size: 1.0;

  glPopMatrix ()

let gfx_cube () =
  printf "sdl = %s\n%!" (string_of_version (Version.get_runtime_version ()));

  Sdl.init [`VIDEO];

  printf "initializing gfx\n%!";

  let w = Window.create
    ~title: "ffi test"
    ~pos: (`centered, `centered) 
    ~dims: (640, 480)
    ~flags: [Sdl.Window.OpenGL] in

  let r = Sdl.Render.create_renderer
    ~win: w
    ~index: 0
    ~flags: [Sdl.Render.Accelerated] in

  let c = Sdl.GL.create_context ~win: w in
  Sdl.GL.make_current ~win: w ~ctx: c;
  let x = Sdl.GL.get_swap_interval () in

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective
    ~fovy: 80.0
    ~aspect: (3.0 /. 2.0)
    ~zNear: 0.01
    ~zFar: 5.0;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatev (0.0, 0.0, -2.0);

  for i = 1 to 100 do
    render ();
    Sdl.GL.swap_window w;
    Sdl.Timer.delay 10
  done;


  Sdl.quit ()
;;

let () =
  register "gfx_cube" gfx_cube
  
