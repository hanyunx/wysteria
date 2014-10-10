open Ffi
open Printf

open GL
open Glu
open Glut

open Sdl
open Sdlimage

let rot1 = ref 0.0;;
let rot2 = ref 0.0;;
let rot3 = ref 0.0;;

type tex = {mutable gltid: texture_id option;
            surf: Sdlsurface.t;
            width: int;
            height: int;
            widthf: float;
            heightf: float}

let card_width = 512.0;;
let card_height = 715.0;;

let textures = Hashtbl.create 16;;

let load_png f =
  printf "loading [%s]\n%!" f;
  let rwo = Sdlrwops.from_file ~filename:f ~mode: "rb" in

  let s = load_png_rw rwo in

  let temp = {gltid = None;
              surf = s;
              width = Sdlsurface.get_width s;
              height = Sdlsurface.get_height s;
              widthf = float_of_int (Sdlsurface.get_width s);
              heightf = float_of_int (Sdlsurface.get_height s)
             } in

  let tid = glGenTexture () in
  
  temp.gltid <- Some tid;

  glBindTexture ~target: BindTex.GL_TEXTURE_2D ~texture: tid;

  let (intmode, mode) =
    (if (Sdlsurface.get_bits_per_pixel s) = 32 then
        (InternalFormat.GL_RGBA, GL_RGBA) else 
        (InternalFormat.GL_RGB, GL_RGB)) in

  glTexImage2D
    ~target: TexTarget.GL_TEXTURE_2D
    ~level: 0
    ~internal_format: intmode
    ~width: temp.width
    ~height: temp.height
    ~format_: mode
    ~type_: GL_UNSIGNED_BYTE
    ~pixels: (Bigarray.genarray_of_array1 (Sdlsurface.get_pixels s));

  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);
  glTexParameter
    ~target: TexParam.GL_TEXTURE_2D
    ~param: (TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);

  Hashtbl.replace textures f temp;
  temp

let all_cards = ref [];;

let load_cards () =
  let suits = ["C"; "D"; "H"; "S"] in
  let faces = ["A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K"] in

  all_cards := Util.lists_prod suits faces;

  List.iter (fun (s,c) ->
    let fname = "cards/" ^ c ^ s ^ ".png" in
    ignore (load_png fname)) !all_cards

let get_some x = match x with
  | Some y -> y
  | _ -> failwith "couldn't get some"

let bind_texture f =
  let t = try Hashtbl.find textures f with
      Not_found -> load_png f in
  let tid = get_some t.gltid in
  glBindTexture BindTex.GL_TEXTURE_2D tid;
  t

let string_of_version (a,b,c) =
  sprintf "sdl version %d.%d.%d" a b c;;

let render_card c =
  let t = bind_texture ("cards/" ^ c ^ ".png") in

  let w = t.widthf /. t.heightf in

  glEnable GL_TEXTURE_2D;
  glDisable GL_LIGHTING;

  glEnable GL_CULL_FACE;
  glCullFace GL_BACK;

  glEnable GL_DEPTH_TEST;
  
  glPushMatrix();

  glTranslatev (w /. (-2.0), -0.5, -0.001);
  
  glBegin GL_QUADS;
  glTexCoord2v (1.0, 1.0); glVertex2v (w, 1.0);
  glTexCoord2v (1.0, 0.0); glVertex2v (w, 0.0);
  glTexCoord2v (0.0, 0.0); glVertex2v (0.0, 0.0);
  glTexCoord2v (0.0, 1.0); glVertex2v (0.0, 1.0);
  glEnd ();

  glTranslatev (0.0, 0.0, 0.002);

  let t = bind_texture ("cards/Red_Back.png") in

  glBegin GL_QUADS;
  glTexCoord2v (1.0, 1.0); glVertex2v (w, 1.0);
  glTexCoord2v (0.0, 1.0); glVertex2v (0.0, 1.0);
  glTexCoord2v (0.0, 0.0); glVertex2v (0.0, 0.0);
  glTexCoord2v (1.0, 0.0); glVertex2v (w, 0.0);
  glEnd ();

  glPopMatrix();

  glDisable GL_TEXTURE_2D


let render () =
  rot1 := !rot1 +. 0.02;
  rot2 := !rot2 +. 1.3;
  rot3 := !rot3 +. 0.7;

  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glPushMatrix ();

  glDisable GL_DEPTH_TEST;
  glLineWidth ~width: 1.0;

  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;

  let a = ref (37.0 +. !rot2) in
  let b = ref (73.0 +. !rot3) in

  glTranslatev (0.0, 0.0, -11.0);
  glRotatev ~angle: (5.0) ~vec: (1.0, 0.0, 0.0);

  glRotatev ~angle: !rot1 ~vec: (0.0, 1.0, 0.0);

  List.iter (fun (s,c) ->
    glRotatev ~angle: (360.0 /. 52.0) ~vec: (0.0, 1.0, 0.0);

    glPushMatrix();

    glTranslatev (10.0, 0.0, 0.0);

    (*glRotatev ~angle: !a ~vec: (1.0, 0.0, 0.0);*)
    glRotatev ~angle: !b ~vec: (0.0, 1.0, 0.0);

    a := !a +. (360.0 /. 52.0);
    b := !b +. (360.0 /. 52.0);

    render_card (c ^ s);

    glPopMatrix();
  ) !all_cards;

  glPopMatrix ()

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
  ignore (Sdl.GL.make_current ~win: w ~ctx: c);
  let x = Sdl.GL.get_swap_interval () in

  load_cards ();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective
    ~fovy: 80.0
    ~aspect: (3.0 /. 2.0)
    ~zNear: 0.01
    ~zFar: 25.0;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatev (0.0, 0.0, -2.0);

  while true do
    event_loop ();
    render ();
    Sdl.GL.swap_window w;
    Sdl.Timer.delay (1000 / 60);
  done;


  Sdl.quit ()
;;

let () =
  register "gfx_cube" gfx_cube
  
