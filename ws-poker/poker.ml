open Printf

open GL
open Glu
open Glut

open Sdl
open Sdlimage

open Sdldefs

let rot_circle = ref 0.0;;
let rot_card_horizontal = ref 0.0;;
let rot_card_vertical = ref 0.0;;

let load_cards () =
  List.iter (fun (s,c) ->
    let fname = "cards/" ^ c ^ s ^ ".png" in
    ignore (Loader.load_png fname)) !Cards.all_cards;

  ignore (Loader.load_png "cards/Blue_Back.png");
  ignore (Loader.load_png "cards/Red_Back.png")

let load_gfx () =
  ignore (Loader.load_png "gfx/logo.png")

let render_logo () =
  Glutil.enable2d ();

  glEnable GL_TEXTURE_2D;

  glDisable GL_LIGHTING;
  glDisable GL_DEPTH_TEST;

  let t = Loader.bind_texture "gfx/logo.png" in

  let w = Sdlutil.get_window_main () in

  glTranslatev(w.W.widthf /. 2.0,
               w.W.heightf /. 2.0,
               0.0);

  let s = 0.5 *. w.W.widthf in  
  glScalev(s, s /. t.T.aspect, 1.0);

  glTranslatev(-0.5, -0.5, 0.0);

  Glutil.draw_square_tex ();

  glDisable GL_TEXTURE_2D;

  Glutil.disable2d ()
;;
  
let render_cards () = 
  glPushMatrix ();

  glDisable GL_DEPTH_TEST;
  glLineWidth ~width: 1.0;

  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;

  let a = ref (0.0 +. !rot_card_vertical) in
  let b = ref (0.0 +. !rot_card_horizontal) in

  glTranslatev (0.0, 0.0, -10.0);
  glRotatev ~angle: (5.0) ~vec: (1.0, 0.0, 0.0);

  glRotatev ~angle: !rot_circle ~vec: (0.0, 1.0, 0.0);

  List.iter (fun (s,c) ->
    glRotatev ~angle: (360.0 /. 52.0) ~vec: (0.0, 1.0, 0.0);
    glPushMatrix();
    glTranslatev (10.0, 0.0, 0.0);
(*    glRotatev ~angle: !a ~vec: (1.0, 0.0, 0.0);*)
    glRotatev ~angle: !b ~vec: (0.0, 1.0, 0.0);
    (*a := !a +. (360.0 /. 52.0);*)
    b := !b +. (360.0 /. 52.0);
    Glcards.render (c ^ s);
    glPopMatrix();
  ) !Cards.all_cards;

  glPopMatrix ()

let update dt = 
  rot_circle := !rot_circle +.
    1.3 *. dt;
  rot_card_vertical := !rot_card_vertical +.
    13.7 *. dt;
  rot_card_horizontal := !rot_card_horizontal +.
    37.3 *. dt

let render () =
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  render_cards ();
  render_logo ()
;;

let ui_start () =
  Sdlutil.init ();

  load_cards ();
  load_gfx ();

  Sdlutil.callback_render := render;
  Sdlutil.callback_update := update;

  let m = Sdlmixer.load_music "audio/ambiance1.ogg" in
  Sdlmixer.play_music m (-1);

  Sdlutil.loop ();
  Sdlutil.quit ()

;;

let () =
  Wsffi.register "ui_start" ui_start
