open GL
open Glu
open Printf
open Sdl
open Sdlimage

open Sdldefs

let resize w =
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective
    ~fovy: 80.0
    ~aspect: w.W.aspect
    ~zNear: 0.01
    ~zFar: 25.0;

  glViewport ~x: 0 ~y: 0 ~width: w.W.width ~height: w.W.height;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatev (0.0, 0.0, -2.0)
;;

let init () =
  glEnable GL_NORMALIZE;
  glEnable GL_BLEND;
  glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA
;;

(*let make_ttf_text s =
  try Hashtbl.find textures s
  with Not_found ->
    let surface = Sdlttf.render_text_blended (get font) s Sdlvideo.white in
    let sinfo = Sdlvideo.surface_info surface in
    let temp = {surface = surface;
                tid = None;
                width = sinfo.Sdlvideo.w;
                height = sinfo.Sdlvideo.h;
                widthf = float_of_int (sinfo.Sdlvideo.w);
                heightf = float_of_int (sinfo.Sdlvideo.h)} in

    Hashtbl.replace textures s temp;
    temp
;;*)

let enable2d () =
  (* http://www.gamedev.net/community/forums/topic.asp?topic_id=104791*)
  let (v0,v1,v2,v3) = glGetInteger4 Get.GL_VIEWPORT in

  glMatrixMode GL_PROJECTION; glPushMatrix (); glLoadIdentity ();
  glOrtho 0.0 (float_of_int v2) (float_of_int v3) 0.0 (-1.0) 1.0;
  glMatrixMode GL_MODELVIEW; glPushMatrix (); glLoadIdentity ()
;;

let disable2d () =
  glMatrixMode GL_PROJECTION; glPopMatrix();
  glMatrixMode GL_MODELVIEW; glPopMatrix()
;;

let draw_square_tex () =
  glBegin GL_QUADS;
  glTexCoord2v(1.0, 1.0); glVertex2v(1.0, 1.0);
  glTexCoord2v(1.0, 0.0); glVertex2v(1.0, 0.0);
  glTexCoord2v(0.0, 0.0); glVertex2v(0.0, 0.0);
  glTexCoord2v(0.0, 1.0); glVertex2v(0.0, 1.0);
  glEnd ();
;;

(*
let draw_wire_box ~xmin ~xmax ~ymin ~ymax ~zmin ~zmax =
  glBegin GL_LINE_LOOP;
  glVertex3 ~x: xmin ~y: ymin ~z: zmax;
  glVertex3 ~x: xmin ~y: ymax ~z: zmax;
  glVertex3 ~x: xmin ~y: ymax ~z: zmin;
  glVertex3 ~x: xmin ~y: ymin ~z: zmin;
  glEnd ();

  glBegin GL_LINE_LOOP;
  glVertex3 ~x: xmax ~y: ymin ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmin;
  glVertex3 ~x: xmax ~y: ymin ~z: zmin;
  glEnd ();

  glBegin GL_LINES;
  glVertex3 ~x: xmin ~y: ymin ~z: zmax;
  glVertex3 ~x: xmax ~y: ymin ~z: zmax;

  glVertex3 ~x: xmin ~y: ymax ~z: zmax;
  glVertex3 ~x: xmax ~y: ymax ~z: zmax;

  glVertex3 ~x: xmin ~y: ymax ~z: zmin;
  glVertex3 ~x: xmax ~y: ymax ~z: zmin;

  glVertex3 ~x: xmin ~y: ymin ~z: zmin;
  glVertex3 ~x: xmax ~y: ymin ~z: zmin;
  glEnd ()
;;

let colored_wire_box_by_coords carray =
  let draw_vec = (fun i ->
    let (col, vec) = carray.(i) in
    glColor4v col; glVertex3v vec) in

  glBegin GL_LINE_LOOP;
  List.iter draw_vec [0;3;2;1];
  glEnd ();

  glBegin GL_LINE_LOOP;
  List.iter draw_vec [4;5;6;7];
  glEnd ();

  glBegin GL_LINES;
  List.iter draw_vec [0;4; 1;5; 2;6; 3;7];
  glEnd ();
;;

let colored_solid_box_by_coords carray =
  let draw_vec = (fun i ->
    let (col, vec) = carray.(i) in
    glColor4v col; glVertex3v vec) in

  glBegin GL_QUADS;
  List.iter draw_vec [0;3;2;1; 4;5;6;7; 0;4;7;3; 1;2;6;5; 0;1;5;4; 3;7;6;2];
  glEnd ();
;;

let set_window_size w h =
  w_width := w;
  w_height := h;
  w_widthf := float_of_int !w_width;
  w_heightf := float_of_int !w_height;
  w_aspect := !w_widthf /. !w_heightf;

  exec_handler reinit_handler
;;

let toggle_fullscreen () =
  fullscreen := not !fullscreen;

  if !fullscreen then begin
    match Sdlvideo.list_modes [`FULLSCREEN; `OPENGL] with
    | Sdlvideo.DIM (modes) ->
      begin match List.rev modes with
      | (w,h) :: _ -> set_window_size w h
      | _ -> failwith "could not get any video modes"
      end
    | _ -> failwith "could not get any video modes"
  end;

  exec_handler reinit_handler
;;
*)
