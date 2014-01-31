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
    ~aspect: w.aspect
    ~zNear: 0.01
    ~zFar: 25.0;

  glViewport ~x: 0 ~y: 0 ~width: w.width ~height: w.height;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatev (0.0, 0.0, -2.0)
;;

let init () =
  glEnable GL_NORMALIZE;
  glEnable GL_BLEND;
  glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA
;;
