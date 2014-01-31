open GL
open Glu
open Printf
open Sdl
open Sdlimage

let init () =
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective
    ~fovy: 80.0
    ~aspect: (3.0 /. 2.0)
    ~zNear: 0.01
    ~zFar: 25.0;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatev (0.0, 0.0, -2.0)
