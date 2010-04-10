;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

(deftk oval (item)
  ()
  (:tk-spec oval
    -dash
   -activedash
   -disableddash
   -dashoffset
   (tk-fill -fill)
   -activefill
   -disabledfill
   -offset
   -outline
   -activeoutline
   -disabledoutline
   -outlinestipple
   -activeoutlinestipple
   -disabledoutlinestipple
   -stipple
   -activestipple
   -disabledstipple
   -state
   -tags
   -width
   -activewidth
   -disabledwidth))

(deftk polygon (item)
  ()
  (:tk-spec polygon
    -dash
    -activedash
    -disableddash
    -dashoffset
    (tk-fill -fill)
    -activefill
    -disabledfill
    -offset
    -outline
    -activeoutline
    -disabledoutline
    -outlinestipple
    -activeoutlinestipple
    -disabledoutlinestipple
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    -width
    -activewidth
    -joinstyle -smooth -splinesteps))

(deftk rectangle (item)
  ()
  (:tk-spec rectangle
    -dash
   -activedash
   -disableddash
   -dashoffset
   (tk-fill -fill)
   -activefill
   -disabledfill
   -offset
   -outline
   -activeoutline
   -disabledoutline
   -outlinestipple
   -activeoutlinestipple
   -disabledoutlinestipple
   -stipple
   -activestipple
   -disabledstipple
   -state
   -tags
   -width
   -activewidth
   -disabledwidth))
