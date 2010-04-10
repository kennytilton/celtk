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

(deftk bitmap (item)
  ()
  (:tk-spec bitmap
    -state -tags
    -anchor
    -background
    -activebackground
    -disabledbackground
    -bitmap
    -activebitmap
    -disabledbitmap
    -foreground
    -activeforeground
    -disabledforeground))


(deftk image-item (item)
  ()
  (:tk-spec image
    -state
    -tags
    -anchor
    -image
    -activeimage
    -disabledimage))

