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

(deftk text-item (tkfontified item)
  ()
  (:tk-spec text
    (tk-fill -fill)
    -activefill
    -disabledfill
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    ;; -- special ---
    -anchor
    (tkfont -font)
    (tk-justify -justify)
    -text
    -underline
    -width)
  (:default-initargs
      :l-coords '(0 0) ;; the p-offset will positon the text
      :coords-tweak (c_? (eko (nil "coords tweak" self)
                          (list 0 0 #+not (- (* (tk-scaling .tkw) (^tkfont-ascent))))))))
