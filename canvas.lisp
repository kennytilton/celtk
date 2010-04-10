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

(deftk canvas (widget)
  ((active :initarg :active :accessor active :initform (c-in t))   
   )
  (:tk-spec canvas
    -background -borderwidth -cursor
    -highlightbackground -highlightcolor -highlightthickness
    -insertbackground -insertborderwidth -insertofftime -insertontime -insertwidth
    -relief -selectbackground -selectborderwidth -selectforeground
    -state -takefocus -xscrollcommand -yscrollcommand
    -closeenough -confine -height (scroll-region -scrollregion) -width 
    -xscrollincrement -yscrollincrement)
  (:default-initargs
      :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :id (gentemp "CV")
    :tile? nil))

(defun focusIn->active ()
  (list '|<FocusIn>| (lambda (self event &rest args)
                      (declare (ignorable event))
                      (trc "focus in activating" self event args)
                      (setf (^active) t))))

(defun focusOut->active ()
  (list '|<FocusOut>| (lambda (self event &rest args) 
                       (declare (ignorable event))
                        (trc "focus out de-activating" self event args)
                       (setf (^active) nil))))

(deftk arc (item)
  ()
  (:tk-spec arc
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
    -disabledwidth
    -extent -start -style))

(deftk line (item)
  ()
  (:tk-spec line
    -dash
    -activedash
    -disableddash
    -dashoffset
    (tk-fill -fill)
    -activefill
    -disabledfill
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    -width
    -activewidth
    -disabledwidth
    -arrow -arrowshape -capstyle -joinstyle -smooth -splinesteps))

