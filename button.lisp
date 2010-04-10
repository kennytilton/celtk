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

;--- button ----------------------------------------------

(deftk button (commander widget)
  ()
  (:tk-spec button
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -cursor
    -disabledforeground    (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    (tk-justify -justify) 
    -padx -pady -relief -repeatdelay
    -repeatinterval -takefocus -text -textvariable
    -underline -wraplength
    -command -compound -default -height -overrelief -state -width)
  (:default-initargs
      :id (gentemp "B")))



(defmacro mk-button-ex ((text command) &rest initargs)
  `(make-instance 'button
     :fm-parent *parent*
     :text ,text
     :on-command (c? (lambda (self)
                       (declare (ignorable self))
                       ,command))
     ,@initargs))

; --- checkbutton ---------------------------------------------

(deftk radiocheck (commander widget) 
  ()
  (:tk-spec radiocheck
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground   (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    (tk-justify -justify) -padx -pady -relief -takefocus -text -textvariable
    -underline -wraplength 
    -command -height -indicatoron -offrelief 
    -overrelief -selectcolor -selectimage -state -tristateimage 
    -tristatevalue (tk-variable -variable) -width))


(deftk checkbutton (radiocheck)
  ()
  (:tk-spec checkbutton
    -offvalue -onvalue)
  (:default-initargs
      :id (gentemp "CK")
    :value (c-in nil)
    :tk-variable (c? (^path))
    :on-command (lambda (self)
                  (setf (^value) (not (^value))))))

(defobserver .value ((self checkbutton))
  (tk-format `(:variable ,self) "set ~(~a~) ~a" (path self) (if new-value 1 0)))

; --- radiobutton -------------------------------------

(deftk radiobutton (radiocheck)
  ()
  (:tk-spec radiobutton
    -value)
  (:default-initargs
      :id (gentemp "RB")
      :tk-variable (c? (path (upper self tk-selector)))
      :on-command (lambda (self)
                 (setf (selection (upper self tk-selector)) (value self)))))

(defmacro mk-radiobutton-ex ((text value) &rest initargs)
  `(make-instance 'radiobutton
     :fm-parent *parent*
     :text ,text
     :value ,value
     ,@initargs))