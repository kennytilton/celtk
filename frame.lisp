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

;--- group geometry -----------------------------------------

(defmodel inline-mixin (composite-widget widget)
  ((padx :initarg :padx :accessor padx :initform 0)
   (pady :initarg :pady :accessor pady :initform 0)
   (packing-side :initarg :packing-side :accessor packing-side :initform 'left)
   (layout-anchor :initarg :layout-anchor :accessor layout-anchor :initform 'nw))
  (:default-initargs
      :kid-slots (lambda (self) ;; /// vestigial? packing now defaults to nil anyway, methinks
                   (declare (ignore self))
                   (list
                    (mk-kid-slot (packing :if-missing t)
                      nil))) ;; suppress default (but see overall comment a few lines up)
    :kids-packing (c? (when (^kids)
                        (format nil "pack~{ ~a~} -side ~a -anchor ~a -padx ~a -pady ~a"
                          (mapcar 'path (^kids))
                          (down$ (^packing-side))
                          (down$ (^layout-anchor))
                          (^padx)(^pady))))))

(defobserver kids-packing ()
  (when new-value
    (tk-format `(:pack ,self kids-packing) new-value)))

(defmodel row-mixin (inline-mixin)
  ()
  (:default-initargs
    :packing-side 'left))

(defmodel stack-mixin (inline-mixin)
  ()
  (:default-initargs
    :packing-side 'top))


;--- f r a m e --------------------------------------------------

(deftk frame (composite-widget widget)
  ()
  (:tk-spec frame -borderwidth -cursor	-highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background (tk-class -class) 
    -colormap -container -height -visual -width)
  (:default-initargs
      :id (gentemp "F")))

(deftk frame-selector (tk-selector frame) ())
(deftk frame-row (row-mixin frame-selector)())
(deftk frame-stack (stack-mixin frame-selector)())


;--- l a b e l f r a m e ----------------------------------------------

(deftk labelframe (widget)
  ()
  (:tk-spec labelframe -borderwidth -cursor -highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background (tk-class -class) -colormap -container -height -visual -width
    -text -labelanchor -labelwidget)
  (:default-initargs
      :id (gentemp "LF")))

(deftk labelframe-selector (tk-selector labelframe)())
(deftk labelframe-row (row-mixin labelframe-selector)())
(deftk labelframe-stack (stack-mixin labelframe-selector)())

;;; --- handy macros

(defmacro def-mk-inline (name (unlabelled labelled))
  `(defmacro ,name ((&rest initargs) &rest kids)
     (if (evenp (length initargs))
         `(make-instance ',',unlabelled
            :fm-parent *parent*
            ,@initargs
            :kids (c? (the-kids ,@kids)))
       `(make-instance ',',labelled
          :fm-parent *parent*
          :text ,(car initargs)
          ,@(cdr initargs)
          :kids (c? (the-kids ,@kids))))))

(def-mk-inline mk-row (frame-row labelframe-row))
(def-mk-inline mk-stack (frame-stack labelframe-stack))

