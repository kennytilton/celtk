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


; --- scroll bars ----------------------------------------

(deftk scrollbar (widget)
  ()
  (:tk-spec scrollbar
    -activebackground -activerelief
    -background -borderwidth -command -cursor
    -elementborderwidth
    -highlightbackground -highlightcolor -highlightthickness
    -jump -orient -relief -repeatdelay
    -repeatinterval  -takefocus
    -troughcolor -width)
  (:default-initargs
      :id (gentemp "SBAR")))

(deftk scrolled-list (row-mixin frame-selector)
  ((list-item-keys :initarg :list-item-keys :accessor list-item-keys :initform nil)
   (list-item-factory :initarg :list-item-factory :accessor list-item-factory :initform nil)
   (list-height :initarg :list-height :accessor list-height :initform nil)
   (tkfont :initarg :tkfont :accessor tkfont :initform (c-in '(courier 9)))
   (width :initarg :width :accessor width :initform (c-in 20))
   (activestyle :initarg :activestyle :accessor activestyle :initform (c-in nil))
   (selectforeground :initarg :selectforeground
                     :accessor selectforeground :initform (c-in "black"))
   (selectbackground :initarg :selectbackground
                     :accessor selectbackground :initform (c-in nil))
   (selectmode :initarg :selectmode
                     :accessor selectmode :initform (c-in 'single))
   
   )
  (:default-initargs
      :list-height (c? (max 1 (length (^list-item-keys))))
    :kids-packing nil
      :kids (c? (the-kids
                 (mk-listbox :id :list-me
                   :kids (c? (the-kids
                              (mapcar (list-item-factory .parent)
                                (list-item-keys .parent))))
                   :tkfont (c? (tkfont .parent))
                   :width (c? (width .parent))
                   :activestyle (c? (activestyle .parent))
                   :selectforeground (c? (selectforeground .parent))
                   :selectbackground (c? (selectbackground .parent))
                   :selectmode (c? (selectmode .parent))
                   :state (c? (if (enabled .parent) 'normal 'disabled))
                   :takefocus (c? (if (enabled .parent) 1 0))
                   :height (c? (list-height .parent))
                   :packing (c? (format nil "pack ~a -side left -fill both -expand 1" (^path)))
                   :yscrollcommand (c? (when (enabled .parent)
                                         (format nil "~a set" (path (nsib))))))
                 (mk-scrollbar :id :vscroll
                     :packing (c?pack-self "-side right -fill y")
                     :command (c? (format nil "~a yview" (path (psib)))))))))

(defmethod tk-output-selection :after ((self scrolled-list) new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (trc nil "scrolled-list selection output" self new-value)
  (when new-value
    (let ((lb (car (^kids)))
          (item-no (position new-value (^list-item-keys) :test 'equal)))
      (trc nil "tk-output selection: lb | item-no | path of lb " lb item-no (path lb))

      (if item-no
          (tk-format `(:selection ,self) "~(~a~) selection set ~a" (path lb) item-no)
        (break "~&scrolled-list ~a selection ~a not found in item keys ~a" self new-value (^list-item-keys))))))


;--- scroller (of canvas; need to generalize this) ----------

(defmodel scroller (grid-manager frame)
  ((canvas :initarg :canvas :accessor canvas :initform nil))
  (:default-initargs
      :id :cv-scroller
    :kids-packing nil
    :gridding '(:columns ("-weight {1}" "-weight {0}")
                 :rows ("-weight {1}" "-weight {0}"))
    :kids (c? (the-kids
               (^canvas)
               (mk-scrollbar :id :hscroll
                 :orient "horizontal"
                 :gridding "-row 1 -column 0 -sticky we"
                 :command (c? (format nil "~a xview" (path (kid1 .parent)))))
               (mk-scrollbar :id :vscroll
                 :orient "vertical"
                 :gridding "-row 0 -column 1 -sticky ns"
                 :command (c? (format nil "~a yview" (path (kid1 .parent)))))))))

(defmacro mk-scroller (&rest iargs)
  `(make-instance 'scroller
     :fm-parent self
     ,@iargs))

(defmethod initialize-instance :after ((self scroller) &key)
  ;
  ; Tk does not do late binding on widget refs, so the canvas cannot mention the scrollbars
  ; in x/y scrollcommands since the canvas gets made first
  ;
  (with-integrity (:client `(:post-make-tk ,self))
    (setf (xscrollcommand (kid1 self)) (format nil "~a set" (path (fm! :hscroll))))
    (setf (yscrollcommand (kid1 self)) (format nil "~a set" (path (fm! :vscroll))))))

