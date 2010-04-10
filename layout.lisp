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

;;;  --- packing ---------------------------------------------------------

(defobserver packing ()
  (when new-value
    (assert (null (kids-packing .parent)) ()
      "Do not specify packing (here for ~a) unless parent leaves kids-packing unspecified. 
This parent is ~a, kids-packing ~a" self (list .parent (type-of .parent)) (kids-packing .parent)))
  ;
  ; This use next of the parent instead of self is pretty tricky. It has to do with getting
  ; the pack commands out with nested widgets pacing before parents. The pack command issued on behalf
  ; of a top frame is sorted on the parent. Now we have to pack the top frame. If we associate
  ; the command with the frame, the sort is a tie and either might go first. So we continue
  ; the theme and associate /this/ pack with this top frame's parent. Note that we cannot go the
  ; normal route and pack the kids in their own context, because multiple kids get packed
  ; in one pack statement (and we cannot arbitrarily pack with the first kid because this is a nested
  ; deal and any kid might have kids, so each family packs associated with itself)
  ;
  (when (and new-value (not (typep .parent 'panedwindow)))
    (tk-format `(:pack ,(fm-parent self)) new-value)))

(defmacro c?pack-self (&optional (modifier$ ""))
  `(c? (format nil "pack ~a ~a" (path self) ,modifier$)))

;;; --- grids -------------------------------------------------------------------------

(defmodel grid-manager ()())

(defobserver gridding ((self grid-manager))
  (when new-value
    (loop for k in (^kids)
          when (gridding k)
          do (tk-format `(:grid ,k) (format nil "grid ~a ~a" (path k) (gridding k))))
    (destructuring-bind (&key columns rows) new-value
      (when columns
        (loop for config in columns
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid columnconfigure ~a ~a ~a" (^path) idx config))))
      (when columns
        (loop for config in rows
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid rowconfigure ~a ~a ~a" (^path) idx config)))))))
