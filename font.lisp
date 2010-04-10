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

;;; --- fonts obtained from Tk-land ---------------

(eval-now!
  (export '(make-tkfinfo tkfinfo-family tkfinfo-size tkfinfo-slant tkfinfo-ascent  tkfinfo-linespace tkfinfo-fixed
             tkfont-id tkfont-info tkfinfo-ascent tkfont-height tkfont-ascent 
             tkfinfo-descent ^tkfont-descent ^tkfont-find
             tkfinfo tkfinfo-em ^tkfont-em 
             line-up line-down tkfont-size-info)))

(defmacro def^macros (&rest fn-names)
  `(progn ,@(loop for fn-name in fn-names
                  collecting (let ((^name (format nil "^~:@(~a~)" fn-name)))
                               `(progn
                                  (eval-now!
                                    (export '(,(intern ^name))))
                                  (defmacro ,(intern ^name) ()
                                    `(,',fn-name self)))))))

(def^macros line-up line-down tkfont-height tkfont-ascent tkfinfo-descent)

(defstruct tkfinfo id family size slant ascent descent linespace fixed em)

(deftk tkfont (widget)
  ()
  (:tk-spec font
    -family -size -weight -slant -underline -overstrike)
  (:default-initargs
      :id (gentemp "fnt")))

(defmethod make-tk-instance ((self tkfont))
  (setf (gethash (^path) (dictionary .tkw)) self)
  (tk-format `(:make-tk ,self) "font create ~a ~{~(~a~) ~a~^ ~}"
      (tkfont-id self)(tk-configurations self)))

(defmethod tk-configure ((self tkfont) option value)
  (tk-format `(:configure ,self ,option) "font configure ~(~a~) ~(~a~) ~a"
    (path self) option (tk-send-value value)))

(defun tkfont-id (tkfont) (md-name tkfont))

(defmethod path ((self tkfont))
  (tkfont-id self))

(defmacro ^tkfont-find (tkfont-id)
  `(cdr (assoc ,tkfont-id (tkfont-info .tkw))))
      
(defmodel tkfontified ()
  ((fkey :initarg :fkey :accessor fkey :initform nil)
   (f-size-step :initarg :f-size-step :accessor f-size-step
     :initform 0)
   (tkfinfo :initarg :tkfinfo :accessor tkfinfo
     :initform (c_? (bwhen (fkey (^fkey))
                       (let ((fkey-table (cdr (assoc fkey (tkfont-info .tkw)))))
                         (assert fkey-table () "no such tkfont: ~a ~a" fkey (symbol-package fkey))
                         (svref fkey-table (^f-size-step)))))))
  (:default-initargs
      :tkfont (c_? (bwhen (fi (^tkfinfo))
                  (tkfinfo-id fi)))))

(defun tkfont-size-info (self tkfont decrements)
  (let ((tkfont-size-table (cdr (assoc tkfont (tkfont-info .tkw)))))
    (assert tkfont-size-table () "no such tkfont: ~a ~a" tkfont (symbol-package tkfont))
    (svref tkfont-size-table (+ 2 decrements)))) ;; we allow -decrements as a guess that it will be needed. dumb. :)

(defun tkfont-ascent (self)
  (tkfinfo-ascent (^tkfinfo)))

(defun tkfont-height (self)
  (tkfinfo-linespace (^tkfinfo)))

(defun line-up (self)
  (ceiling (tkfont-height self) -2))

(defun line-down (self)
  (floor (tkfont-height self) 2))



