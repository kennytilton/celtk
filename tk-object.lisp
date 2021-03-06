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

;;; --- tk-object ------------------


(defmodel tk-object (model)
  ((.md-name :cell nil :initform (gentemp "TK") :initarg :id)
   (tk-class :cell nil :initform nil :initarg :tk-class :reader tk-class)
   
   (timers :owning t :initarg :timers :accessor timers :initform nil)
   (on-command :initarg :on-command :accessor on-command :initform nil)
   (on-key-down :initarg :on-key-down :accessor on-key-down :initform nil
     :documentation "Long story. Tcl C API weak for keypress events. This gets dispatched
eventually thanks to DEFCOMMAND")
   (on-key-up :initarg :on-key-up :accessor on-key-up :initform nil)
   (on-double-click-1 :initarg :on-double-click-1 :accessor on-double-click-1 :initform nil)
   (user-errors :initarg :user-errors :accessor user-errors :initform nil)
   
   (tile? :initform t :cell nil :reader tile? :initarg :tile?))
  (:documentation "Root class for widgets and (canvas) items"))

(export! valid? ^valid?)

(defun valid? (self)
  (not (^user-errors)))

(defmacro ^valid? ()
  '(valid? self))

(defmethod md-awaken :before ((self tk-object))
  (make-tk-instance self))

(defmethod parent-path ((self tk-object)) (path self))

;;; --- deftk --------------------

(defmacro deftk (class superclasses (&rest std-slots) &rest defclass-options)
  (destructuring-bind (&optional tk-class &rest tk-options)
      (cdr (find :tk-spec defclass-options :key 'car))
    
    (setf tk-options (tk-options-normalize tk-options))
    
    `(eval-now!
      (defmodel ,class ,(or superclasses '(tk-object))
        (,@(append std-slots (loop for (slot-name nil) in tk-options
                                 collecting `(,slot-name :initform nil
                                               :initarg ,(intern (string slot-name) :keyword)
                                               :accessor ,slot-name))))
        ,@(remove-if (lambda (k) (find k '(:default-initargs :tk-spec))) defclass-options :key 'car)
        (:default-initargs
            ,@(when tk-class `(:tk-class ',tk-class))
          ,@(cdr (find :default-initargs defclass-options :key 'car))))
      (defmethod tk-class-options append ((self ,class))
        ',tk-options)
      (export ',(loop for (slot nil) in tk-options
                    nconcing (list slot (intern (conc$ "^" slot)))))
      (defmacro ,(intern (conc$ "mk-" (symbol-name class))) (&rest inits)
        `(make-instance ',',class
           :fm-parent *parent*
           ,@inits)))))

(defun tk-options-normalize (tk-options)
  "normalize '(-aaa (tk-bbb -bbb)) => '((aaa -aaa)(tk-bbb -bbb))"
  (loop for tk-option-def in tk-options
      for slot-name = (intern (de- (if (atom tk-option-def)
                                       tk-option-def (car tk-option-def))))
      collecting (list slot-name (if (atom tk-option-def)
                                     tk-option-def (cadr tk-option-def)))))

(eval-now!
  (defun de- (sym)
    (remove #\- (symbol-name sym) :end 1)))
  
(defgeneric tk-class-options (self)
  (:method-combination append)
  (:method :around (self)
    (or (get (type-of self) 'tk-class-options)
             (setf (get (type-of self) 'tk-class-options)
               (loop with all = (remove-duplicates (call-next-method) :key 'second)
                     for old in (when (tile? self)
                                  (case (type-of self)
                                    (label '(pady padx height indicatoron relief tk-label))
                                    (otherwise '(pady padx #+hmmm height indicatoron relief tk-label))));;
                     do (setf all (delete old all :key 'car))
                     finally (return all))))))

(defun tk-config-option (self slot-name)
  (second (assoc slot-name (tk-class-options self))))

(defmethod slot-value-observe progn (slot-name (self tk-object) new-value old-value old-value-boundp cell)
  (declare (ignorable old-value cell))
  (when old-value-boundp ;; initial propagation to Tk happens during make-tk-instance
    (bwhen (tco (tk-config-option self slot-name)) ;; (get slot-name 'tk-config-option))
      (tk-configure self (string tco) (or new-value "")))))

(defun tk-configurations (self)
  (loop with configs
      for (slot-name tk-option) in (tk-class-options self)
      when tk-option
      do (bwhen (slot-value (funcall slot-name self)) ;; must go thru accessor with Cells, not 'slot-value
           (setf configs (nconc (list tk-option (tk-send-value slot-value)) configs)))
      finally (return configs)))

