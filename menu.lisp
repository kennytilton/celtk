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

#| do list

tear-off
dynamic add/remove

|#

;;; --- menu bars -----------------------------------

(defmodel menubar (menu)())
(defun mk-menubar (&rest inits)
  (apply 'make-instance 'menubar
    :fm-parent *parent*
    inits))

(defmethod make-tk-instance ((self menubar))
  (tk-format `(:make-tk ,self) "menu ~a -tearoff 0 -type menubar ~{~(~a~) ~a~^ ~}" (^path) (tk-configurations self))

;;;  (let ((opts (tk-class-options self))
;;;        (figs (tk-configurations self)))
;;;    (trc (background self) " menu-figs!!!!!!!!!!!!" figs :opts opts)
;;;    (tk-format `(:make-tk ,self) "menu ~a ~{~(~a~) ~a~^ ~}" ;; call to this GF now integrity-wrapped by caller
;;;      (path self) figs))
  (tk-format `(:configure ,self) ". configure -menu ~a" (^path)))

;;; --- menus -------------------------------------------

(deftk menu (widget)
  ((label :initarg :label :initform nil :accessor label))
  (:tk-spec menu -activebackground -activeborderwidth -activeforeground -background
    -borderwidth -cursor -disabledforeground (tkfont -font)
    -foreground -relief -takefocus
    -postcommand -selectcolor -tearoff -tearoffcommand
    (-title nil) (-tk-type -type))
  (:default-initargs
      :id (gentemp "MNU")))


(defmethod make-tk-instance ((self menu))
  (trc nil "maketkinstance menu" self :parent .parent (type-of .parent)
    :grandpar (fm-parent .parent) (type-of (fm-parent .parent)))
  (tk-format `(:make-tk ,self) "menu ~a -tearoff 0" (^path)))

(defmacro mk-menu-ex (&rest submenus)
  `(mk-menu :kids (c? (the-kids ,@submenus))))

(defmethod make-tk-instance :after ((self menu))
  (trc nil "make-tk-instance > traversing menu" self)
  (fm-menu-traverse self
    (lambda (entry &aux (menu self))
      (assert (typep entry 'menu-entry))
      (trc nil "make-tk-instance visiting menu entry" (path menu) entry)
      (tk-format `(:post-make-tk ,self) "~(~a~) add ~(~a~) ~{~(~a~) ~a~^ ~}"
        (path menu)
        (tk-class entry)
        (tk-configurations entry)))))

;;; --- menu entries ------------------------------------
;;;   these get created a lot diff than widgets and items, and the path is
;;;   specified diff, so we start a new object hierarchy for them
;;;

(defmodel menu-entry (tk-object)
  ((idx :cell nil :initarg :idx :accessor idx :initform nil))
  (:documentation "e.g, New, Open, Save in a File menu"))

(defmethod idx :around ((self menu-entry))
  (or (call-next-method)
    (setf (idx self)
      (block count-to-self
        (let ((i -1)
              (menu (upper self menu)))
          (fm-menu-traverse menu
            (lambda (entry)
              (assert (typep entry 'menu-entry))
              (incf i)
              (when (eq entry self)
                (return-from count-to-self i)))))))))

(defmethod make-tk-instance ((self menu-entry))
  "Parent has to do this to get them in the right order"
  (setf (gethash (path-idx self) (dictionary .tkw)) self))

(defmethod parent-path ((self menu-entry))
  (path .parent))

(defmethod path-idx ((self menu-entry))
  "This method hopefully gets used only internally and not given to Tcl qua thing name, which will not recognize it"
  (assert (idx self))
  (format nil "~a.~a" (path (upper self menu))(idx self)))

(defun fm-menu-traverse (family fn)
  "Traverse family arbitrarily deep as need to reach all menu-entries
without recursively penetrating nested menu (in which case menu-entries
encountered would belong to that menu, versus the one on which fm-menu-traverse
was implicitly invoked (which is why menu is not passed to callback fn))."
  (loop for k in (kids family)
      do (typecase k
           (menu-entry (funcall fn k))
           (menu (c-break "not stopped at cascade?"))
           (family (fm-menu-traverse k fn)))))


(defmethod not-to-be :after ((self menu-entry))
  (unless (find .tkw *windows-destroyed*)
    (trc "whacking menu-entry" (path-idx self)(path (upper self menu))(idx self))
    ;(trace tk-format)
    #+dissed(tk-format `(:destroy ,self) "~a delete ~a" (path (upper self menu)) (idx self))))

(defmethod tk-configure ((self menu-entry) option value)
  (assert (>= (idx self) 0) () "cannot configure menu-entry ~a until instantiated and index decided" self)
  (tk-format `(:configure ,self) "~A entryconfigure ~a ~(~a~) ~a"
    (path (upper self menu)) (idx self) option (tk-send-value value)))

(deftk menu-entry-separator (menu-entry)
  ()
  (:tk-spec separator -columnbreak))

(deftk menu-entry-usable (menu-entry)
  ()
  (:tk-spec menu -activebackground -activeforeground -accelerator -background
    -bitmap -columnbreak -command
    -compound (tkfont -font) -foreground -hidemargin
    -image -label -state -underline))

(defobserver accelerator :around ((self menu-entry-usable))
  (call-next-method)
  (with-integrity (:client '(:bind nil))
    (when new-value
      (tk-format-now "bind . <~a> {~a invoke ~a}" new-value (path (upper self menu)) (idx self)))))


(deftk menu-entry-cascade (tk-selector family menu-entry-usable)
  ()
  (:tk-spec cascade
    -menu)
  (:default-initargs
      :menu (c? (path (kid1 self)))))

(defmacro mk-menu-entry-cascade-ex ((&rest initargs) &rest submenus)
  `(mk-menu-entry-cascade
    ,@initargs
    :kids (c? (the-kids (mk-menu :kids (c? (the-kids ,@submenus)))))))

(defmethod path ((self menu-entry-cascade))
  (format nil "~(~a.~a~)" (path .parent) (md-name self)))

(defmethod tk-output-selection ((self menu-entry-cascade) new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (when (and new-value #+not (not old-value-boundp))
    (tk-format `(:selection ,self)
      (if (listp new-value) "set ~(~a~) {~{~a~^ ~}}" "set ~(~a~) ~s")
      (^path) new-value)))

(deftk menu-entry-command (menu-entry-usable)
  ()
  (:tk-spec command -command)
  (:default-initargs
      :command (c? (format nil "do-on-command ~a" (path-idx self)))))

(defmacro mk-menu-entry-command-ex ((&rest menu-command-initargs) lbl callback-body)
  `(mk-menu-entry-command
    ,@menu-command-initargs
    :label ,lbl
    :on-command (lambda (self)
                  (declare (ignorable self))
                  ,callback-body)))

(deftk menu-entry-button (menu-entry-command)
  ()
  (:tk-spec command
    (tk-variable nil) -selectcolor -selectimage -indicatoron))

; --- menu check button -----------------------------------

(deftk menu-entry-checkbutton (menu-entry-command)
  ()
  (:tk-spec checkbutton
    (tk-variable -variable)
    -offvalue
    -onvalue)
  (:default-initargs
    :value (c-in nil)
    :tk-variable (c? (format nil "~a.~(~a~)" (path .parent)(md-name self)))
    :on-command  (lambda (self)
                   (setf (^value) (not (^value))))))

(defobserver .value ((self menu-entry-checkbutton))
  (trc nil "defobserver  value menu-entry-checkbutton" self new-value old-value-boundp)
  (when (and new-value (not old-value-boundp))
    (tk-format `(:variable ,self) "set ~a ~a" (^tk-variable) (if new-value 1 0))))

; --- menu radio button -----------------------------------

(deftk menu-entry-radiobutton (menu-entry-command)
  ()
  (:tk-spec radiobutton
    (tk-variable -variable)
    -value)
  (:default-initargs
    :tk-variable (c? (down$ (path (upper self tk-selector))))
    :on-command  (lambda (self)
                   (declare (ignore key args))
                   (trc "menu radio button command firing" self (^value) (upper self tk-selector))
                   (setf (selection (upper self tk-selector)) (^value)))))

(defmodel menu-radio-group (tk-selector family)
  ((.md-name :cell nil :initform (gentemp "RG") :initarg :id))
  (:documentation "Sits in Celtk menu tree managing radio buttons but has no Tk correlate"))

(defmethod path ((self menu-radio-group))
  (format nil "~(~a.~a~)" (path .parent) (md-name self)))

(defun mk-menu-radio-group (&rest inits)
  (apply 'make-instance 'menu-radio-group
    :fm-parent *parent*
    inits))

(defmethod parent-path ((self menu-radio-group))
  (path .parent))

(defmethod tk-output-selection ((self menu-radio-group) new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (trc nil "selection output for radio group" self new-value old-value old-value-boundp (^path))
  (unless old-value-boundp ;; just needed for initialization; Tk manages variable afterwards
    (tk-format `(:variable ,self) "set ~(~a~) ~a" (^path) (tk-send-value new-value))))

(deftk menubutton (widget)
  ((menu-values :initarg :menu-values :accessor menu-values :initform nil))
  (:tk-spec menubutton -activebackground -activeforeground -anchor -background
    -bitmap -borderwidth -cursor -disabledforeground
    (tkfont -font) -foreground -highlightbackground -highlightcolor
    -highlightthickness -image (tk-justify -justify) -padx
    -pady -relief -takefocus -text
    -textvariable -underline -wraplength
    -compound -direction -height -indicatoron
    (-tk-menu -menu) -state -width))

(defmethod make-tk-instance ((self menubutton)) 
  (setf (gethash (^path) (dictionary .tkw)) self)
  (when (tk-class self)
    (tk-format `(:make-tk ,self) "~(~a~) ~a ~{~(~a~) ~a~^ ~}"
      (tk-class self) (path self)(tk-configurations self)) :stdfctry))

(deftk popup-menubutton (tk-selector menubutton)
  ((initial-value :initarg :initial-value :initform nil :accessor initial-value)
   (entry-values :initarg :entry-values :initform nil :accessor entry-values))
  (:tk-spec menubutton)
  (:default-initargs
      :tk-menu (c? (path (kid1 self)))
    ;;:text (c? (tk-send-value (or (^selection) "unselected")))
    :textvariable (c? (^path))
    :relief 'raised
    :indicatoron 1
    :kids (c? (the-kids
               (mk-menu
                :kids (c? (the-kids ;; don't worry, this flattens
                           (loop for v in (entry-values .parent)
                               collecting
                                 (mk-menu-entry-radiobutton
                                    :label (down$ v)
                                    :value v)))))))))

(defobserver initial-value ((self popup-menubutton))
  (when new-value
    (with-integrity (:change self)
      (setf (selection self) new-value))))

(defmethod tk-output-selection ((self popup-menubutton) new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (when new-value
    (with-integrity (:client `(:selection ,self))
      (tk-format-now
       (if (listp new-value) "set ~(~a~) {~{~a~^ ~}}" "set ~(~a~) ~s")
       (^path) new-value))))
