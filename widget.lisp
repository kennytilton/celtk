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

;;; --- widget tkwin window glue -----------------------

(defun widget-to-tkwin (self)
  (tk-name-to-window *tki* (path self) (tk-main-window *tki*)))

(defun xwin-register (self)
  (when (tkwin self)
    (let ((xwin (tkwin-window (tkwin self))))
      (unless (zerop xwin)
        (setf (gethash xwin (xwins .tkw)) self)
        xwin))))

(defun tkwin-widget (tkwin)
;;;  (assert *tkw*)
;;;  (assert (tkwins *tkw*) () "Widget hash NIL for *tkw* ~a" *tkw*)
;;;  (gethash (pointer-address tkwin) (tkwins *tkw*))
  (if (and *tkw* (tkwins *tkw*))
      (gethash (pointer-address tkwin) (tkwins *tkw*))
    (unless .stopped
      (trc "tkw issues" *tkw* (when *tkw* (tkwins *tkw*)))
      .stop
      nil)))

(defun xwin-widget (xwin) ;; assignment of xwin is deferred so...all this BS..
  (when (plusp xwin)
    (or (gethash xwin (xwins *tkw*))
      (loop for self being the hash-values of (tkwins *tkw*)
          using (hash-key tkwin)
          unless (xwin self) ;; we woulda found it by now
          do (when (eql xwin (xwin-register self))
               (return-from xwin-widget self))
          finally (trc "xwin-widget > no widget for xwin " xwin)))))

;;; --- widget -----------------------------------------

(defmodel widget (family tk-object)
  ((path :accessor path :initarg :path
     :initform (c? (eko (nil "path" self (parent-path (fm-parent self))(md-name self))
                     (format nil "~(~a.~a~)"
                       (parent-path (fm-parent self))
                       (md-name self)))))
   (tkwin :cell nil :accessor tkwin :initform nil)
   (xwin :cell nil :accessor xwin :initform nil)
   (packing :reader packing :initarg :packing :initform nil)
   (gridding :reader gridding :initarg :gridding :initform nil)
   (parent-x :reader parent-x :initarg :parent-x :initform nil)
   (parent-y :reader parent-y :initarg :parent-y :initform nil)
   (relx :reader relx :initarg :relx :initform nil)
   (rely :reader rely :initarg :rely :initform nil)
   (enabled :reader enabled :initarg :enabled :initform t)
   (event-handler :reader event-handler :initarg :event-handler :initform nil)
   (menus :reader menus :initarg :menus :initform nil
     :documentation "An assoc of an arbitrary key and the associated CLOS menu instances (not their tk ids)")
   (image-files :reader image-files :initarg :image-files :initform nil)
   (tk-selector :reader tk-selector :initarg :tk-selector
     :initform (c? (upper self tk-selector))))
  (:default-initargs
      :id (gentemp "W")
    :event-handler nil #+debug (lambda (self xe)
                                 (trc "debug event handler" self (tk-event-type (xsv type xe))))))

(eval-now!
  (export '()))

(defun tk-create-event-handler-ex (widget callback-name &rest masks)
  (let ((self-tkwin (widget-to-tkwin widget)))
    (assert (not (null-pointer-p self-tkwin)))
    (trc nil "setting up widget virtual-event handler" widget callback-name :tkwin self-tkwin :masks masks)
    (tk-create-event-handler self-tkwin
      (foreign-masks-combine 'tk-event-mask :PointerMotionMask)
      (get-callback callback-name)
      self-tkwin)
    (tk-create-event-handler self-tkwin
      (apply 'foreign-masks-combine 'tk-event-mask masks)
      (get-callback callback-name)
      self-tkwin)))

(defun widget-menu (self key)
  (or (find key (^menus) :key 'md-name)
    (break "The only menus I see are~{ ~a,~} not requested ~a" (mapcar 'md-name (^menus)) key)))

(defmacro ^widget-menu (key)
  `(widget-menu self ,key))

(defun tkwin-register (self)
  (let ((tkwin (or (tkwin self)
                  (setf (tkwin self)
                    (tk-name-to-window *tki* (^path) (tk-main-window *tki*))))))
    (if (not (zerop tkwin))
        (trc nil "got tkwin" self tkwin)
      (break "under *tki* ~a unable to get window-ptr for ~a in main ~a" *tki* (^path) (tk-main-window *tki*)))
    (setf (gethash (pointer-address tkwin) (tkwins .tkw)) self)))

(defmethod make-tk-instance ((self widget)) 
  (setf (gethash (^path) (dictionary .tkw)) self)
  (trc nil "mktki" self (^path))
  (with-integrity (:client `(:make-tk ,self))
    (when (tk-class self)
      (tk-format-now "~(~a~) ~a ~{~(~a~) ~a~^ ~}" ;; call to this GF now integrity-wrapped by caller
        (tk-class self) (path self)(tk-configurations self)))))

(defmethod tk-class :around ((self widget))
  (conc$ (when (tile? self) "TTK::") (call-next-method)))

(defmethod make-tk-instance :after ((self widget)) 
  (with-integrity (:client `(:post-make-tk ,self))
    (tkwin-register self)
    (tk-create-event-handler-ex self 'widget-event-handler-callback -1)))

(defobserver parent-x ((self widget))
  (unless (typep self 'window)
    (when new-value
      (tk-format `(:grid ,self) ;; placing is like grid for this sort
        "place ~a ~a -x ~a -y ~a" (if old-value "configure" "")
        (^path) new-value (^parent-y)))))

(defcallback widget-event-handler-callback :void  ((client-data :pointer)(xe :pointer))
  (bif (self (tkwin-widget client-data))
        (widget-event-handle self xe)
        ;; sometimes I hit the next branch restarting after crash....
        (trc nil "widget-event-handler > no widget for tkwin ~a" client-data))
  #+nahhh(handler-case
      (bif (self (tkwin-widget client-data))
        (widget-event-handle self xe)
        ;; sometimes I hit the next branch restarting after crash....
        (trc "widget-event-handler > no widget for tkwin ~a" client-data))
    (excl:simple-break (error)
      (declare (ignorable error))
      (trc "widget-event-handler-callback honoring break" error)
     (invoke-debugger error)
      )
    (t (error)
      (declare (ignorable error))
      (trc "widget-event-handler-callback ignoring error" error)
      ;;#-demo (invoke-debugger error)
      ))
;;;  #+demo
;;;  (handler-case
;;;      (bif (self (tkwin-widget client-data))
;;;        (widget-event-handle self xe)
;;;        ;; sometimes I hit the next branch restarting after crash....
;;;        (trc "widget-event-handler > no widget for tkwin ~a" client-data))
;;;    (t (error)
;;;      (declare (ignorable error))
;;;      #-demo (invoke-debugger error)
;;;      ))
;;;  #+development
;;;  (progn
;;;    
;;;    (bif (self (tkwin-widget client-data))
;;;      (widget-event-handle self xe)
;;;      ;; sometimes I hit the next branch restarting after crash....
;;;      (trc "widget-event-handler > no widget for tkwin ~a" client-data)))
  )

(export! widget-event-handle)

(defmethod widget-event-handle ((self widget) xe) ;; override for class-specific handling
  (trc nil "bingo widget-event-handle" (xevent-type xe))
  (bif (h (^event-handler)) ;; support instance-specific handlers
    (funcall h self xe)
    (case (xevent-type xe)
      (:buttonpress (trc "button pressed:" (xbe button xe)(xbe x xe)(xbe y xe)(xbe x-root xe)(xbe y-root xe)))
      (:buttonrelease (trc "button released:" (xbe button xe)(xbe x xe)(xbe y xe)(xbe x-root xe)(xbe y-root xe)))(:MotionNotify
       (xevent-dump xe))
      (:virtualevent))))

(defmethod tk-configure ((self widget) option value)
  (tk-format `(:configure ,self ,option) "~a configure ~(~a~) ~a" (path self) option (tk-send-value value)))

;;; --- commander mix-in --------------------------------

(defclass commander ()
  ()
  (:default-initargs
      :command (c? (format nil "do-on-command ~a" (^path)))))


;;; --- items -----------------------------------------------------------------------

(eval-now!
  (export '(canvas-offset ^canvas-offset coords-tweak ^coords-tweak caret-tweak ^caret-tweak
             decorations ^decorations)))

(defmodel item-geometer () ;; mix-in
  ()
  #+vestigial?
  ((canvas-offset :initarg :canvas-offset :accessor canvas-offset
     :initform (c_? (eko (nil "standard canvas offset" self (type-of self) (^p-offset))
                     (c-offset self))))
   (caret-tweak :initarg :caret-tweak :accessor caret-tweak :initform '(0 0))
   (l-bounds :initarg :l-bounds :initform nil :reader l-bounds
     :documentation "Vector of local left, top, right, bottom")
   (p-offset :initarg :p-offset :reader p-offset :initform '(0 0))
   (p-bounds :initarg :p-bounds :reader p-bounds
     :documentation "Vector of parent-relative left, top, right, bottom"
     :initform (c_? (when (and (^l-bounds)(^p-offset) )
                     (bounds-offset (^l-bounds) (^p-offset))))))
  (:documentation "For things like mx-power, which inhabit canvases but need no item for visual representation."))

(defmethod l-bounds :around (i)
  (or (call-next-method)
    (break "no l-bounds for ~a" i)))

(defmethod anchor (other)(declare (ignore other)) nil)

(defmodel item (item-geometer tk-object)
  ((id-no :cell nil :initarg :id-no :accessor id-no :initform nil)
   (l-coords :initarg :l-coords :initform nil :accessor l-coords)
   (coords-tweak :initarg :coords-tweak :initform '(0 0) :accessor coords-tweak
     :documentation "Text items need this to get positioned according to baseline")
   (coords :initarg :coords :accessor coords
     :initform nil #+old (c_? (eko (nil "final coords" self (anchor self)(^l-coords)(^canvas-offset)(^coords-tweak))
                     (loop for coord-xy = (^l-coords) then (cddr coord-xy)
                         while coord-xy
                         nconcing (mapcar '+ coord-xy (^canvas-offset) (^coords-tweak))))))
   (decorations :initarg :decorations :accessor decorations :initform nil
     :documentation "eg, For a left parens text item, the corresponding right parens text item")
   )
  (:documentation "Things you put on a canvas")
  (:default-initargs
      :id (gentemp "I")))

(defmethod make-tk-instance :around ((self item))
  (when (upper self canvas)
    (call-next-method)))

(defmethod make-tk-instance ((self item))
  (when (tk-class self)
    (with-integrity (:client `(:make-tk ,self))
      (assert (^coords) () "Item ~a missing req'd coords" self)
      (setf (id-no self) (tk-eval "~a create ~a ~{ ~a~}  ~{~(~a~) ~a~^ ~}"
                           (path (upper self canvas))
                           (down$ (tk-class self))
                           (coords self)
                           (tk-configurations self))))))

(defmethod tk-configure ((self item) option value)
  (assert (id-no self) () "cannot configure item ~a until instantiated and id obtained" self)
  (tk-format `(:configure ,self ,option)
    "~A itemconfigure ~a ~a ~a" (path .parent) (id-no self) (down$ option) (tk-send-value value)))

(defobserver coords ()
  (when (and (id-no self) new-value)
    (trc nil "coords observer setting item" self (id-no self))
    (tk-format `(:configure ,self) 
      "~a coords ~a ~{ ~a~}" (path .parent) (id-no self) new-value)))

(defmethod not-to-be :after ((self item))
  (unless (find .tkw *windows-destroyed*)
    ;(trc "whacking item" self)
    (tk-format `(:delete ,self) "~a delete ~a" (path (upper self widget)) (id-no self))))

;;; --- widget mixins ------------------------------

;;; --- tk-selector ---------------------------------------------------

(defmodel tk-selector () ;; mixin
  ((selection :initform nil :accessor selection :initarg :selection)
   (tk-variable :initform nil :accessor tk-variable :initarg :tk-variable
     :documentation "The TK node name to set as the selection changes (not the TK -variable option)"))
  (:default-initargs
      :selection (c-in nil)
      :tk-variable (c? (^path))))

(defobserver selection ((self tk-selector))
  ;
  ; handling varies on this, so we hand off to standard GF lest the PROGN
  ; method combo on slot-listener cause multiple handling
  ;
  (tk-output-selection self new-value old-value old-value-boundp))

(defmethod tk-output-selection (self new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (trc nil "selection output" self new-value)
  (when new-value
    (with-integrity (:client `(:variable ,self))
      (let ((v$ (if (stringp new-value) ;; just going slow on switching over to C API before changing tk-send-value
                    new-value
                    (tk-send-value new-value))))
        (tcl-set-var *tki* (tk-variable self) v$ (var-flags :tcl-namespace-only))))))

;;; --- menus ---------------------------------

(defun pop-up (menu x y)
  (trc nil "popping up" menu x y)
  (tk-format-now "tk_popup ~A ~A ~A" (path menu) x y))
