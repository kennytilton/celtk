;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; gears.lisp --- Celtk/Togl version of cl-opengl Lisp version of gears.c (GLUT Mesa demos).
;;;
;;; Simple program with rotating 3-D gear wheels.

(defpackage :gears
  (:use :common-lisp :utils-kt :cells :celtk))

(in-package :gears)

(defvar *startx*)
(defvar *starty*)
(defvar *xangle0*)
(defvar *yangle0*)
(defvar *xangle*)
(defvar *yangle*)

(defparameter *vTime* 100)

(defun gears () ;; ACL project manager needs a zero-argument function, in project package
  (let ((*startx* nil)
        (*starty* nil)
        (*xangle0* nil)
        (*yangle0* nil)
        (*xangle* 0.2)
        (*yangle* 0.0))
    (test-window 'gears-demo)))

(defmodel gears-demo (window)
  ((gear-ct :initform (c-in 1) :accessor gear-ct :initarg :gear-ct)
   (scale :initform (c-in 1) :accessor scale :initarg :scale))
  (:default-initargs
      :title$ "Rotating Gear Widget Test"
    :kids (c? (the-kids
               (mk-stack (:packing (c?pack-self "-side left -fill both"))
                 (mk-label :text "Click and drag to rotate image")
                 (mk-row ()
                   (mk-label :text "Spin delay (ms):")
                   (mk-entry :id :vtime
                     :value (c-in "100"))
                   (mk-button-ex (" Quit " (tk-eval "destroy ."))))
                 (make-instance 'gears
                   :fm-parent *parent*
                   :width 400 :height 400
                   :timer-interval (c? (let ((n$ (value (fm-other :vtime))))
                                         (format nil "~a" (max 1 (or (parse-integer n$ :junk-allowed t) 0)))))
                   :double 1 ;; "yes"
                   :event-handler (c? (lambda (self xe)
                                        (trc nil "togl event" (tk-event-type (xsv type xe)))
                                        (case (tk-event-type (xsv type xe))
                                          (:virtualevent
                                           (trc nil "canvas virtual" (xsv name xe)))
                                          (:buttonpress
                                           #+not (RotStart self (xsv x xe) (xsv y xe))
                                           (RotStart self (xsv x-root xe) (xsv y-root xe)))
                                          (:motionnotify
                                           #+not (RotMove self (xsv x xe) (xsv y xe))
                                           (RotMove self (xsv x-root xe) (xsv y-root xe)))
                                          (:buttonrelease
                                           (setf *startx* nil)))))))))))

(defun RotStart (self x y)
  (setf *startx* x)
  (setf *starty* y)
  (setf *xangle0* (rotx self))
  (setf *yangle0* (roty self)))

(defun RotMove (self x y)
  (when *startx*
    (trc nil "rotmove started" x *startx* *xangle0*)
    (setf *xangle* (+ *xangle0* (- x *startx*)))
    (setf *yangle* (+ *yangle0* (- y *starty*)))
    (setf (rotx self) *xangle*)
    (setf (roty self) *yangle*)
    (togl-post-redisplay (togl-ptr self))))

(defconstant +pif+ (coerce pi 'single-float))

(defmodel gears (togl)
  ((rotx :initform (c-in 40) :accessor rotx :initarg :rotx)
   (roty :initform (c-in 25) :accessor roty :initarg :roty)
   (rotz :initform (c-in 10) :accessor rotz :initarg :rotz)
   (gear1 :initarg :gear1 :accessor gear1
     :initform (c_? (trc nil "making list!!!!! 1")
                 (let ((dl (gl:gen-lists 1)))
                   (gl:with-new-list (dl :compile)
                     (gl:material :front :ambient-and-diffuse #(0.8 0.1 0.0 1.0))
                     (draw-gear 1.0 4.0 1.0 20 0.7))
                   dl)))
   (gear2 :initarg :gear2 :accessor gear2
     :initform (c_? (let ((dl (gl:gen-lists 1)))
                      (gl:with-new-list (dl :compile)
                        (gl:material :front :ambient-and-diffuse #(0.0 0.8 0.2 1.0))
                        (draw-gear 0.5 2.0 2.0 10 0.7))
                      dl)))
   (gear3 :initarg :gear3 :accessor gear3
     :initform (c_? (let ((dl (gl:gen-lists 1)))
                      (gl:with-new-list (dl :compile)
                        (gl:material :front :ambient-and-diffuse #(0.2 0.2 1.0 1.0))
                        (draw-gear 1.3 2.0 0.5 10 0.7))
                      dl)))

   (angle :initform (c-in 0.0) :accessor angle :initarg :angle)
   (frame-count :cell nil :initform 0 :accessor frame-count)
   (t0 :cell nil :initform 0 :accessor t0)
   ;
   (width :initarg :wdith :initform 400 :accessor width)
   (height :initarg :wdith :initform 400 :accessor height)))

(defmethod togl-timer-using-class ((self gears))
  (trc nil "enter gear timer" self (togl-ptr self) (get-internal-real-time))
  (incf (^angle) 5.0)
  (togl-post-redisplay (togl-ptr self))
  ;(loop until (zerop (ctk::Tcl_DoOneEvent 2)))
  )

(defmethod togl-create-using-class ((self gears))
  (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
  (gl:enable :cull-face :lighting :light0 :depth-test)
  (gl:material :front :ambient-and-diffuse #(0.8 0.1 0.0 1.0))
  (gl:enable :normalize)
  (truc self))

(defmethod togl-reshape-using-class ((self gears)) 
  (trc nil "reshape")
  (truc self t)
  )

(defun truc (self &optional truly)
  (let ((width (Togl-width (togl-ptr self)))
        (height (Togl-height (togl-ptr self))))
    (trc nil "enter gear reshape" self width (width self))
    (gl:viewport 0 (- height (height self)) (width self) (height self))
    (unless truly
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (let ((h (/ height width)))
        (gl:frustum -1 1 (- h) h 5 60)))
    (progn
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:translate 0 0 -30))))


(defmethod togl-display-using-class ((self gears) &aux (scale (scale (upper self gears-demo))))
  (declare (ignorable scale))
  (trc nil "display angle" (^rotx)(^roty)(^rotz))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (gl:with-pushed-matrix
      (gl:rotate (^rotx) 1 0 0)
    (gl:rotate (^roty) 0 1 0)
    (gl:rotate (^rotz) 0 0 1)
  
    (gl:with-pushed-matrix
        (gl:translate -3 -2 0)
      (gl:rotate (^angle) 0 0 1)
      (gl:call-list (^gear1)))
    
    (gl:with-pushed-matrix
        (gl:translate 3.1 -2 0)
      (gl:rotate (- (* -2 (^angle)) 9) 0 0 1)
      (gl:call-list (^gear2)))
    
    (gl:with-pushed-matrix ; gear3
        (gl:translate -3.1 4.2 0.0)
      (gl:rotate (- (* -2 (^angle)) 25) 0 0 1)
      (gl:call-list (^gear3))))
  
  (Togl-Swap-Buffers (togl-ptr self))
  
  #+shhh (print-frame-rate self))

(defun draw-gear (inner-radius outer-radius width n-teeth tooth-depth)
  "Draw a gear."
  (declare (single-float inner-radius outer-radius width tooth-depth)
           (fixnum n-teeth))
  (let ((r0 inner-radius)
        (r1 (- outer-radius (/ tooth-depth 2.0)))
        (r2 (+ outer-radius (/ tooth-depth 2.0)))
        (da (/ (* 2.0 +pif+) n-teeth 4.0)))
    (gl:shade-model :flat)
    (gl:normal 0 0 1)
    ;; Draw front face.
    (gl:with-primitives :quad-strip 
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    ;; Draw front sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    (gl:normal 0 0 -1)
    ;; Draw back face.
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))))) 
    ;; Draw back sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* (- width) 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5)))))
    ;; Draw outward faces of teeth.
    (gl:with-primitives :quad-strip
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
          (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
                 (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
                 (len (sqrt (+ (* u u) (* v v)))))
            (setq u (/ u len))
            (setq v (/ u len))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0)
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* (- width) 0.5))
            (setq u (- (* r1 (cos (+ angle (* 3 da))))
                       (* r2 (cos (+ angle (* 2 da))))))
            (setq v (- (* r1 (sin (+ angle (* 3 da))))
                       (* r2 (sin (+ angle (* 2 da))))))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width 0.5))
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0))))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* (- width) 0.5)))
    ;; Draw inside radius cylinder.
    (gl:shade-model :smooth)
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:normal (- (cos angle)) (- (sin angle)) 0.0)
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5)))))))

(defun print-frame-rate (window)
  (with-slots (frame-count t0) window
    (incf frame-count)
    (let ((time (get-internal-real-time)))
      (when (= t0 0)
        (setq t0 time))
      (when (>= (- time t0) (* 5 internal-time-units-per-second))
        (let* ((seconds (/ (- time t0) internal-time-units-per-second))
               (fps (/ frame-count seconds)))
          (declare (ignorable fps))
          #+shh (format *terminal-io* "~D frames in ~3,1F seconds = ~6,3F FPS~%"
                  frame-count seconds fps))
        (setq t0 time)
        (setq frame-count 0)))))
