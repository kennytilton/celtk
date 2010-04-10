(defpackage "NEHE-CL-GL"
  (:use :common-lisp :utils-kt :cells :celtk)
  (:export #:nehe1
           #:nehe2))

(in-package :nehe-cl-gl)

(defclass first-window (window) ())
 
(defmethod initialize-instance :after ((w first-window) &key)
  (print 'first-window)
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1)
; This must be commented otherwise the nehe2 shapes disappear
;  (gl:enable :depth-test)
  (gl:depth-func :equal)
  (gl:hint :perspective-correction-hint :nicest))

(defmethod glut:display ((w first-window))
(defmethod togl-display-using-class ((self nehe-1))
  (print 'first-window-display)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity))

(defmethod glut:reshape ((w first-window) width height)
  (print 'first-window-reshape)
  (if (zerop height)
      (setq height 1))
  (gl:viewport 0 0 width height)                
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45 (/ width height) 0.1 100)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun nehe1 ()
  (unwind-protect
       (progn
         (glut:init-display-mode :single :rgb)
         (make-instance 'first-window
                        :pos-x 100 :pos-y 100
                        :width 640 :height 480
                        :title "NEHE 1"
                        :events '(:display :reshape))
         (glut:main-loop))))

;; ---------------------------------------

(in-package :nehe-cl-gl)

(defclass first-polygon (first-window) ())

(defmethod initialize-instance :after ((w first-polygon) &key)
  (print 'first-polygon))

(defmethod draw-triangle ((w first-polygon))
  (gl:with-primitives :triangles
    (gl:vertex   0.0f0  1.0f0 0.0f0)
    (gl:vertex  -1.0f0 -1.0f0 0.0f0)
    (gl:vertex   1.0f0 -1.0f0 0.0f0)))

(defmethod draw-square ((w first-polygon))
  (gl:with-primitives :quads
    (gl:vertex -1.0f0  1.0f0  0.0f0); Top Left
    (gl:vertex  1.0f0  1.0f0  0.0f0); Top Right
    (gl:vertex  1.0f0 -1.0f0  0.0f0); Bottom Right
    (gl:vertex -1.0f0 -1.0f0  0.0f0))) ; Bottom Left

(defmethod glut:display ((w first-polygon))
  (call-next-method)
  (print 'first-polygon-display)
  (gl:translate -1.5 0.0 -6)
  (draw-triangle w)
  (gl:translate 3.0f0 0.0f0 0.0f0)
  (draw-square w)
  (gl:flush))

(defun nehe2 ()
  (unwind-protect
       (progn
         (glut:init-display-mode :single :rgb)
         (make-instance 'first-polygon
                        :pos-x 100 :pos-y 100
                        :width 640 :height 480
                        :title "NEHE 2"
                        :events '(:display :reshape))
         (glut:main-loop))))