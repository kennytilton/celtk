

(in-package :celtk)

(eval-when (compile load)
  (use-package :gl)
  (use-package :glu))

(defun nehe-02 () ;; ACL project manager needs a zero-argument function, in project package
  (test-window 'nehe-02-demo))

(defmodel nehe-02-demo (window)
  ()
  (:default-initargs
      :title$ "Nehe Tutorial 2"
    :kids (c? (the-kids
               (mk-stack (:packing (c?pack-self "-side left -fill both"))
                 (make-instance 'nehe-02
                   :fm-parent *parent*
                   :width 640 :height 480
                   :double 1))))))

(defmodel nehe-02 (togl)
  ((width :initarg :wdith :initform 640 :accessor width)
   (height :initarg :wdith :initform 480 :accessor height)))

(defmethod togl-create-using-class ((self nehe-02))
  (trc "create")
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0.5)
  (gl:clear-depth 1)
  ; This must be commented otherwise the nehe2 shapes disappear
  (gl:enable :depth-test)
  (gl:depth-func :equal)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:matrix-mode :modelview))

(defmethod togl-timer-using-class ((self nehe-02))
  (trc nil "enter gear timer" self (togl-ptr self) (get-internal-real-time))
  (togl-post-redisplay (togl-ptr self))
  )

(defmethod togl-reshape-using-class ((self nehe-02))
  (let ((width (togl-width (togl-ptr self)))
        (height (togl-height (togl-ptr self))))
    (trc "reshape w,h" width height)
    (if (zerop height)
        (setq height 1))
    (gl:viewport 0 0 width height                )
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 45 (/ width height) 0.1 100)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defmethod togl-display-using-class ((w nehe-02))
  (trc "display")
  (gl:clear #x4000 :DEPTH-BUFFER-BIT)
  (gl:load-identity)
  (gl:clear-color 0 1 0 0)
  (gl:clear #x4000 :DEPTH-BUFFER-BIT)
  ;(gl:translate -1.5 0.0 -6)
  (draw-triangle)
  ;(gl:translate 3.0f0 0.0f0 0.0f0)
  ;(draw-square)
  (gl:flush)
  )

(defmethod draw-triangle ()
  (gl:with-primitives :triangles
    (gl:vertex   0.0f0  1.0f0 0.0f0)
    (gl:vertex  -1.0f0 -1.0f0 0.0f0)
    (gl:vertex   1.0f0 -1.0f0 0.0f0)))

(defmethod draw-square ()
  (gl:with-primitives :quads
    (gl:vertex -1.0f0  1.0f0  0.0f0); Top Left
    (gl:vertex  1.0f0  1.0f0  0.0f0); Top Right
    (gl:vertex  1.0f0 -1.0f0  0.0f0); Bottom Right
    (gl:vertex -1.0f0 -1.0f0  0.0f0))) ; Bottom Left

