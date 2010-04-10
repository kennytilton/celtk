(in-package :celtk)

(defmodel expander (frame-stack)
  ((label :initarg :label :accessor label :initform (c-in nil))
   (expansion :initarg :expansion :accessor expansion :initform nil)
   (expanded :initarg :expanded :accessor expanded :initform (c-in nil)))
  (:default-initargs
    :fm-parent (error "expander widget must have some kind of parent")))

(defmacro mk-expander ((&rest inits) &body body)
  `(make-instance 'expander
     ,@inits
     :fm-parent *parent*
     :expansion (c? (the-kids ,@body))
     :expanded (c-in t)
     :kids (c? (the-kids
                (mk-button-ex ((^label) (setf (expanded (upper self expander))
                                          (not (expanded (upper self expander))))))
                (^expansion)))
     :kids-packing (c? (when (^kids)
                         (if (^expanded)
                             (format nil "pack~{ ~a~} -side top -anchor nw  -padx ~a -pady ~a"
                               (mapcar 'path (^kids))
                               (^padx) (^pady))
                           (format nil "pack forget~{ ~a~}"
                             (mapcar 'path (cdr (^kids)))))))))


(defmodel expander-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
		 (mk-stack (:packing (c?pack-self))
		   (mk-expander (:label "hi")
		     (mk-label :text "hi")
		     (mk-label :text "ho")))))))

(defun test-andy-expander ()
  (test-window 'expander-test))
