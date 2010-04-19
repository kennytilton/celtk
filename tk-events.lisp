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


(defcfun ("Tcl_DoOneEvent" Tcl_DoOneEvent) :int
  (flags :int))

(defcfun ("Tcl_DoWhenIdle" tcl-do-when-idle) :void
  (tcl-idle-proc :pointer)
  (client-data :pointer))

(defcfun ("Tcl_SetResult" tcl-set-result) :void
  (interp :pointer)
  (result :string)
  (free-proc :pointer))

(defcfun ("Tcl_GetString" tcl-get-string) :string
  (tcl-obj :pointer))

(defcallback tcl-idle-proc :void ((client-data :pointer))
  (unless (c-stopped)
    (print (list :idle-proc :client-data client-data))))

;; Tk_MainLoop

(defcfun ("Tk_MainLoop" Tk_MainLoop) :void)

(defcfun ("Tk_CreateEventHandler" tk-create-event-handler) :void
  (tkwin :pointer)
  (mask :int)
  (proc :pointer)
  (client-data :pointer))

(defcenum tk-event-type ;; do not try to generate masks from these!
    "Ok for interpreting type field in event, but not for (expt 2 etype) to get mask"
  (:KeyPress 2)
  :KeyRelease
  :ButtonPress		
  :ButtonRelease		
  :MotionNotify		
  :EnterNotify		
  :LeaveNotify		
  :FocusIn			
  :FocusOut		
  :KeymapNotify		
  :Expose			
  :GraphicsExpose		
  :NoExpose		
  :VisibilityNotify	
  :CreateNotify		
  :DestroyNotify		
  :UnmapNotify		
  :MapNotify		
  :MapRequest		
  :ReparentNotify		
  :ConfigureNotify		
  :ConfigureRequest	
  :GravityNotify		
  :ResizeRequest		
  :CirculateNotify		
  :CirculateRequest	
  :PropertyNotify		
  :SelectionClear		
  :SelectionRequest	
  :SelectionNotify		
  :ColormapNotify		
  :ClientMessage		
  :MappingNotify		
  :virtualEvent
  :ActivateNotify
  :DeactivateNotify
  :MouseWheelEvent)



(defcenum tk-event-mask
    "Use to filter events when calling tk-create-event-handler"
  :NoEventMask		
  (:KeyPressMask			#.(ash 1 0))  
  (:KeyReleaseMask		#.(ash 1 1))  
  (:ButtonPressMask		#.(ash 1 2))  
  (:ButtonReleaseMask		#.(ash 1 3))  
  (:EnterWindowMask		#.(ash 1 4))  
  (:LeaveWindowMask		#.(ash 1 5))  
  (:PointerMotionMask		#.(ash 1 6))  
  (:PointerMotionHintMask	#.(ash 1 7))  
  (:Button1MotionMask		#.(ash 1 8))  
  (:Button2MotionMask		#.(ash 1 9))  
  (:Button3MotionMask		#.(ash 1 10)) 
  (:Button4MotionMask		#.(ash 1 11)) 
  (:Button5MotionMask		#.(ash 1 12)) 
  (:ButtonMotionMask		#.(ash 1 13)) 
  (:KeymapStateMask		#.(ash 1 14))
  (:ExposureMask			#.(ash 1 15)) 
  (:VisibilityChangeMask	#.(ash 1 16)) 
  (:StructureNotifyMask		#.(ash 1 17)) 
  (:ResizeRedirectMask		#.(ash 1 18)) 
  (:SubstructureNotifyMask	#.(ash 1 19)) 
  (:SubstructureRedirectMask	#.(ash 1 20)) 
  (:FocusChangeMask		#.(ash 1 21)) 
  (:PropertyChangeMask		#.(ash 1 22)) 
  (:ColormapChangeMask		#.(ash 1 23)) 
  (:OwnerGrabButtonMask		#.(ash 1 24)) 
  (:MouseWheelMask	      #.(ash 1 28))
  (:ActivateMask	            #.(ash 1 29))
  (:VirtualEventMask          #.(ash 1 30)))


(defun tk-event-type (n) ;; do not try to generate masks from these!
  (ignore-errors 
   (foreign-enum-keyword 'tk-event-type n)))

(defun tk-event-mask-symbol (n) ;; do not try to generate masks from these!
  (ignore-errors 
   (foreign-enum-keyword 'tk-event-mask n)))

(defun foreign-masks-combine (enum-type &rest mask-specs)
  (reduce 'logior (loop for mask-spec in mask-specs
                        collecting (etypecase mask-spec
                                     (number mask-spec)
                                     (keyword (foreign-enum-value enum-type mask-spec))))
    :initial-value 0))


;; sample event handler

(defcallback dump-event :void  ((client-data :pointer)(xe :pointer))
  (call-dump-event client-data xe))

(defun call-dump-event (client-data xe)
  ;;(trc "tkep> serial" (xsv serial xe))
  #+shh (loop for win being the hash-keys of (tkwins *tkw*)
        do (print `(win ,win :xwin ,(tkwin-window win) ,(tkwin-widget win) ,(path (tkwin-widget win)))))
  ;;(trc "    > same-screen" (xsv same-screen xe))

  ;;(trc "tkep> " (tk-event-type (mem-aref xe :int)) :client-data client-data)
  (case (tk-event-type (mem-aref xe :int))
    (:motionnotify
     (trc nil "motionnotify" (xsv x xe) :y (xsv y xe) :x-root (xsv x-root xe) :y-root (xsv y-root xe)))
    (:virtualevent
     (trc "    > :type" (format nil "<<~a>>" (xsv name xe)) :time (xsv time xe) :state (xsv state xe))
     (trc "    > :x" (xsv x xe) :y (xsv y xe) :x-root (xsv x-root xe) :y-root (xsv y-root xe))
     (trc "    > event/root/sub" (mapcar (lambda (w) (when w (path w)))
                                (list (xwin-widget (xsv event-window xe))
                                  (xwin-widget (xsv root-window xe))
                                  (xwin-widget (xsv sub-window xe)))))

     (trc "    > data" (unless (null-pointer-p (xsv user-data xe))
                         (tcl-get-string (xsv user-data xe)))))))

(defun xevent-dump (xe)
  (case (tk-event-type (mem-aref xe :int))
    (:motionnotify
     (trc nil "motionnotify" (xsv x xe) :y (xsv y xe) :x-root (xsv x-root xe) :y-root (xsv y-root xe)))
    (:virtualevent
     (trc "    > :type" (format nil "<<~a>>" (xsv name xe)) :time (xsv time xe) :state (xsv state xe))
     (trc "    > :x" (xsv x xe) :y (xsv y xe) :x-root (xsv x-root xe) :y-root (xsv y-root xe))
     (trc "    > event/root/sub" (mapcar (lambda (w) (when w (path w)))
                                (list (xwin-widget (xsv event-window xe))
                                  (xwin-widget (xsv root-window xe))
                                  (xwin-widget (xsv sub-window xe)))))

     (trc "    > data" (unless (null-pointer-p (xsv user-data xe))
                         (tcl-get-string (xsv user-data xe)))))
    (otherwise
     (trc "tkep> " (tk-event-type (mem-aref xe :int))))))


