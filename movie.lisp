;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    QuicktimeTcl Interfaces

Copyright (C) 2007 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

(export! mk-movie url tk-file plug-n-play-movie)

(deftk movie (widget)
  ()
  (:tk-spec movie -url (tk-file -file)
    -controller -custombutton -highlightbackground -highlightcolor 
    -highlightthickness -height -loadcommand -loadintoram -loopstate 
    -mccommand -mcedit -palindromeloopstate -preferredrate -progressproc
    -qtprogress -qtvrqualitymotion -qtvrqualitystatic -resizable
    -swing -swingspeed -volume -width)
  (:default-initargs
      :tile? nil))

(defobserver tk-file :around ((self movie))
  (call-next-method)
  (with-cc :playmovie
    (when (plusp (length new-value)) ;; gets nil and ""
      (plug-n-play-movie self new-value nil)
      #+goodluck (app-idle-task-new 
       (let ((start (now)))
         (lambda (task app)
           (declare (ignore app))
           (when (> (- (now) start) 2)
             (with-cc 
                 (setf (tk-file self) "")
               (app-idle-task-destroy task)))))))))


(defun plug-n-play-movie (m file &optional (install? t))
  ;
  ; silly harcodes follow....
  ;
  (when install? (setf (tk-file m) file))
  #-silliness
  (tk-format `(:fini ,m) "~a play" (path m))
  ;
  ; this off-on sequence apparently necessary each time a file is loaded or sth.
  ;
  #+silliness
  (with-cc :loopstate
    (setf (palindromeloopstate m) 0)
    (with-cc :loopstate
      (setf (palindromeloopstate m) 1)
      (tk-format `(:fini ,m) "~a play" (path m)))))