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

;; Tcl/Tk

(define-foreign-library Tcl
    (:darwin (:framework "Tcl"))
  (:windows (:or "Tcl85.dll"))
  (:unix "/opt/ActiveTcl-8.5/lib/libtcl8.5.so")
  (t (:default "libtcl")))

#+test
(use-foreign-library Tk)

(define-foreign-library Tk
    (:darwin (:framework "Tk"))
  (:windows (:or "Tk85.dll"))
  (:unix "/opt/ActiveTcl-8.5/lib/libtk8.5.so") ;; also "libtk.so"
  (t (:default "libtk")))

#+nowbuiltin?
(define-foreign-library Tile
    ;(:darwin (:framework "Tk"))
    (:windows (:or "tile083.dll"))
  ;(:unix "libtk.so")
  (t (:default "libtk")))

(defctype tcl-retcode :int)

(defcenum tcl-retcode-values
  (:tcl-ok    0)
  (:tcl-error 1))
    
(defmethod translate-from-foreign (value (type (eql 'tcl-retcode)))
  (unless (eq value (foreign-enum-value 'tcl-retcode-values :tcl-ok))
    (error "Tcl error: ~a" (tcl-get-string-result *tki*)))
  value)
    
;; --- initialization ----------------------------------------

(defcfun ("Tcl_FindExecutable" tcl-find-executable) :void
  (argv0 :string))

(defcfun ("Tcl_Init" Tcl_Init) tcl-retcode
  (interp :pointer))

(defcfun ("Tk_Init" Tk_Init) tcl-retcode
  (interp :pointer))

(defcallback Tk_AppInit tcl-retcode
  ((interp :pointer))
  (unwind-protect
    (tk-app-init interp)))

(defun tk-app-init (interp)
  (assert interp)
  (trace Tcl_Init Tk_Init)
  (Tcl_Init interp)
  (Tk_Init interp)
  ;; Return OK
  (foreign-enum-value 'tcl-retcode-values :tcl-ok))

  ;; Tk_Main
    
(defcfun ("Tk_MainEx" %Tk_MainEx) :void
  (argc :int)
  (argv :string)
  (Tk_AppInitProc :pointer)
  (interp :pointer))

(defun Tk_Main ()
  (with-foreign-string (argv (argv0))
    (%Tk_MainEx 1 argv
      (get-callback 'Tk_AppInit)
      (Tcl_CreateInterp))))
    
;; Tcl_CreateInterp

(defcfun ("Tcl_CreateInterp" Tcl_CreateInterp) :pointer)

(defcfun ("Tcl_DeleteInterp" tcl-delete-interp) :void
  (interp :pointer))

;;; --- windows ----------------------------------

(defcfun ("Tk_GetNumMainWindows" tk-get-num-main-windows) :int)
(defcfun ("Tk_MainWindow" tk-main-window) :pointer (interp :pointer))

(defcfun ("Tk_NameToWindow" tk-name-to-window) :pointer
  (interp :pointer)
  (pathname :string)
  (related-tkwin :pointer))

;;; --- eval -----------------------------------------------

(defcfun ("Tcl_EvalFile" %Tcl_EvalFile) tcl-retcode
  (interp        :pointer)
  (filename-cstr :string))
   
(defun Tcl_EvalFile (interp filename)
  (with-foreign-string (filename-cstr filename)
    (%Tcl_EvalFile interp filename-cstr)))

(defcfun ("Tcl_Eval" %Tcl_Eval) tcl-retcode
  (interp      :pointer)
  (script-cstr :string))

(defun tcl-eval (i s)
  (%Tcl_Eval i s))

(defcfun ("Tcl_EvalEx" %Tcl_EvalEx) tcl-retcode
  (interp      :pointer)
  (script-cstr :string)
  (num-bytes   :int)
  (flags       :int))

(defun tcl-eval-ex (i s)
  (%Tcl_EvalEx i s -1 0))

(defcfun ("Tcl_GetVar" tcl-get-var) :string
  (interp  :pointer)
  (varName :string)
  (flags   :int))

(defcfun ("Tcl_SetVar" tcl-set-var) :string
  (interp    :pointer)
  (var-name  :string)
  (new-value :string)
  (flags     :int))

(defcfun ("Tcl_GetStringResult" tcl-get-string-result) :string
  (interp :pointer))

;; ----------------------------------------------------------------------------
;; Tcl_CreateCommand - used to implement direct callbacks
;; ----------------------------------------------------------------------------

(defcfun ("Tcl_CreateCommand" tcl-create-command) :pointer
  (interp :pointer)
  (cmdName :string)
  (proc :pointer)
  (client-data :pointer)
  (delete-proc :pointer))

;; ----------------------------------------------------------------------------
;; Tcl/Tk channel related stuff
;; ----------------------------------------------------------------------------

(defcfun ("Tcl_RegisterChannel" Tcl_RegisterChannel) :void
  (interp :pointer)
  (channel :pointer))

(defcfun ("Tcl_UnregisterChannel" Tcl_UnregisterChannel) :void
  (interp :pointer)
  (channel :pointer))

(defcfun ("Tcl_MakeFileChannel" Tcl_MakeFileChannel) :pointer
  (handle :int)
  (readOrWrite :int))

(defcfun ("Tcl_GetChannelName" Tcl_GetChannelName) :string
  (channel :pointer))

(defcfun ("Tcl_GetChannelType" Tcl_GetChannelType) :pointer
  (channel :pointer))


(defcfun ("Tcl_GetChannel" Tcl_GetChannel) :pointer
  (interp :pointer)
  (channelName :string)
  (modePtr :pointer))

;; Initialization mgmt - required to avoid multiple library loads

(defvar *initialized* nil)

(defun set-initialized ()
  (setq *initialized* t))

(defun reset-initialized ()
  (setq *initialized* nil))

#+doit
(reset-initialized)

(defun argv0 ()
  #+allegro (sys:command-line-argument 0)
  #+lispworks (nth 0 system:*line-arguments-list*) ;; portable to OS X
  #+sbcl (nth 0 sb-ext:*posix-argv*)
  #+openmcl (car ccl:*command-line-argument-list*)
  #-(or allegro lispworks sbcl openmcl)
  (error "argv0 function not implemented for this lisp"))

(defun tk-interp-init-ensure ()
  (unless *initialized*
    (use-foreign-library Tcl)
    (use-foreign-library Tk)
    ;;#-macosx (use-foreign-library Tile)
    ;;#-macosx (pushnew :tile cl-user::*features*)
    (print :try-using-togl)
    #+togl (use-foreign-library Togl)
    #+togl (print :try-using-togl-ok)
    (tcl-find-executable (argv0))
    (set-initialized)))

#+test
(load-foreign-library 'Togl)

#+test
(load "togl17.dll" :verbose t)

;; Send a script to a given Tcl/Tk interpreter

(defun eval-script (interp script)
  (assert interp)
  (assert script)
  (tcl-eval interp script))

