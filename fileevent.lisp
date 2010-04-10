;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
;;;
;;; Copyright (c) 2006 by Frank Goenninger, Germany.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; ---------------------------------------------------------------------------
;;; $Header: /project/cells/cvsroot/Celtk/fileevent.lisp,v 1.9 2006/11/04 20:53:08 ktilton Exp $
;;; ---------------------------------------------------------------------------

;;; ===========================================================================
;;; PACKAGE / EXPORTS
;;; ===========================================================================

(in-package :celtk)

(eval-when (:load-toplevel :compile-toplevel)
  (export '(tk-fileevent
	    iostream
	    read-fn
	    write-fn
	    eof-fn
	    mk-fileevent
	    stream-2-in-fd
	    stream-2-out-fd)))

;;; ===========================================================================
;;; TK-FILEEVENT MODEL
;;; ===========================================================================

(defmodel tk-fileevent (widget)
  
  ((.md-name
    :accessor id :initarg :id
    :initform (c-in nil)
    :documentation "ID of the fileevent instance.")

   (input-fd
    :accessor input-fd :initarg :input-fd
    :initform (c? (if (^iostream)
		      (stream-2-in-fd (^iostream))))
    :documentation "The input/read file descriptor - internal use only.")

   (output-fd
    :accessor output-fd
    :initarg :output-fd
    :initform (c? (if (^iostream)
	   	      (stream-2-out-fd (^iostream))))
    :documentation "The output/write file descriptor - internal use only.")

   (in-tcl-channel
    :accessor in-tcl-channel :initarg  :in-tcl-channel
    :initform (c? (fd-to-tcl-channel (^tki) (^input-fd)))
    :documentation "The TCL channel generated from the input file descriptor. - Internal use only.")

   (out-tcl-channel
    :accessor out-tcl-channel :initarg  :in-tcl-channel
    :initform (c? (fd-to-tcl-channel (^tki) (^output-fd)))
    :documentation "The TCL channel generated from the output file descriptor. - Internal use only.") 

   (in-tcl-ch-name
    :accessor in-tcl-ch-name :initarg  :in-tcl-ch-name
    :initform (c? (if (^in-tcl-channel)
	            (Tcl_GetChannelName (^in-tcl-channel))
	            nil))
    :documentation "The input TCL channel's name as passed to the fileevent command. - Internal use only.")

   (out-tcl-ch-name
    :accessor out-tcl-ch-name :initarg  :in-tcl-ch-name
    :initform (c? (if (^out-tcl-channel)
		    (Tcl_GetChannelName (^out-tcl-channel))
		    nil))
    :documentation "The output TCL channel's name as passed to the fileevent command. - Internal use only.") 

   (iostream
    :accessor iostream :initarg :iostream
    :initform (c-in nil)
    :documentation "The Lisp stream to be monitored - API: initarg,setf.")

   (readable-cb
    :accessor readable-cb :initarg :readable-cb
    :initform (c-in nil)
    :documentation "The readable callback. A dispatcher function used to call the function supplied via the read-fn slot. - Internal use only.")

   (writeable-cb
    :accessor writeable-cb :initarg :writeable-cb
    :initform (c-in nil)
    :documentation "The writeable callback. A dispatcher function used to call the function supplied via the read-fn slot. - Internal use only.")

   (eof-cb
    :accessor eof-cb :initarg :eof-cb
    :initform (c-in nil)
    :documentation "The eof callback. A dispatcher function used to call the function supplied via the eof-fn slot. - Internal use only.")

   (error-cb
    :accessor error-cb :initarg :error-cb
    :initform (c-in nil)
    :documentation "The error callback. A dispatcher function used to call the function supplied via the error-fn slot. - Internal use only.")

   (tki
    :accessor tki :initarg :tki
    :initform (c-in nil)
    :documentation "The Tcl/Tk Interpreter used. - API: initarg.")

   (opcode
    :accessor opcode :initarg :opcode
    :initform (file-event-opcode-cell-rule)
    :documentation "The opcode slot is used to control the operaion of the fileevent instance. - Internal use only.")

   (read-fn
    :accessor read-fn :initarg :read-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is ready for reading. Gets iostream as parameter. - API: initarg, setf")

   (write-fn
    :accessor write-fn :initarg :write-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is ready for writing. Gets iostream as parameter. - API: initarg, setf")

   (eof-fn
    :accessor eof-fn :initarg :eof-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is EOF. Gets iostream as parameter. - API: initarg, setf (Via default-initarg set to fn default-eof-fn which simply closes the stream).")

   (error-fn
    :accessor error-fn :initarg :error-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream has encountntered an error. Gets iostream and error sting as parameters. - API: initarg, setf (Via default-initarg set to fn default-error-fn which simply closes the stream and signals an error of class tcl-error)."))

  (:default-initargs
      :id (gensym "tk-fileevent-")
    :eof-fn 'default-eof-fn))


;;; ===========================================================================
;;; CELL RULE: FILE-EVENT/OPCODE
;;; ===========================================================================
;;;
;;; Depending on opcode call the appropriate function to handle the various
;;; cases/combinations of input-fd, output-fd, and the previously executed
;;; update operation.

(defun file-event-opcode-cell-rule ()
  "Set the opcode depending on values of input-fd, output-fd, iostream, readable-cb, writeable-cb"
  (c? (cond
       ((not (or (^input-fd) (^output-fd) .cache))
        :nop)
       
       ((and (^input-fd) (^iostream) (^readable-cb))
        :update-input-tk-fileevent)
	   
       ((and (^output-fd) (^iostream) (^writeable-cb))
        :update-output-tk-fileevent)
       
       ((not (or (^iostream) (^input-fd)))
        :reset-input-tk-fileevent)
		 
       ((not (or (^iostream) (^output-fd)))
        :reset-output-tk-fileevent)

       (t :nop))))

;;; ===========================================================================
;;; INIT-TK-FILEEVENT - CALLED UPON INITIALIZATION
;;; ===========================================================================

(defun init-tk-fileevent (tki)
  (assert tki)
  ;; Nop - all init done in observers now.
)

;;; ===========================================================================
;;; FILEEVENT HELPER METHODS AND FUCTIONS
;;; ===========================================================================

(defmethod set-tk-readable ((self tk-fileevent) ch-name path type)

;; frgo, 2006-05-26:
;; The code here was aimed at EOF checking after reading...
;; So the API needs rework...
;; STATUS: IN WORK
;;
;;   (tk-format-now " proc readable {channel path} {
;;      # check for async errors (sockets only, I think)
;;      if {[string length [set err [fconfigure $channel -error]]]} {
;;          error-cb $path $err
;; 	 close $channel
;;  	 return
;;      }
;;      # Read a line from the channel
;;      if {[catch {set line [gets $channel]} err]} {
;;          error-cb $path $err
;;  	close $channel
;;  	return
;;      }
;;      if {[string length $line]} {
;;  	received-cb $path $line
;;      }
;;      # check for eof
;;      if {[eof $channel]} {
;;          eof-cb $path
;;          close $channel
;;      }
;;  }")

;;  frgo: Old code snippet:
;;   (tk-format-now "proc readable {channel path} { if [ eof $channel ] then { eof-cb $path } else { readable-cb $path } }")         
;;   (tk-format-now "fileevent ~A readable [list readable ~A ~A]"
;; 		 ch-name
;; 		 ch-name
;; 		 path)

  (trc "tk-set-readable sees ch-name path type" ch-name path type)
  (tk-format-now
   "proc readable {channel path type} {  

      if {! [string compare $type \"socket\"]} {
        if {[string length [set err [fconfigure $channel -error]]]} {
          error-cb $path $err
 	  close $channel
  	  return
        }
      }
  
      readable-cb $path

      catch { if {[eof $channel]} {
                eof-cb $path
                close $channel
              }
            }
    }")
  
  (tk-format-now "fileevent ~A readable [list readable ~A ~A ~a]"
		 ch-name
		 ch-name
		 path
		 type)
)

(defmethod set-tk-writeable ((self tk-fileevent) ch-name path type)
  (tk-format-now "proc writeable {channel path type} { if [ eof $channel ] then  { eof-cb $path } else { writeable-cb $path } }")
  (tk-format-now "fileevent ~A writeable [list writeable ~A ~A ~a]"
		 ch-name
		 ch-name
		 path
		 type))

;;; ===========================================================================
;;; FILEEVENT CONDITIONS
;;; ===========================================================================

(define-condition tcl-fileevent-error (error)
  ())

;;; ===========================================================================
;;; OBSERVERS - USED TO SEND UPDATES TO TK LAND
;;; ===========================================================================

(defobserver opcode ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (ecase new-value
    
      ((:init-tk-fileevent)
       (init-tk-fileevent (tki self)))
    
      ((:update-input-tk-fileevent)
       (let* ((channel (in-tcl-channel self))
	      (path    (path self))
	      (ch-name (Tcl_GetChannelName channel))
	      (ch-type (Tcl_GetChannelType channel)))
	 (set-tk-readable self
			  ch-name
			  path
			  (foreign-slot-value ch-type
				     	      'Tcl_ChannelType
					      'typeName ))))

      ((:update-output-tk-fileevent)
       (let* ((channel (out-tcl-channel self))
	      (path    (path self))
	      (ch-name (Tcl_GetChannelName channel))
	      (ch-type (Tcl_GetChannelType channel)))
         (set-tk-writeable self
			   ch-name
			   path
			   (foreign-slot-value ch-type
				               'Tcl_ChannelType
					       'typeName))))

      ((:reset-input-tk-fileevent)
       ;; Do nothing
       nil)

      ((:reset-output-tk-fileevent)
       ;; Do nothing
       nil)

      ((:nop)
       ;; Do nothing
       nil))))

(defobserver in-tcl-channel ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (if (and new-value
	     (not old-value))
      (Tcl_RegisterChannel *tki* new-value))
    (if (and old-value (not new-value))
      (progn
	(tk-format-now "fileevent ~A readable {}"
		       (Tcl_GetChannelName old-value))
	(Tcl_UnregisterChannel *tki* old-value)))))

(defobserver out-tcl-channel ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (if (and new-value (not old-value))
	 (Tcl_RegisterChannel *tki* new-value))
      (if (and old-value (not new-value))
	(progn
	  (tk-format-now "fileevent ~A writeable {}"
			 (Tcl_GetChannelName old-value))
	  (Tcl_UnregisterChannel *tki* old-value)))))

(defobserver readable-cb ((self tk-fileevent))
  (if new-value
    (tcl-create-command *tki*
		       "readable-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

(defobserver writeable-cb ((self tk-fileevent))
  (if new-value
    (tcl-create-command *tki*
		       "writeable-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

(defobserver eof-cb ((self tk-fileevent))
  (if new-value
    (tcl-create-command *tki*
		       "eof-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

(defobserver error-cb ((self tk-fileevent))
  (if new-value
    (tcl-create-command *tki*
		       "error-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

;;; ===========================================================================
;;; HELPER FUNCTIONS - FILE DESCRIPTOR TO STREAM AND CHANNEL
;;; ===========================================================================

(defun fd-to-tcl-channel (interp fd)
  (assert interp)
  (if fd
      (let ((channel (Tcl_MakeFileChannel fd 6))) ;; 6 = READ/WRITE
	(if channel
	    channel
	    (error "*** Tcl error: ~a" (tcl-get-string-result interp))))))


(defun stream-2-out-fd (stream) ;; FRGO: PORTING...

  #+allegro
    (excl:stream-output-fn stream)

  #-allegro
    (error "STREAM-2-OUT-FD: Not implemented for ~A Version ~A. Sorry."
	   (lisp-implementation-type)
	   (lisp-implementation-version))
)

(defun stream-2-in-fd (stream)  ;; FRGO: PORTING...
  
  #+allegro
    (excl:stream-input-fn stream)

  #-allegro
    (error "STREAM-2-IN-FD: Not implemented for ~A Version ~A. Sorry."
	   (lisp-implementation-type)
	   (lisp-implementation-version))
)

;;; ===========================================================================
;;; CALLBACKS
;;; ===========================================================================

(defcallback readable-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignore clientData argc interp))
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^read-fn))
      (funcall fn self :read))) 
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

(defcallback writeable-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignore clientData argc interp))
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^write-fn))
      (funcall fn self :write)))
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

(defcallback eof-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignore clientData interp argc))
  (trc "EOF-CB !!!")
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^eof-fn))
	   (funcall fn self)))
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

(defcallback error-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignore clientData interp argc))
  (trc "ERROR-CB !!!")
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (err$ (foreign-string-to-lisp (mem-aref argv :pointer 2)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^error-fn))
	   (funcall fn self err$)))
  (values (foreign-enum-value 'tcl-retcode-values :tcl-error)))

;;; ===========================================================================
;;; MK-FILEEVENT: CONVENIENCE MACRO
;;; ===========================================================================

(defmacro mk-fileevent (&rest inits)
  `(make-instance 'tk-fileevent
		  :tki *tki*
		  :readable-cb (get-callback 'readable-cb)
		  :writeable-cb (get-callback 'writeable-cb)
		  :eof-cb (get-callback 'eof-cb)
		  :error-cb (get-callback 'error-cb)
		  :fm-parent *parent*
		  ,@inits))

;;; ===========================================================================
;;; A DEFAULT EOF FUNCTION, USER MAY SUPPLY ANOTHER FUNCTION WHEN MAKING THE
;;; INSTANCE OF TK-FILEEVENT
;;; ===========================================================================

(defmethod default-eof-fn ((self tk-fileevent))
  ;; Default action: close stream
  (bwhen (iostream (^iostream))
    (with-integrity (:client `(:variable ,self))
      (setf (^iostream) nil)
      (close iostream))))

;;; ===========================================================================
;;; A DEFAULT ERROR FUNCTION, USER MAY SUPPLY ANOTHER FUNCTION WHEN MAKING THE
;;; INSTANCE OF TK-FILEEVENT
;;; ===========================================================================

(defmethod default-error-fn ((self tk-fileevent) err$)
  (declare (ignorable err$))
  (trc "Heya! Error ~a ... :-(" err$)
  ;; Default action 1: close stream
  (bwhen (iostream (^iostream))
    (close iostream)
    (setf (^iostream) nil))
  ;; Default action 2: signal error
  (signal 'tcl-fileevent-error))

;;; ===========================================================================
;;; TESTING
;;; ===========================================================================
;;;
;;; With these few lines below we get a simple application with a text widget
;;; that shows data sent to a pipe in that text widget.
;;;
;;; The app does this by opening the named pipe for reading. It then waits
;;; for data on the pipe via the Tcl fileevent command. When establishing
;;; the fileevent a set of callbacks is established. The callbacks call
;;; two Lisp functions, depending on the type of channel (read or write.
;;;
;;; The callback functions look for the file channel's registered read or
;;; write functions. Those functions are set via the write-fn and read-fn
;;; methods of the tk-fileevent object.
;;;
;;; In the test example below we use the read case: the function read-from-pipe
;;; actually reads from the pipe and sends the data to the text widget by
;;; setting the text widgets model value.
;;; 
;;; In order to use this example please adapt the code below with a
;;; pipe name suitable for you (see the ^^^^^^^^ marks below).
;;; On Unixes you have to create the pipe with the mkfifo command.
;;;
;;; Have fun!
;;;
;;; Questions welcome...
;;;
;;; Frank Goenninger
;;; frgo@mac.com
;;;
;;; May 2006


;;; This is the User Supplied Read Function USRF. USRF has to take care of
;;; closing the channel if it is a file that is read from !!!
;;; The sample supplied here may serve as a template ...
(defmethod USRF ((self tk-fileevent) &optional (operation :read))
  (declare (ignorable operation))
  (let ((stream (^iostream)))
    (let ((data (read-line stream nil nil nil)))
      (trc "*** USRF: data = " data)
      (if data
	 (setf (value (fm-other :receive-window)) data)
	 (funcall (^eof-fn) self)))))

(defmodel fileevent-test-window (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
		   (mk-stack (:packing (c?pack-self))
 		     (mk-label :text "Receive window"
			       :pady 10)
		     (mk-text-widget :id :receive-window
				     ;:state 'disabled
				     :value (c-in "")
				     :height 10
				     :width 80
				     :borderwidth 2
				     :relief 'sunken
				     :pady 5))
		   (mk-fileevent :id :fileevent-test
				 :read-fn 'USRF
			         :iostream (c-in
					     (open "/Users/frgo/dribble.lisp"
;;;                           Adapt here !!!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
						  :direction :input)))))))

;;; Call this function  for testing !!
(defun test-fileevent ()
  (trc "-----------------------------------------------------------------------------")
  (test-window 'fileevent-test-window)
  (trc "-----------------------------------------------------------------------------"))

#+test
(test-window 'fileevent-test-window)