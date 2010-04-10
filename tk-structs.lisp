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


(defctype Window :unsigned-long) ;; <sigh> The XWindow pointer stored in the tkwin record
(defctype Time :unsigned-long)
(defctype Tk_Uid :string)

(defcstruct tk-fake-win
    "Used by macros to peek at tkwins (why use a fake window definition?)"
  (display :pointer)
  (dummy1 :pointer)
  (screen-num :int)
  (visual :pointer)
  (depth :int)
  (window Window)
  (dummy2 :pointer)
  (dummy3 :pointer)
  (parent-ptr Window)
  (dummy4 :pointer)
  (dummy5 :pointer)
  (pathname :string)
  ;;;    Tk_Uid nameUid;
  ;;;    Tk_Uid classUid;
  ;;;    XWindowChanges changes;
  ;;;    unsigned int dummy6;	/* dirtyChanges */
  ;;;    XSetWindowAttributes atts;
  ;;;    unsigned long dummy7;	/* dirtyAtts */
  ;;;    unsigned int flags;
  ;;;    char *dummy8;		/* handlerList */
  ;;;#ifdef TK_USE_INPUT_METHODS
  ;;;    XIC dummy9;			/* inputContext */
  ;;;#endif /* TK_USE_INPUT_METHODS */
  ;;;    ClientData *dummy10;	/* tagPtr */
  ;;;    int dummy11;		/* numTags */
  ;;;    int dummy12;		/* optionLevel */
  ;;;    char *dummy13;		/* selHandlerList */
  ;;;    char *dummy14;		/* geomMgrPtr */
  ;;;    ClientData dummy15;		/* geomData */
  ;;;    int reqWidth, reqHeight;
  ;;;    int internalBorderLeft;
  ;;;    char *dummy16;		/* wmInfoPtr */
  ;;;    char *dummy17;		/* classProcPtr */
  ;;;    ClientData dummy18;		/* instanceData */
  ;;;    char *dummy19;		/* privatePtr */
  ;;;    int internalBorderRight;
  ;;;    int internalBorderTop;
  ;;;    int internalBorderBottom;
  ;;;    int minReqWidth;
  ;;;    int minReqHeight;
  )

(defun tkwin-pathname (tkwin)
  (foreign-slot-value tkwin 'tk-fake-win 'pathname))

(defun tkwin-window (tkwin)
  "Get the (different!) XWindow pointer from the tkwin data structure.
Note that the Xwindow structure is not allocated straight away, not until
(I guess) the XWindow server has gotten involved with the widget."
  (foreign-slot-value tkwin 'tk-fake-win 'window))

#|
typedef struct {
    int type;
    unsigned long serial;   /* # of last request processed by server */
    Bool send_event;	    /* True if this came from a SendEvent request */
    Display *display;	    /* Display the event was read from */
    Window event;	    /* Window on which event was requested. */
    Window root;	    /* root window that the event occured on */
    Window subwindow;	    /* child window */
    Time time;		    /* milliseconds */
    int x, y;		    /* pointer x, y coordinates in event window */
    int x_root, y_root;	    /* coordinates relative to root */
    unsigned int state;	    /* key or button mask */
    Tk_Uid name;	    /* Name of virtual event. */
    Bool same_screen;	    /* same screen flag */
    Tcl_Obj *user_data;     /* application-specific data reference; Tk will
			     * decrement the reference count *once* when it
			     * has finished processing the event. */
} XVirtualEvent;
|#

(defcstruct x-virtual-event
    "common event fields"
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (event-window Window)
  (root-window Window)
  (sub-window Window)
  (time Time)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :unsigned-int)
  (name :string)
  (same-screen :boolean)
  (user-data :pointer)
  )

(defmacro xsv (slot-name xptr)
  `(foreign-slot-value ,xptr 'x-virtual-event ',slot-name))

(defun myx (xe)
  (xsv x xe))
(defmacro xke (slot-name xptr)
  `(foreign-slot-value ,xptr 'x-key-event ',slot-name))

(export! xevent-type)
(defun xevent-type (xe)
  (tk-event-type (xsv type xe)))

;; -------------------------------------------

(defcstruct x-key-event
   "X key Event"
  (xke-header x-virtual-event)
  (trans-char-0 :char)
  (trans-char-1 :char)
  (trans-char-2 :char)
  (trans-char-3 :char))

(defcstruct x-button-event
    "common event fields"
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (event-window Window)
  (root-window Window)
  (sub-window Window)
  (time Time)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :unsigned-int)
  (button :unsigned-int)
  (same-screen :boolean))

(defmacro xbe (slot-name xptr)
  `(foreign-slot-value ,xptr 'x-button-event ',slot-name))

(defun xbe-x (xbe) (xbe x xbe))
(defun xbe-y (xbe) (xbe y xbe))
(defun xbe-button (xbe) (xbe button xbe))
(export! xbe-x xbe-y xbe-button xbe)

;; --------------------------------------------

(defcenum tcl-event-flag-values
    (:tcl-dont-wait         2)
  (:tcl-window-events     4)
  (:tcl-file-events       8)
  (:tcl-timer-events     16)
  (:tcl-idle-events      32)
  (:tcl-all-events       -3))

(defcenum tcl-variable-related-flag
    "flags passed to getvar, setvar, tracevar, etc"
  (:tcl-global-only      1)
  (:tcl-namespace-only	 2)
  (:tcl-append-value	 4)
  (:tcl-list-element	 8)
  (:tcl-trace-reads      #x10)
  (:tcl-trace-writes	 #x20)
  (:tcl-trace-unsets	 #x40)
  (:tcl-trace-destroyed	 #x80)
  (:tcl-interp-destroyed #x100)
  (:tcl-leave-err-msg	 #x200)
  (:tcl-trace-array      #x800)
  ;; required to support old variable/vdelete/vinfo traces */
  (:tcl-trace-old-style	 #x1000)
  ;; indicate the semantics of the result of a trace */
  (:tcl-trace-result-dynamic #x8000)
  (:tcl-trace-result-object  #x10000))

(defun var-flags (&rest kws)
  (apply '+ (loop for kw in kws
                  collecting (foreign-enum-value 'tcl-variable-related-flag kw))))

(defcstruct Tcl_ChannelType
  (typeName         :string)
  (blockModeProc    :pointer)
  (closeProc        :pointer)
  (inputProc        :pointer)
  (outputProc       :pointer)
  (seekProc         :pointer)
  (setOptionProc    :pointer)
  (getOptionProc    :pointer)
  (watchChannelProc :pointer)
  (channelReadyProc :pointer)
  (getFileProc      :pointer))

