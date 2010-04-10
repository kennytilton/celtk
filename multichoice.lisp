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

(deftk scale (commander widget)
  ()
  (:tk-spec scale
    -activestyle  -background -borderwidth -cursor
    (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness
    -relief -state
    -takefocus -troughcolor -width -xscrollcommand -yscrollcommand
    -orient -repeatdelay
    -repeatinterval
    -bigincrement -command -digits -from
    (-tk-label -label) (-tk-length -length) -resolution
    -showvalue -sliderlength -sliderrelief
    -tickinterval -to (-tk-variable nil))
  (:default-initargs
      :id (gentemp "SCL")
      :value (c-in nil)
    :tk-variable nil ;;(c? (^path))
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :on-command (lambda (self value)
                  ;; (trc "hi scale" self value)
                  (setf (^value) (parse-integer value :junk-allowed t)))))

(defmethod make-tk-instance :after ((self scale))
  "Still necessary?"
  (when (^value)
    (tk-format `(:variable ,self) "~a set ~a"  (^path) (^value))))

; --- listbox --------------------------------------------------------------

(deftk listbox (widget)
  ()  
  (:tk-spec listbox
    -activestyle  -background -borderwidth -cursor
    -disabledforeground -exportselection (tkfont -font) -foreground
    -height -highlightbackground -highlightcolor -highlightthickness
    -listvariable -relief -selectmode -selectbackground
    -selectborderwidth -selectforeground -setgrid -state
    -takefocus -width -xscrollcommand -yscrollcommand)
  (:default-initargs
      :id (gentemp "LBX")
    :tile? nil
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :event-handler (lambda (self xe)
                     (case (tk-event-type (xsv type xe))
                       (:virtualevent
                         (trc nil "LISTBOX :virtualevent" (xsv name xe))
                         (let ((event-id
                                 (intern (read-from-string
                                           (string-upcase (xsv name xe)))
                                         :keyword)))
                           (case event-id
                             (:listboxselect
                               (let ((selection
                                       (parse-integer
                                         (tk-eval "~a curselection" (^path))
                                         :junk-allowed t)))
                                 (trc nil "LISTBOX :virtualevent => selection: " selection)
                                 (when selection
                                   (setf (selection (tk-selector self))
                                         (value (elt (^kids) selection))))))))
                        )))))

(defmodel listbox-item (tk-object)
  ((item-text :initarg :item-text :accessor item-text
     :initform (c? (format nil "~a" (^value))))))

(defmethod make-tk-instance ((self listbox-item))
  (trc nil "make-tk-instance listbox-item insert" self)
  (tk-format `(:post-make-tk ,self) "~a insert end ~s" (path .parent) (^item-text)))

(defobserver .kids ((self listbox))
  (when old-value
    (tk-format `(:destroy ,self) "~a delete ~a ~a"
      (^path)
      0 (1- (length old-value)))))

; --- spinbox ---------------------------------------------

(deftk spinbox (commander widget)
  ((initial-value :initform nil :initarg :initial-value :reader initial-value))
  (:tk-spec spinbox
    -activebackground -background -borderwidth -cursor
    -buttonbackground -buttoncursor -buttondownrelief -buttonuprelief
    -disabledforeground  -disabledbackground -exportselection
    (tkfont -font) (spin-format -format) -foreground -from
    -command -invalidcommand -increment
    -highlightbackground -highlightcolor -highlightthickness 
    -insertbackground -insertborderwidth -insertofftime -insertontime
    -insertwidth -jump (tk-justify -justify) -orient
    -padx -pady -relief -repeatdelay
    -repeatinterval -selectbackground -selectborderwidth -selectforeground
    -readonlybackground -state -to
    -takefocus -text -textvariable
    -troughcolor -underline -xscrollcommand  
    -validate -validatecommand (tk-values -values) -width -wrap)
  (:default-initargs
      :value (c-in nil)
      :id (gentemp "SPN")
      :textvariable (c? (^path))
    :tile? nil
    :xscrollcommand (c-in nil)
    :command (c? (format nil "do-on-command ~a %s" (^path)))
    :on-command (c? (lambda (self text)
                      (setf (^value) text)))))

(defobserver .value ((self spinbox))
  (when new-value
    (tk-format `(:variable ,self) "set ~a ~a" (^path) (tk-send-value new-value))))

(defobserver initial-value ((self spinbox))
  (when new-value
    (setf (^value) new-value)))


