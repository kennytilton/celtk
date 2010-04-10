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


(in-package :celtk-user)

;;; Creates a pathname with NAME and TYPE in the same
;;; directory/host/device/whatever as this lisp file. Tries to get
;;; that at compile time to cope with some useful ASDF extensions that
;;; place fasls in arbitrary places.
(defun data-pathname (name type)
  (merge-pathnames (make-pathname :name name :type type)
                   #.(or *compile-file-truename* *load-truename*)))

(defmodel lotsa-widgets (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (demo-all-menubar)
                 
                 (mk-row (:packing (c?pack-self))
                   (mk-label :text "aaa"
                     :image-files (list (list 'kt (data-pathname "kt69" "gif")))
                     :height 400
                     :width 200
                     :image (c? (format nil "~(~a.~a~)" (ctk::^path) 'kt)))
                   
                   (assorted-canvas-items)
                   
                   (mk-row ()
                     (mk-stack ()
                       (style-by-edit-menu)
                       (mk-row ()
                         (mk-stack ()
                           (mk-text-widget
                            :id :my-text
                            :value (c?n "hello, world")
                            :height 8
                            :width 25)
                          
                           (spin-package-with-symbols))
                         
                         (mk-stack ()
                           (mk-row (:id :radio-ny :selection (c-in 'yes))
                             (mk-radiobutton-ex ("yes" 'yes))
                             (mk-radiobutton-ex ("no" 'no))
                             (mk-label :text (c? (string (selection (upper self tk-selector))))))
                           (mk-row ()
                             (mk-checkbutton :id :check-me
                               :text "Check Me"
                               :value (c-in t))
                             (mk-label :text (c? (if (fm^v :check-me) "checked" "unchecked"))))
                           (mk-row ()
                             (mk-button-ex ("Time now?" (setf (fm^v :push-time)
                                                          (get-universal-time))))
                             (mk-label :text (c? (time-of-day (^value)))
                               :id :push-time
                               :value (c-in (get-universal-time))))
                           (style-by-widgets)
                           
                           (mk-row (:layout-anchor 'sw)
                             (mk-entry :id :enter-me)

                             (mk-label :text (c? (conc$ "echo " (fm^v :enter-me))))))
                         
                         (mk-stack ()
                           (duelling-scrolled-lists)
                           #+tcl-quicktime
                           (mk-row ()
                             (mk-button-ex ("Serious Demo" (plug-n-play-movie (fm^ :play-me)
                                                             "c:/0dev/celtk/demo.mov"))
                               :id :serious-demo)
                             (mk-button-ex ("Celtk?" (plug-n-play-movie (fm^ :play-me)
                                                       "c:/0dev/celtk/good-thing2.mov"))))
                           #+tcl-quicktime
                           (mk-movie :id :play-me
                             :loopstate (c-in 0) :palindromeloopstate (c-in 0)
                             :tk-file (c? (let ((entry (fm^v :enter-me)))
                                            (cond
                                             ((find entry '("bush" "war" "anger" "hate") :test 'string-equal)
                                              "c:/0dev/celtk/demo.mov")
                                             ((find entry '("sex" "drugs" "rock-n-roll" "peace") :test 'string-equal)
                                              "c:/0dev/celtk/good-thing2.mov")
                                             (t "c:/0dev/celtk/good-thing2.mov" #+not .cache))))))))))))))


(defun style-by-edit-menu ()
  (mk-row ("Style by Edit Menu")
    (mk-label :text "Four score and seven years ago today"
      :wraplength 600
      :tkfont (c? (list
                   (selection (fm^ :app-font-face))
                   (selection (fm^ :app-font-size))
                   (if (fm^v :app-font-italic)
                       'italic 'roman)
                   (if (fm^v :app-font-bold)
                       'bold 'normal))))))

(defun spin-package-with-symbols ()
  (mk-stack ()
    (mk-spinbox
     :id :spin-pkg
     :value (cells::c?n "cells")
     :tk-values (mapcar 'down$
                  (sort (mapcar 'package-name
                          (list-all-packages))
                    'string>)))
    (mk-scrolled-list
     :id :spinpkg-sym-list
     :list-height 6
     :list-item-keys (c? (let* ((spinner (fm^ :spin-pkg))
                                (item (when spinner (value spinner)))
                                (pkg (find-package (string-upcase item))))
                           (when pkg
                             (loop for sym being the symbols in pkg
                                   for n below 25
                                   counting sym into symct
                                   collecting sym into syms
                                   finally (return syms)))))
     :list-item-factory (lambda (sym)
                          (make-instance 'listbox-item
                            :fm-parent *parent*
                            :value sym
                            :item-text (down$ (symbol-name sym)))))))

(defun duelling-scrolled-lists ()
  (mk-row ()
    (mk-scrolled-list
     :id :pkg-list
     :selection (c-in (find-package "ASDF"))
     :list-height 6
     :list-item-keys (list-all-packages)
     :list-item-factory (lambda (pkg)
                          (make-instance 'listbox-item
                            :fm-parent *parent*
                            :value pkg
                            :item-text (down$ (package-name pkg)))))
    (mk-scrolled-list
     :id :pkg-sym-list
     :list-height 6
     :list-item-keys (c? (bwhen (pkg (selection (fm^ :pkg-list)))
                           (loop  for sym being the present-symbols in pkg
                                 for n below 25
                               collecting sym)))
     :list-item-factory (lambda (sym)
                          (make-instance 'listbox-item
                            :value sym
                            :fm-parent *parent*
                            :item-text (down$ (symbol-name sym)))))))

(defun assorted-canvas-items ()
  (mk-canvas
   :height 350
   :kids (c? (the-kids
              (mk-bitmap :coords (list 140 140)
                :bitmap (conc$ "@" (namestring (data-pathname "x1" "xbm"))))
              (mk-rectangle :coords (list 10 10 100 60)
                :tk-fill "red")
              (mk-text-item :coords (list 100 80)
                :text "i am an item"
                :tk-fill 'blue)
              (mk-arc :coords (list 10 100 100 160)
                :start 45
                :tk-fill "orange")
              (mk-line :coords (list 250 10 300 40 250 70 400 100)
                :width 8
                :smooth 'bezier
                :joinstyle 'miter
                :arrow 'both
                :tk-fill 'purple)
              (mk-oval :coords (list 10 200 100 260)
                :tk-fill "yellow")
              (mk-polygon :coords (list 250 210 300 220 340 200 260 180)
                :width 4
                :tk-fill 'green
                :smooth 'bezier
                :joinstyle 'miter)
              (mk-arc :coords (list 10 300 100 360)
                :start 45
                :tk-fill "white")
              ))))

(defun style-by-widgets ()
  (mk-stack ("Style by Widgets" :id :widstyle)
    (mk-row (:id :stywid
              :packing-side 'left
              :layout-anchor 'sw)
      (mk-popup-menubutton
       :id :font-face
       :initial-value (c? (second (^entry-values)))
       :entry-values (c? (subseq (tk-eval-list "font families") 4 10)))
                          
      (mk-scale :id :font-size
        :value (c-in 14)
        :tk-label "Font Size"
        :from 7 :to 24 
        :orient 'horizontal))
              
              
    (mk-label :text "Four score and seven years ago today, our fathers broguht forth on this continent a new nation..."
      :wraplength 200
      :tk-justify 'left
      :tkfont (c? (list
                 (selection (fm^ :font-face))
                 (value (fm^ :font-size)))))))

(defun demo-all-menubar ()
  (mk-menubar
   :id 'mbar
   :kids (c? (the-kids
              (mk-menu-entry-cascade
               :id 'file
               :label "File"
               :kids (c? (the-kids
                          (mk-menu
                           :id 'filemenu
                           :kids (c? (the-kids
                                      (mk-menu-entry-command :label "New" :command "tk_getOpenFile") ;; not quite right, is it?
                                      (mk-menu-entry-command :label "Open" :command "tk_getOpenFile")
                                      (mk-menu-entry-command :label "Close" :command "{destroy .}")
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-command :label "Quit"
                                        :state (c? (if t ;; (value (fm^ :check-me))
                                                       'normal 'disabled))
                                        :command "tk_getOpenFile"))))))) ;; 'exit' in production, but under dev would take out Lisp IDE
              (mk-menu-entry-cascade
               :id 'editcascade
               :label "Edit"
               :kids (c? (the-kids
                          (mk-menu
                           :id 'editmenu
                           :kids (c? (the-kids
                                      (mk-menu-entry-command :label "Undo"
                                        :on-command  (lambda (self) 
                                                         (trc "edit menu undo" self)))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-command :label "Cut" :command "exit")
                                      (mk-menu-entry-command :label "Copy" :command "exit")
                                      (mk-menu-entry-command :label "Paste" :command "exit")
                                      (mk-menu-entry-command :label "Clear" :command "exit")
                                      (mk-menu-entry-separator)
                                      (mk-menu-radio-group :id :app-font-face
                                        :selection (c-in "courier")
                                        :kids (c? (the-kids
                                                   (mk-menu-entry-radiobutton :label "Times" :value "times")
                                                   (mk-menu-entry-radiobutton :label "Courier" :value "courier")
                                                   (mk-menu-entry-radiobutton :label "Helvetica" :value "helvetica"))))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-cascade
                                       :id :app-font-size
                                       :label "Font Size"
                                       :menu (c? (path (kid1 self)))
                                       :selection (c-in 12)
                                       :kids (c? (the-kids
                                                  (mk-menu
                                                   :id :fsztoff
                                                   :tearoff 1
                                                   :kids (c? (the-kids
                                                              (loop for (label value) in '(("9" 9)("12" 12)("14"  14))
                                                                  collecting (mk-menu-entry-radiobutton :label label :value value))))))))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-checkbutton :id :app-font-italic :label "Italic")
                                      (mk-menu-entry-checkbutton :id :app-font-bold :label "Bold" :value (c-in t))))))))))))


