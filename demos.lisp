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


(defmodel my-test (window)
 ((my-mode :accessor my-mode :initform (c? (evenp (selection (fm! :my-selector))))))
 (:default-initargs
     :id :my-test-id
   :kids (c? (the-kids
              (mk-stack ("stack"  :packing (c?pack-self "-side bottom") :relief 'ridge)
                        (mk-entry :id :my-entry
                                  :value (c-in "abc"))
                        (mk-row ( "row"  #| :packing (c?pack-self "-side bottom") |# :relief 'ridge)
                                (mk-label :text (c? (format nil "selection: ~a"  (selection (fm^ :my-selector)))))
                              (mk-label :text "Labeltext")
                              (mk-button-ex ("Reset" (setf (selection (fm^ :my-selector)) 1)))
                              (mk-stack ((c? (format nil "current selection: ~a" (^selection))) :id :my-selector :selection (c-in 1) :relief 'ridge)
                                        (mk-radiobutton-ex ("selection 1" 1))
                                        (mk-radiobutton-ex ("selection 2" 2))
                                        (mk-radiobutton-ex ("selection 3" 3))
                                        (mk-radiobutton-ex ("selection 4" 4)))
                              (mk-label :text (c? (format nil "selection: ~a"  (selection (fm^ :my-selector)))))
                              ))))))

(defobserver my-mode ((self my-test) new-value old-value old-value-bound-p)
 (format t "~% mode changed from ~a to ~a" old-value new-value))

(defun ctk::franks-test ()
  (run-window 'my-test))

#+test 
(ctk::franks-test)

(defun ctk::tk-test () ;; ACL project manager needs a zero-argument function, in project package
  (setf ctk::*tk-log* (open "/devel/tk.log" :direction :output :if-exists :supersede))
  (test-window 
   ;;'place-test
   ;; 'one-button-window
   ;;'ltktest-cells-inside
   ;;'menu-button-test
   ;;'spinbox-test
   'lotsa-widgets
   ;; Now in Gears project 'gears-demo
  ))

(defmodel place-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids                
                 (mk-label :text "hi, Mom"
                   :parent-x 100
                   :py 20)))))

(defmodel one-button-window (window)
  ()
  (:default-initargs
      :kids (c? (the-kids                
                 #+shhhh (mk-menubar
                  :kids (c? (the-kids
                             (mk-menu-entry-cascade-ex (:label "File")
                               (mk-menu-entry-command-ex () "Load" (format t "~&Load pressed"))
                               (mk-menu-entry-command-ex () "Save" (format t "~&Save pressed"))))))
                 (mk-frame-stack
                  :packing (c?pack-self)
                  :kids (c? (the-kids
                             (mk-text-widget
                              :id :my-text
                              :value (c?n "[bzbzbzbz]")
                              :height 8
                              :width 25)
                             (make-instance 'entry
                               :id :entree
                               :fm-parent *parent*
                               :value (c-in "kenzo")))))))))

(defun one-deep-menubar ()
  (mk-menubar
   :id 'mbar
   :kids (c? (the-kids
              (mk-menu-entry-cascade-ex (:label "File")
                            (mk-menu-entry-command-ex () "Load" (format t "~&Load pressed"))
                            (mk-menu-entry-command-ex () "Save" (format t "~&Save pressed")))
              (mk-menu-entry-cascade
               :id 'editcascade
               :label "Edit"
               :kids (c? (the-kids
                          (mk-menu
                           :id 'editmenu
                           :kids (c? (the-kids
                                      (mk-menu-radio-group :id :app-font-face
                                        :selection (c-in "courier")
                                        :kids (c? (the-kids
                                                   (mk-menu-entry-radiobutton :label "Times" :value "times")
                                                   (mk-menu-entry-radiobutton :label "Courier" :value "courier")
                                                   (mk-menu-entry-radiobutton :label "Helvetica" :value "helvetica"))))))))))))))

(defmodel spinbox-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-stack (:packing (c?pack-self))
                   (mk-spinbox
                    :id :spin-pkg
                    :value (c-in "cells") ;;(cells::c?n "cells")
                    :tk-values (mapcar 'down$
                                 (sort (mapcar 'package-name
                                         (list-all-packages))
                                   'string>)))
                   (mk-scrolled-list
                    :id :spinpkg-sym-list
                    :list-height 6
                    :list-item-keys (c? (trc "enter item keys" self (fm^ :spin-pkg))
                                      (let* ((spinner (fm^ :spin-pkg))
                                               (item (when spinner (value spinner)))
                                               (pkg (find-package (string-upcase item))))
                                          (when pkg
                                            (loop for sym being the symbols in pkg
                                                for n below 5
                                                counting sym into symct
                                                collecting sym into syms
                                                finally (return syms)))))
                    :list-item-factory (lambda (sym)
                                         (make-instance 'listbox-item
                                           :fm-parent *parent*
                                           :value sym
                                           :item-text (down$ (symbol-name sym)))))
                   (mk-label :text (c? (selection (fm^ :spinpkg-sym-list)))))))))


(defmodel menu-button-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-stack ("Style by Widgets" :id :widstyle :packing (c?pack-self))
                   (mk-popup-menubutton
                    :id :font-face
                    :initial-value (c? (second (^entry-values)))
                    :entry-values (c? (subseq (tk-eval-list "font families") 4 10)))
                   (mk-label :text "Four score and seven years ago today, our fathers broguht forth on this continent a new nation..."
                     :wraplength 200
                     :tk-justify 'left
                     :tkfont (c? (list
                                  (selection (fm^ :font-face))
                                  14))))))))
 
(defmodel font-view-2 (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-panedwindow
                  :packing (c?pack-self)
                  :orient 'vertical
                  :kids  (c? (the-kids
                              (loop repeat 2
                                  collecting (make-instance 'font-view :fm-parent *parent*)))))))))

(defun mk-font-view ()
  (make-instance 'font-view))

(defmodel font-view (frame-stack)
  ()
  (:default-initargs
      :value (c? (tk-eval-list "font families"))
    :pady 2 :padx 4
    :packing-side 'left
    :layout-anchor 'nw
    :kids (c? (the-kids
               (mk-spinbox :id :font-face
                 :value (c-in (car (^value)))
                 :tk-values (c? (value .parent)))
               (mk-scale :id :font-size
                 :value (c-in 14)
                 :tk-label "Font Size"
                 :from 7 :to 24 
                 :orient 'horizontal)
               (mk-label :id :txt
                 :text "Four score seven years ago today"
                 :wraplength 600
                 :tkfont (c? (list ;; format nil "{{~{~a~^ ~}} ~a}" ;; eg, {{wp greek century} 24}
                            (value (fm^ :font-face))
                            (value (fm^ :font-size)))))))))

#| 06-02-14 following stuff not resurrected after latest revisions to Celtk

;;; ---- toplevel --------------------------------




(defmodel file-open (toplevel)
  ()
  (:default-initargs
    :value (c? (directory "\\windows\\fonts\\*.ttf"))
    :pady 2 :padx 4
    :kids  (c? (the-kids
                (mk-spinbox :id :font-face
                  :value (c-in (car (^value)))
                 :tk-values (c? (mapcar 'pathname-name (value .parent))))
                (mk-button-ex ("Open" (progn
                                     (tk-format `(:destroy ,self) "destroy ~a" (path (upper self toplevel)))
                                     (not-to-be (upper self toplevel))))
                 :underline 0)))))

|#