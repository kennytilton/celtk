;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(progn
  (declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0))))

(asdf:defsystem :gears
  :name "Gears"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :version "1.0"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :description   "Gears in Celtk"
  :long-description "Classic Gears Demo Under Celtk"
  :depends-on (:cells :celtk :cl-opengl :cl-glu)
  :serial t
  :components ((:module "gears"
                 :serial t
                 :components ((:file "gears")))))
