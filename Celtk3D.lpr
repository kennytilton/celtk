;; -*- lisp-version: "8.2 [64-bit Linux (x86-64)] (Mar 3, 2010 14:34)"; cg: "1.134"; -*-

(in-package :cg-user)

(defpackage :celtk)

(define-project :name :celtk3d
  :modules (list (make-instance 'module :name "cellogears.lisp"))
  :projects (list (make-instance 'project-module :name "../cells/cells" :show-modules
                                 nil)
                  (make-instance 'project-module :name "celtk" :show-modules nil))
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :celtk
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :top-level :debugger)
  :build-flags (list :allow-runtime-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+M +t \"Console for Debugging\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 0
  :run-with-console nil
  :project-file-version-info nil
  :on-initialization 'celtk::cellogears
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
