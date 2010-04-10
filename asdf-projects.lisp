(in-package :cl-user)



(eval-when (compile load)
  (require :asdf)
  (loop for project in '(alexandria trivial-features_0.6 babel_0.3.0 cffi_0.10.5)
      do (pushnew
          (namestring (make-pathname :directory `(:absolute "devel" ,(string project))))
          asdf:*central-registry* :test 'string-equal))
  (asdf:oos 'asdf:load-op :cffi))

