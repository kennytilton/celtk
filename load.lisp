;;;
;;; 
;;; 1. Grab these:
;;;
;;;    http://common-lisp.net/cgi-bin/viewcvs.cgi/cells/?root=cells
;;;    Celtk: http://common-lisp.net/cgi-bin/viewcvs.cgi/Celtk/?root=cells
;;;    CFFI: http://common-lisp.net/project/cffi/releases/cffi_0.9.1.tar.gz
;;;    cl-opengl: http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=cl-opencl%20cl-opengl;a=summary 
;;
;;;   At the bottom of any of those pages look for a "Download tarball" link. Except cl-opengl, those guys
;;;   are not download-friendly.
;;;
;;; 2. Get ASDF loaded. From http://www.cliki.net/asdf we learn:
;;;
;;; "If you have SBCL, OpenMCL, ECL or ACL, it's bundled and you need only (require 'asdf). 
;;;  If you have Debian or Gentoo and the Common Lisp Controller installed, you also 
;;;  already have it. Otherwise you can find it in the Sourceforge cCLan CVS repository:
;;;
;;;     http://cclan.cvs.sourceforge.net/cclan/asdf/ "
;;;
;;; 3. If the automatic options in step 2 could not be used, adjust the path and evaluate

#+adjust-pathname-first!

(load (make-pathname #+lispworks :host #-lispworks :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

;;; 4. Only after you have gotten ASDF loaded, you can tell ASDF
;;;    where you put everything by adjusting these paths and evaluating:

(progn
  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "cells"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "1-devtools" "cffi-060606"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "Celtk"))
        asdf:*central-registry*))

;;; 5. Track down all the define-foreign-library calls in the source
;;;    and fix the pathnames to point to your shared libraries. Recently these were:
;;;
;;;    In tk-interp.lisp, Tcl and Tk d-f-ls.

;;; 6. Now you can try building the whole mess. Warning: I use ":serial t" to work around
;;;    silly ASDF default behavior, so if you start fiddling with the code you may not want
;;;    to use ASDF to build (or comment out the :serial option until the next session):

(ASDF:OOS 'ASDF:LOAD-OP :celtk)

;;; and test:

(ctk::test-window 'celtk-user::lotsa-widgets)

;;; To see the OpenGL Gears demo, some heavy lifting is required.
;;;
;;; 1. Get, install, and test Togl. Here is a Web link:
;;;
;;;       http://www.mesa3d.org/brianp/sig97/togl.htm
;;;
;;;    If you are on win32 and have trouble, send an email to the list and I will send you a DLL
;;;
;;; 2. You already grabbed cl-opengl from the location shown above. Now:
;;;

#+adjust-pathname-and-evaluate
(push (make-pathname #+lispworks :host #-lispworks :device "c"
        :directory '(:absolute "1-devtools" "cl-opengl"))
  asdf:*central-registry*)

;;;
;;;  3.  Adjust the pathname again in togl.lisp, in the define-foreign-library for Togl.
;;;
;;;  4.  Build:

(ASDF:OOS 'ASDF:LOAD-OP :gears)

;;; 5.  Test:

#+test
(gears::gears)
