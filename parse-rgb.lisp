;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- ;;;;;;;;;;;;;;;;;80
;;;;
;;;; parse X11's rgb.txt file to automatically generate colour definitions
;;;; for CL-TCOD.
;;;;
;;;; Taken from Tamas Papp's CL-COLORS library and modified.
;;;; Original library is available at: (@url :file-name "http://github.com/tpapp/cl-colors/" :display "Github")
;;;;
;;;; If you want to regenerate tcod-colours.lisp:
;;;; - Install the CL-PPCRE library
;;;;   from: (@url :file-name "http://weitz.de/cl-ppcre/" :display "PCCRE website")
;;;; - At command prompt, change into the cl-tcod library's directory
;;;; - Start lisp, and at the command prompt:
;;;;      (asdf:load-system :parse-rgb)
;;;;      (parse-rgb:parse-rgb-file)

(defpackage :parse-rgb
  (:use :cl)
  (:export #:parse-rgb-file
           #:make-rgb.txt-colours))

(in-package :parse-rgb)

(defvar *rgb-txt-file* "/etc/X11/rgb.txt"
  "Full path to rgb.txt file to parse")

(defvar *output-file-name* "tcod-colours.lisp"
  "Name of file to generate")

(defun parse-rgb-file (&key (infile *rgb-txt-file*)
			      (outfile *output-file-name*))
  "Parse a file of colour definitions in the format of the X-windows file
'rgb.txt'. Generate a file of lisp code that allows the CL-TCOD library
to use all the colours defined in the input file, by name."
  (let ((color-scanner		     ; will only take names w/o spaces
	 (cl-ppcre:create-scanner 
	  "^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([\\s\\w]+\?)\\s*$"
	  :extended-mode t))
	(comment-scanner
	 (cl-ppcre:create-scanner
	  "^\\s*!")))
    (with-open-file (s infile
		       :direction :input
		       :if-does-not-exist :error)
      (with-open-file (colornames outfile
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	(format colornames ";;;; This file was generated automatically ~
by parse-rgb.lisp~%~
;;;; Please do not edit directly.~%~
 (in-package :tcod)~%
 (defun make-rgb.txt-colours ()~%")
	(labels ((string-to-float (string)
		   (let ((i (read-from-string string)))
		     (assert (and (typep i 'integer) (<= i 255)))
		     i)))
	  (do ((line (read-line s nil nil) (read-line s nil nil)))
	      ((not line))
	    (unless (cl-ppcre:scan-to-strings comment-scanner line)
	      (multiple-value-bind (match registers)
		  (cl-ppcre:scan-to-strings color-scanner line)
		(cond
		  (match
		  (if (find #\space (aref registers 3))
		      (setf (aref registers 3) (substitute #\- #\space
							   (aref registers 3))))
		  (format colornames
			  "  (make-colour :~A ~A ~A ~A)~%"
			  (string-downcase (aref registers 3))
			  (string-to-float (aref registers 0))
			  (string-to-float (aref registers 1))
			  (string-to-float (aref registers 2))))
		  (t
		   (format *error-output* "ignoring line ~A~%" line))))))
          (format colornames "  )~%")
          )))))
