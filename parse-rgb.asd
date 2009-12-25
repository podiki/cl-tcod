;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- ;;;;;;;;;;;;;;;;;80

(defpackage #:parse-rgb-asd
  (:use :cl :asdf))

(in-package :parse-rgb-asd)

(defsystem parse-rgb
  :name "parse-rgb"
  :version "1"
  :author "moriarty4@gmail.com"
  :description
  "Autogenerate TCOD colour definitions by parsing rgb.txt on Xwindows"
  :serial t	; Only true for the simple case	.
  :components 
   ((:file "parse-rgb"))
  :depends-on ("tcod" "cl-ppcre"))




