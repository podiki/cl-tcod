;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(defpackage #:tcod-system
	(:use #:cl #:asdf))

(in-package #:tcod-system)

(defsystem tcod
    :description "CFFI for libtcod, a truecolour console library."
    :author "moriarty4@gmail.com"
    :components
    ((:file "tcod") (:file "tcod-colours"))
    :depends-on ("cffi" "defstar"))
