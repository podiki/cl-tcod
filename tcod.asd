;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(defpackage #:tcod-system
	(:use #:cl #:asdf))

(in-package #:tcod-system)

(defsystem tcod
    :description "Common Lisp bindings for libtcod, a truecolour
terminal-emulation library written in C."
    :author "Paul Sexton <eeeickythump@gmail.com>"
    :components
    ((:file "tcod") (:file "tcod-colours"))
    :depends-on ("cffi" "defstar"))
