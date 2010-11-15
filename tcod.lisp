;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- ;;;;;;;;;;;;;;;;;80

(in-package :cl-user)


;;;
;;; Windows users: change the string "libtcod-mingw.dll" to reflect the name
;;; of your libtcod library (DLL file).
;;;
;;; Colours are passed to C as integers. There is also a system mapping
;;; - make a colour from R, G, B values using COMPOSE-COLOUR.
;;; - break down a colour into R, G and B values using DECOMPOSE-COLOUR.
;;; - to start the colour system call START-COLOURS.
;;; - to make a new colour and associate it with a name, use MAKE-COLOUR.

(declaim (optimize (speed 0) (safety 2) (debug 3)))

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))


(defpackage :tcod
  (:use :cl :cffi :defstar)
  (:export
   #:*root*
   #:+null+
   ;; [[Colours]] ==========================================================
   #:start-colours
   #:start-colors
   #:colour
   #:color
   #:compose-colour
   #:compose-color
   #:decompose-colour
   #:decompose-color
   #:invert-colour
   #:invert-color
   #:colour->grayscale
   #:color->grayscale
   #:colour-set-hsv
   #:colour-get-hsv
   #:colour-get-hue
   #:colour-get-saturation
   #:colour-get-value
   #:colour-equals?
   #:colour-add
   #:colour-multiply
   #:colour-multiply-scalar
   #:colour-lerp
   #:make-colour
   #:color-set-hsv
   #:color-get-hsv
   #:color-get-hue
   #:color-get-saturation
   #:color-get-value
   #:color-equals?
   #:color-add
   #:color-multiply
   #:color-multiply-scalar
   #:color-lerp
   #:make-color
   #:background-alpha
   #:background-add-alpha
   ;; [[Console]] ==========================================================
   #:console-wait-for-keypress
   #:console-check-for-keypress
   #:console-set-colour-control
   #:console-set-color-control
   #:console-flush
   #:console-get-fading-colour
   #:console-get-fading-color
   #:console-get-fade
   #:console-set-fade
   #:console-get-char
   #:console-get-default-foreground
   #:console-get-default-background
   #:console-set-default-foreground
   #:console-set-default-background
   #:console-set-alignment
   #:console-get-alignment
   #:console-set-background-flag
   #:console-get-background-flag
   #:console-print
   #:console-print-ex
   #:console-print-rect
   #:console-print-rect-ex
   ;; #:console-print-left
   ;; #:console-print-right
   ;; #:console-print-centre
   ;; #:console-print-center
   ;; #:console-print-left-rect
   ;; #:console-print-right-rect
   ;; #:console-print-centre-rect
   ;; #:console-print-center-rect
   #:console-hline
   #:console-vline
   #:console-print-frame
   #:console-print-double-frame
   #:console-map-ascii-code-to-font
   #:console-map-ascii-codes-to-font
   #:console-map-string-to-font
   #:console-get-height-rect
   ;; #:console-height-left-rect
   ;; #:console-height-right-rect
   ;; #:console-height-centre-rect
   ;; #:console-height-center-rect
   #:legal-console-coordinates?
   #:console-put-char
   #:console-put-char-ex
   #:console-set-char
   #:console-set-char-foreground
   #:console-set-char-background
   #:console-get-char-foreground
   #:console-get-char-background
   #:console-clear
   #:console-fill-char
   #:console-set-dirty
   #:console-init-root
   #:console-is-fullscreen?
   #:console-set-fullscreen
   #:console-is-window-closed?
   #:console-credits
   #:console-credits-reset
   #:console-credits-render
   #:console-set-custom-font
   #:console-set-window-title
   #:console-rect
   #:drawing-character
   #:colctrl
   #:colctrl->char
   #:background-flag
   #:console
   #:console-new
   #:console-delete
   #:console-get-width
   #:console-get-height
   #:console-blit
   ;; [[Keyboard input]] ======================================================
   #:key
   #:keycode
   #:key-p
   #:key-c
   #:key-vk
   #:key-lalt
   #:key-ralt
   #:key-lctrl
   #:key-rctrl
   #:key-shift
   #:make-key
   #:make-simple-key
   #:same-keys?
   #:key-state
   #:key-pressed
   #:is-key-pressed?
   #:console-set-keyboard-repeat
   #:console-disable-keyboard-repeat
   ;; == Unicode ==
   ;; todo not yet implemented
   ;; [[Mouse]] ===============================================================
   #:mouse
   #:make-mouse
   #:mouse-x
   #:mouse-y
   #:mouse-cx
   #:mouse-cy
   #:mouse-dx
   #:mouse-dy
   #:mouse-lbutton
   #:mouse-mbutton
   #:mouse-rbutton
   #:mouse-lbutton-pressed
   #:mouse-mbutton-pressed
   #:mouse-rbutton-pressed
   #:mouse-move
   #:mouse-get-status
   #:mouse-get-x
   #:mouse-get-y
   #:mouse-get-cx
   #:mouse-get-cy
   #:mouse-get-dx
   #:mouse-get-dy
   #:mouse-get-dcx
   #:mouse-get-dcy
   #:mouse-get-lbutton
   #:mouse-get-mbutton
   #:mouse-get-rbutton
   #:mouse-get-lbutton-pressed
   #:mouse-get-mbutton-pressed
   #:mouse-get-rbutton-pressed
   ;; [[Image]] ===============================================================
   #:image-new
   #:image-load
   #:image-save
   #:image-from-console
   #:image-clear
   #:image-put-pixel
   #:image-blit
   #:image-blit-rect
   #:image-set-key-color
   #:image-set-key-colour
   #:image-get-pixel
   #:image-get-mipmap-pixel
   ;; [[Random]] ==============================================================
   #:random-new
   #:random-get-instance
   #:random-delete
   #:random-get-int
   #:random-get-float
   ;; [[Noise]] ===============================================================
   #:noise-new
   #:noise-delete
   #:noise-set-type
   #:noise-get
   #:noise-get-ex
   #:noise-get-fbm
   #:noise-get-fbm-ex
   #:noise-get-turbulence
   #:noise-get-turbulence-ex
   ;; [[Heightmap]] ===========================================================
   #:heightmap
   #:heightmap-new
   #:heightmap-get-value
   #:heightmap-get-interpolated-value
   #:heightmap-get-slope
   #:heightmap-set-value
   #:heightmap-add
   #:heightmap-add-fbm
   #:heightmap-scale
   #:heightmap-lerp
   #:heightmap-add-hm
   #:heightmap-multiply-hm
   #:heightmap-clear
   #:heightmap-delete
   #:heightmap-clamp
   #:heightmap-count-cells
   #:heightmap-has-land-on-border?
   #:heightmap-get-min
   #:heightmap-get-max
   #:heightmap-normalize
   #:heightmap-normalise
   #:heightmap-copy
   #:heightmap-dig-bezier
   #:heightmap-dig-line
   #:heightmap-rain-erosion
   ;; [[Field of view]] =======================================================
   #:fov-algorithm
   #:mapptr
   #:map-new
   #:map-set-properties
   #:map-compute-fov
   #:map-is-in-fov?
   #:map-is-transparent?
   #:map-is-walkable?
   #:map-clear
   #:map-delete
   #:map-copy
   ;; [[A* pathfinding]] ======================================================
   #:a*-path
   #:dijkstra-path
   #:path-new-using-map
   #:path-new-using-function
   #:path-delete
   #:path-compute
   #:path-get-origin
   #:path-get-destination
   #:path-size
   #:path-get
   #:path-walk
   #:path-is-empty?
   ;; [[Dijkstra pathfinding]] ================================================
   #:dijkstra-path
   #:dijkstra-new
   #:dijkstra-new-using-function
   #:dijkstra-delete
   #:dijkstra-compute
   #:dijkstra-path-set
   #:dijkstra-size
   #:dijkstra-get-distance
   #:dijkstra-get
   #:dijkstra-is-empty?
   #:dijkstra-path-walk
   ;; [[System]] ==============================================================
   #:sys-save-screenshot
   #:sys-sleep-milli
   #:sys-set-fps
   #:sys-get-fps
   #:sys-get-current-resolution
   #:sys-flush
   ;; [[Testing]] =============================================================
   )
  (:documentation
   "* Introduction

Welcome to CL-TCOD, an interface between Common Lisp and the Doryen Library,
AKA `libtcod', a portable truecolour console library intended for use with
roguelike games.

CL-TCOD consists of the following files:
1. =tcod.lisp=, a lisp file which creates lisp bindings for C functions in the
   compiled libtcod library, using the =CFFI= lisp foreign function interface.
2. =tcod.asd=, which allows TCOD to be easily loaded and used as a library by
   other common lisp programs, via the =ASDF= library-loading facility.
3. =tcod-colours.lisp=, a lisp file containing definitions for all the colours
   named in /etc/X11/rgb.txt; autogenerated using 'parse-rgb' (see below)
4. =parse-rgb.lisp=, a lisp file containing code for parsing =/etc/X11/rgb.txt=
   and generating tcod-colours.lisp
5. =parse-rgb.asd=, ASDF system definition file for =parse-rgb.lisp=

CL-TCOD has been tested with SBCL 1.0.36 on Linux and Windows, Clozure 1.5
on Linux and Windows, and CLISP on Windows.

**Note** that it has not been used on a Mac; if you do this you may need to
tell CFFI the name of the compiled libtcod library under MacOS. To do this,
open =tcod.lisp= in an editor, find the ='(define-foreign-library...'= clause,
uncomment the ='(:macintosh...)'= line and change the string on that line to
the name of the libtcod library file.

* License

The CL-TCOD package is placed in the Public Domain by its author.

* Dependencies

=CL-TCOD= depends on the following libraries:
1. ASDF: [[http://common-lisp.net/project/asdf/]]
2. DEFSTAR: [[http://bitbucket.org/eeeickythump/defstar/]]
3. CFFI: [[http://common-lisp.net/project/cffi/]]

* Hints on installation

You need to know your way around your chosen common lisp and how to install and
load lisp libraries before proceeding. You also need to have a version of
libtcod newer than 1.4.1rc2, which is the first version that includes the
='wrappers.c'= and ='wrappers.h'= source files that allow CL-TCOD to interface
with libtcod.

1. Ensure you have a working common lisp installation.
2. Ensure the ASDF lisp library is installed.
3. If CFFI or DEFSTAR are not installed, download and install them somewhere ASDF
   can find them. CFFI requires several third-party lisp libraries -- see the
   CFFI documentation for more details.
4. Put the CL-TCOD files in a directory where ASDF can find them.
5. Make sure libtcod is installed and compiled. Make sure the libtcod
   dynamically linked library (=.DLL= or =.SO= file) is somewhere your lisp system
   can find it. It probably is, but if CFFI complains about being unable to
   find the library, you can either copy it to an appropriate directory or add
   its directory to the list variable =cffi:*foreign-library-directories*=
   e.g. by typing the following in the lisp interpreter:

;;;   (push #P\"/my/libtcod/directory/\" cffi:*foreign-library-directories*)

   *On windows*, DLL files should be put in one of the directories listed in the
   =PATH= environment variable. You will need to put =SDL.dll= in the same place
   if you don't already have SDL installed.

   *On Linux*, you can usually put .SO files in =/usr/local/lib/=.
   Use your package installer to install =libSDL=.
   Try running the libtcod demo programs to check everything works.

6. Start lisp. Load ASDF, then CL-TCOD:

;;;   (load \"/path/to/asdf/asdf.lisp\")
;;;   (asdf:oos 'asdf:load-op :tcod)

7. Type something like the following commands at the lisp prompt to start using TCOD
   from within Lisp. Alternatively you can type =(tcod:hello-world)=, which
   is a function containing the code below.

;;;   (tcod:console-set-custom-font \"terminal.png\" '(:font-layout-ascii-in-row) 16 16)
;;;   (tcod:console-init-root 80 25 \"Test\" nil)
;;;   (tcod:console-clear tcod:*root*)
;;;   (tcod:console-print tcod:*root* 1 1 \"Hello, world!~%\")
;;;   (tcod:console-wait-for-keypress t)


* Differences between CL-TCOD and libtcod

** Naming conventions

The C function =TCOD_foobar= corresponds to the lisp function =foobar=, which
is in the =tcod= package (and so requires a prefix of =tcod:= to access in most
situations). Underscores become hyphens. So:

:  TCOD_foobar_function(a, b)     <===>    (tcod:foobar-function a b)

`Predicate functions' are functions whose main job is to return a boolean
value, true (non =NIL=) or false (=NIL=), that answers a question. These have a
terminal '?' added to their name:

:  TCOD_console_is_fullscreen()   <===>    (tcod:console-is-fullscreen?)

C enums have generally more succinct names. As they are lisp keywords, their
names all begin with =':'=. THey are named according to the following pattern:

:  TCODK_BACKSPACE (etc)         <===>  :backspace
:  TCOD_CHAR_HLINE  (etc)        <===>  :char-hline
:  TCOD_COLCTRL_1  (etc)         <===>  :colctrl-1
:  TCOD_BKGND_SET (etc)          <===>  :set
:  TCOD_FONT_LAYOUT_ASCII_INCOL  <===>  :font-layout-ascii-in-col
:  FOV_SHADOW                    <===>  :fov-shadow
:  TCOD_KEY_PRESSED              <===>  :key-pressed
:  CENTER                        <===>  :center

In general, most functions exist in both U.S. and non-U.S. spellings, This is
mainly relevant to those functions with colour/color or centre/center in their
names.

** Colournums

In libtcod, colours are represented as structures containing three integer
values: *red*, *green* and *blue* (each 0-255). The name of the structure type is
=TCOD_color_t=.

In CL-TCOD, these colour structs are converted into 3-byte integers using the C
functions =int_to_color(int)= and =color_to_int(TCOD_color_t)=, both defined in
=wrappers.c=. The 3 bytes are red, green and blue in order (blue is 1's). ie:

:    /* C */                              ;; lisp ;;
:   struct TCOD_color_t {r, g, b}  <==>   #x00RRGGBB

So, for example, one way to use the function =TCOD_color_multiply_scalar= from
lisp is:

;;;  (tcod:color-multiply-scalar (tcod:compose-colour 218 165 32) 0.5)

All C functions that take or return =TCOD_color_t= structs, are wrapped by lisp
functions that take or return integers as described above.

** Colours by keyword

A lisp keyword is any symbol beginning with ':'. In lisp, keywords (like all
symbols) are first-class values and can be passed around just like any other
value. CL-TCOD uses keywords to refer to particular colours, for example the
keyword =:cyan= refers to the colour #x0056A3CD (or 5678029 in decimal notation).

You can use keywords instead of colournums as arguments to lisp functions, by
using the function =colour= to return the colournum associated with a keyword:

;;;  (tcod:colour :cyan)    ; returns 5678029


You can also define your own colour names, like so:

;;;  (tcod:make-colour :my-goldenrod 218 165 32)
;;;  (tcod:color-multiply-scalar (tcod:colour :my-goldenrod) 0.5)

CL-TCOD knows all the colour names defined in the 'rgb.txt' file under
Xwindows, eg =:navajo-white, :honeydew, :mint-cream=, and so on. There is
nothing special about the fact that rgb.txt comes from Xwindows -- the colours
are just named R,G,B values and can be used anywhere that CL-TCOD can be
used. Look in the source file ='tcod-colours.lisp'= to see the available colour
names. If you are using [[http://www.gnu.org/software/emacs/][GNU Emacs]], the
king of lisp IDEs, do =M-x list-colors-display= to see a list of all colours.

** Lisp =format= versus C =printf=

The TCOD functions that accept =printf=-like string-formatting arguments,
have been modified to instead accept arguments to Common Lisp's =format=
function.'  For example:

#+BEGIN_SRC c
  TCOD_console_print (con, x, y, \"Printing at %d, %d\n\", x, y);
#+END_SRC

becomes:

;;;    (tcod:console-print con x y \"Printing at ~D, ~D~%\" x y)

** Miscellaneous extra functions

- [[console-print-double-frame]] is like [[console-print-frame]], but
  but draws using `double-line' characters:

;;;  (tcod:console-print-double-frame CONSOLE X Y W H EMPTY? STRING...)


* Resources

** Specific to CL-TCOD and libtcod

The latest version of CL-TCOD is available at:

    [[http://bitbucket.org/eeeickythump/cl-tcod/]]

Forum for discussion of CL-TCOD and use of lisp in roguelike games:

    [[http://doryen.eptalys.net/forum/index.php?board=33.0][Roguecentral Lisp forum]]

The latest version of libtcod is available at:

    [[http://doryen.eptalys.net/libtcod/]]

This Common Lisp package depends on CFFI, the Common Foreign Function Interface:

    [[http://common-lisp.net/project/cffi/]]

** Learning Common Lisp

Recently written book, 'Practical Common Lisp'. buy hard copy or download free.
Recommended, especially if coming from non-lisp languages.

- [[http://www.gigamonkeys.com/book/]]

*\"Lisp in a Box\"* -- aims to make it easy to start using Common Lisp by
providing a single download with everything set up in advance:

- [[http://common-lisp.net/project/lispbox/]]

Lisp editors and IDEs:
- [[http://www.gnu.org/software/emacs/][GNU Emacs]] (the best; see below)
  - [[http://common-lisp.net/project/slime/][SLIME]] is the Emacs interface to
    Common Lisp.
- [[http://bitfauna.com/projects/cusp/][Cusp]], a common lisp plugin for Eclipse.
- The [[http://www.franz.com/products/allegrocl/][Allegro]] and
  [[http://www.lispworks.com/][LispWorks]] lisp implementations each have a
  builtin IDE.
- If you are on a Mac, the free, high-quality [[http://ccl.clozure.com][Clozure CL]]
  has a builtin IDE called Cocoa.
- Some editors with good lisp syntax highlighting include jEdit and Notepad++

** A note on editors and IDEs

Emacs is a very powerful program. It is mainly used as a programmers' text and
source code editor, but it can do -- and plugins exist to make it do -- just
about anything you can imagine. It is mostly written in a dialect of lisp, and
this is also its extension language. When combined with SLIME, a plugin (mode)
that allows it to communicate directly with a running common lisp
compiler/interpreter, Emacs is not only the best IDE for common lisp, but
one of the best and most advanced IDEs available for any programming language.

The downside: because Emacs + SLIME is so good, common lisp programmers have
put very little effort into getting other popular programming editors/IDEs to
support common lisp, at least beyond simple syntax highlighting. Emacs is an
idiosyncratic program (it is about 34 years old) and despite good efforts to
modernise/regularise its interface it still has a steeper learning curve than
many other IDEs, especially when you are also struggling to set up SLIME and
get it to communicate with your lisp...

My advice is that while all roads lead to Emacs, you don't have to hurry to get
there. Initially you should concentrate on getting common lisp set up and
starting to learn the language. Think about using the trial version of one of
the big commercial implementations (Allegro or LispWorks), as they have
built-in IDEs. Once you are ready to move on from them, install Emacs and
SLIME.

** Commercial Common Lisp implementations

These are both high quality, but painfully expensive. Luckily they have
'limited' versions that can be downloaded for free, and which I recommend you
use when beginning to learn common lisp.

- [[http://www.franz.com/products/allegrocl/][Allegro]] -- starts at $599 USD
- [[http://www.lispworks.com/][LispWorks]] -- starts at $900 USD for a
  noncommercial license. The trial version quits automatically after 5 hours.

** Full-featured, free Common Lisp implementations

Move on to one of these if and when you outgrow Allegro or LispWorks.

- [[http://www.sbcl.org]] (compiles to machine code, great on Linux/Mac,
  still 'experimental' on Windows)
- [[http://clisp.cons.org][GNU CLISP]] (bytecode compiler, but runs pretty much
  everywhere)
- [[http://ccl.clozure.com][Clozure CL]] (compiles to machine code; native to
  Mac but runs well on Linux and Windows; it has displaced SBCL to become my
  implementation of choice)
- [[http://ecls.sourceforge.net/][Embeddable Common Lisp]] Promising, compiles
  to C and then passes code to your C compiler. Does this 'on the fly' when
  running as an interpreter. Also designed to be easily embeddable as a
  scripting language.

Help & advice with lisp:

    [[http://www.lispforum.com]]
"))

(in-package :tcod)


;;; Comment this out if you want cl-tcod to be 'fast' rather than 'safe and
;;; maximally debuggable'
(eval-when (:compile-toplevel :load-toplevel :execute)
 (pushnew :tcod-debug *features*))


#+tcod-debug (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-tcod-debug (declaim (optimize (speed 3) (safety 1) (debug 0)))


;;; CFFI 0.10.0 started using Babel to "encode" strings. This breaks extended
;;; ASCII characters when the default encoding scheme of :UTF-8 is used, ie C
;;; will receive different characters from those which are sent to it by the
;;; Lisp program. To actually pass the literal string to C, we need to change
;;; the encoding scheme to ISO-8859-1.
;;;

(setf cffi:*default-foreign-encoding* :iso-8859-1)

;;; If debug mode is on, force compiler to explicitly enforce type declarations
;;; for function arguments, raising an error when a type mismatch occurs.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf defstar:*check-argument-types-explicitly?*
        #+tcod-debug t
        #-tcod-debug nil))


;;;; <<Library>> ==============================================================



(define-foreign-library libtcod
	(:unix #+libtcod-debug "libtcod-debug.so"
               #-libtcod-debug "libtcod.so")
	(:windows #-libtcod-debug "libtcod-mingw.dll"
                  #+libtcod-debug "libtcod-mingw-debug.dll")
	;; (:macintosh "NAME-OF-LIBTCOD-LIBRARY-IN-MACOS")
	(t (:default "libtcod")))

(defvar *libtcod-loaded* nil
  "Global variable, set to non-nil once libtcod is loaded. This is to
avoid crashes which occur in some CL implementations when you load
an already-loaded foreign library.")

(eval-when (:load-toplevel :execute)
	(unless *libtcod-loaded*
		(use-foreign-library libtcod)
		(setf *libtcod-loaded* t)))


;;;; <<Macros>> ===============================================================


;;; The following are some wrapper macros to ease the creation
;;; of `type-safe' CFFI wrappers to C functions.


(defmacro define-c-enum (name &rest vals)
  "Defines both the CFFI =enum= type, and a lisp type of the same
name which is satisified by any of the values allowed by the enum type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (defcenum ,name ,@vals)
       (deftype ,name ()
         '(member ,@(mapcar #'(lambda (val)
                                (if (listp val) (car val) val))
                            vals))))))


(defmacro define-c-bitfield (name &rest clauses)
  "Defines both the CFFI bitfield, and a lisp type of the same name, which
is satisfied by a list containing only bitfield keywords as members."
  (flet ((make-predicate (sym)
           (intern (concatenate 'string (string sym) "-PREDICATE"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
         (defbitfield ,name ,@clauses)
         (defun ,(make-predicate name) (ls)
           (and (listp ls)
                (null (set-difference
                       ls ',(mapcar #'car clauses)))))
         (deftype ,name ()
           '(satisfies ,(make-predicate name)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prepend-percent (sym)
    (intern (concatenate 'string "%%" (string sym)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c-type->lisp-type (c-type)
    "Given a CFFI foreign type, return an equivalent lisp type."
    (case c-type
      (:boolean 'boolean)
      ((:int :unsigned-int) 'uint)
      (:unsigned-char 'uchar)
      (:uint8 'uint8)
      (:uint32 'uint32)
      (:float 'single-float)
      (:pointer (type-of (null-pointer)))
      (:string 'string)
      (:void t)
      (otherwise
       (if (simple-type? c-type)
           c-type
           (error "In C-TYPE->LISP-TYPE: unrecognised c type `~S'." c-type))))))


(defmacro define-c-function ((foreign-fn-name fn-name) return-type args
                             &body body)
  "Format is similar to =CFFI:DEFCFUN=, except that:
1. The function arguments are wrapped in a set of outer parentheses.
2. Everything after this `arguments' term is considered to be the body
   of the wrapper function. Within this body, the macro =(call-it)=
   will call the actual C function. If =call-it= is called with no arguments,
   it will pass all the arguments given to the wrapper function, to the
   C function. Otherwise it will pass whatever arguments it is given, to
   the C function (similar to =call-next-method=).
3. If there is nothing after the function arguments, then the wrapper function
   body will automatically consist of a single call to the underlying
   C function."
  (let ((args-no-rest (remove '&rest args)))
    `(progn
       (defcfun (,foreign-fn-name ,(prepend-percent fn-name)) ,return-type
         ,@args)
       (declaim (inline ,fn-name))
       (defun* (,fn-name -> ,(c-type->lisp-type return-type))
           ,(mapcar #'(lambda (clause)
                        `(,(first clause) ,(c-type->lisp-type (second clause))
                           ,@(cddr clause)))
                    args-no-rest)
         ,@(if (stringp (car body)) (list (pop body)) nil)
         ,(if body
              `(macrolet ((call-it (&rest callargs)
                            (cons ',(prepend-percent fn-name)
                                  (or callargs '(,@(mapcar #'car args-no-rest))))))
                 ,@body)
              `(,(prepend-percent fn-name) ,@(mapcar #'car args-no-rest)))))))



(defmacro define-c-type (name foreign-type)
  "Define both a CFFI foreign type, and a corresponding lisp type of the same
name."
  `(progn
     (defctype ,name ,foreign-type)
     (deftype ,name () ',(c-type->lisp-type foreign-type))))




;;;; <<Types>> ================================================================


(deftype uint32 () `(unsigned-byte 32))
(deftype uint24 () `(unsigned-byte 24))
(deftype uint16 () `(unsigned-byte 16))
(deftype uint8 () `(unsigned-byte 8))
(deftype uint () `(unsigned-byte ,(* 8 (foreign-type-size :int))))
(deftype uchar () `(unsigned-byte ,(* 8 (foreign-type-size :unsigned-char))))

(deftype sint16 () `(signed-byte 16))

(deftype ucoord () `(integer 0 1000))


(define-c-type colournum :unsigned-int)


;; TCOD_color_t
;; This is seldom used -- colournums are used instead (see above).
(defcstruct colour
	(r :uint8)
	(g :uint8)
	(b :uint8))


;; TCOD_renderer_t (enum)
(define-c-enum renderer
  :RENDERER-GLSL
  :RENDERER-OPENGL
  :RENDERER-SDL)


;; TCOD_keycode_t (enum)
(define-c-enum keycode
	:NONE
	:ESCAPE
	:BACKSPACE
	:TAB
	:ENTER
	:SHIFT
	:CONTROL
	:ALT
	:PAUSE
	:CAPSLOCK
	:PAGEUP
	:PAGEDOWN
	:END
	:HOME
	:UP
	:LEFT
	:RIGHT
	:DOWN
	:PRINTSCREEN
	:INSERT
	:DELETE
	:LWIN
	:RWIN
	:APPS
	:key-0
	:key-1
	:key-2
	:key-3
	:key-4
	:key-5
	:key-6
	:key-7
	:key-8
	:key-9
	:KP0
	:KP1
	:KP2
	:KP3
	:KP4
	:KP5
	:KP6
	:KP7
	:KP8
	:KP9
	:KPADD
	:KPSUB
	:KPDIV
	:KPMUL
	:KPDEC
	:KPENTER
	:F1
	:F2
	:F3
	:F4
	:F5
	:F6
	:F7
	:F8
	:F9
	:F10
	:F11
	:F12
	:NUMLOCK
	:SCROLLLOCK
	:SPACE
	:CHAR)


;; TCOD_key_t
;; This is no longer used -- key structs are converted to a bitfield by
;; wrapper functions in libtcod.
(defcstruct key-press
	(vk keycode)     ; character if vk == TCODK_CHAR else 0
	(c :unsigned-char)
	(flags :uint8))  ; does this correspond to a key press or key
					; release event ?


(defstruct key
  "The structure used by CL-TCOD to represent key-press events. Corresponds
to the structure used by libtcod."
  (vk :none :type keyword)
  (c #\null :type character)
  (pressed nil :type boolean)
  (lalt nil :type boolean)
  (lctrl nil :type boolean)
  (ralt nil :type boolean)
  (rctrl nil :type boolean)
  (shift nil :type boolean))




(define-c-enum drawing-character
	(:CHAR-HLINE 196)
	(:CHAR-VLINE 179)
	(:CHAR-NE 191)
	(:CHAR-NW 218)
	(:CHAR-SE 217)
	(:CHAR-SW 192)
	(:CHAR-TEEW 180)
	(:CHAR-TEEE 195)
	(:CHAR-TEEN 193)
	(:CHAR-TEES 194)
	(:CHAR-CROSS 197)
	;; Double walls
	(:CHAR-DHLINE 205)
	(:CHAR-DVLINE 186)
	(:CHAR-DNE 187)
	(:CHAR-DNW 201)
	(:CHAR-DSE 188)
	(:CHAR-DSW 200)
	(:CHAR-DTEEW 181)
	(:CHAR-DTEEE 198)
	(:CHAR-DTEEN 208)
	(:CHAR-DTEES 210)
	(:CHAR-DCROSS 206)
	;; Blocks
	(:CHAR-BLOCK1 178)
	(:CHAR-BLOCK2 177)
	(:CHAR-BLOCK3 176)
	;; Arrows
	(:CHAR-ARROW-N 24)
	(:CHAR-ARROW-S 25)
	(:CHAR-ARROW-E 26)
	(:CHAR-ARROW_W 27)
	;; Arrows without tail
	(:CHAR-ARROW2-N 30)
	(:CHAR-ARROW2-S 31)
	(:CHAR-ARROW2-E 16)
	(:CHAR-ARROW2-W 17)
	;; Double arrows
	(:CHAR-DARROW2-H 29)
	(:CHAR-DARROW2-V 18)
	;; GUI stuff
	(:CHAR-CHECKBOX-UNSET 224)
	(:CHAR-CHECKBOX-SET 225)
	(:CHAR-RADIO-UNSET 9)
	(:CHAR-RADIO-SET 10)
	;; Subpixel resolution kit
	(:CHAR-SUBP-NW 226)
	(:CHAR-SUBP-NE 227)
	(:CHAR-SUBP-N 228)
	(:CHAR-SUBP-SE 229)
	(:CHAR-SUBP-DIAG 230)
	(:CHAR-SUBP-E 231)
	(:CHAR-SUBP-SW 232))


;; TCOD_colctrl_t (enum)
(define-c-enum colctrl
	(:COLCTRL-1 1)
	:COLCTRL-2
	:COLCTRL-3
	:COLCTRL-4
	:COLCTRL-5
	(:COLCTRL-NUMBER 5)
	:COLCTRL-FORE-RGB
	:COLCTRL-BACK-RGB
	:COLCTRL-STOP )

;; TCOD_bkgnd_flag_t (enum)
(define-c-enum background-flag
	:NONE
	:SET
	:MULTIPLY
	:LIGHTEN
	:DARKEN
	:SCREEN
	:COLOR-DODGE
	:COLOR-BURN
	:ADD
	:ADDA
	:BURN
	:OVERLAY
	:ALPH)


(define-c-enum alignment
	:LEFT
	:CENTER
	:RIGHT)


(define-c-bitfield key-state
	(:KEY-PRESSED 1)
	(:KEY-RELEASED 2))


(define-c-bitfield custom-font-flags
	(:FONT-LAYOUT-ASCII-IN-COL 1)
	(:FONT-LAYOUT-ASCII-IN-ROW 2)
	(:FONT-TYPE-GREYSCALE 4)
	(:FONT-LAYOUT-TCOD 8))


(define-c-enum noise-type
    (:NOISE-DEFAULT 0)
  (:NOISE-PERLIN 1)
  (:NOISE-SIMPLEX 2)
  (:NOISE-WAVELET 4))


(define-c-enum rng-algorithm
	:RNG-MT
	:RNG-CMWC)


(define-c-enum fov-algorithm
    :FOV-BASIC
    :FOV-DIAMOND
    :FOV-SHADOW
    :FOV-PERMISSIVE-0
    :FOV-PERMISSIVE-1
    :FOV-PERMISSIVE-2
    :FOV-PERMISSIVE-3
    :FOV-PERMISSIVE-4
    :FOV-PERMISSIVE-5
    :FOV-PERMISSIVE-6
    :FOV-PERMISSIVE-7
    :FOV-PERMISSIVE-8
    :FOV-RESTRICTIVE)


;; TCOD_console_t
(define-c-type console :pointer)


;; TCOD_random_t
(define-c-type randomptr :pointer)


;; TCOD_mouse_t
(defcstruct mouse-state
	(x :int)
	(y :int)
	(dx :int)
	(dy :int)
	(cx :int)
	(cy :int)
	(dcx :int)
	(dcy :int)
        (lbutton :boolean)
        (rbutton :boolean)
        (mbutton :boolean)
        (lbutton-pressed :boolean)
        (rbutton-pressed :boolean)
        (mbutton-pressed :boolean)
        (wheel-up :boolean)
        (wheel-down :boolean))

(defstruct mouse
  "Structure used by CL-TCOD to represent mouse status."
  (x 0 :type uint16) ;; absolute position
  (y 0 :type uint16)
  (dx 0 :type sint16) ;; movement since last update in pixels
  (dy 0 :type sint16)
  (cx 0 :type uint16) ;; cell coordinates in the root console
  (cy 0 :type uint16)
  (dcx 0 :type sint16)	;; movement since last update in console cells
  (dcy 0 :type sint16)
  (lbutton nil :type boolean)                ;; left button status
  (rbutton nil :type boolean)                ;; right button status
  (mbutton nil :type boolean)                ;; middle button status
  (lbutton-pressed nil :type boolean)        ;; left button pressed event
  (rbutton-pressed nil :type boolean)        ;; right button pressed event
  (mbutton-pressed nil :type boolean)        ;; middle button pressed event
  (wheel-up nil :type boolean)
  (wheel-down nil :type boolean))

;; TCOD_image_t
(define-c-type image :pointer)

(define-c-type noise :pointer)

(define-c-type heightmap :pointer)

(define-c-type mapptr :pointer)

(define-c-type a*-path :pointer)

(define-c-type dijkstra-path :pointer)


;;;; <<Utilities>> ============================================================


(defun* (get-bit -> boolean) ((n integer) (pos uint8))
  "Return the bit at position POS within the integer N (represented as
a bitfield). POS = 1 refers to the 1's (rightmost) bit."
  (/= 0 (logand n (expt 2 (1- pos)))))



(defvar *root* (null-pointer) "The root console.")
(defparameter +NULL+ (null-pointer) "The null pointer.")
(defconstant +NOISE-DEFAULT-HURST+ 0.5
  "Default Hurst exponent for noise functions.")
(defconstant +NOISE-DEFAULT-LACUNARITY+ 2.0
  "Default lacunarity for noise functions.")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simple-type? (sym)
    "* Arguments
- SYM :: A symbol.
* Return Value
Boolean.
* Description
Returns =T= if =SYM= names a non-class type, such as can be
defined by [[deftype]]."
    (handler-case (typep t sym)
      (error () (return-from simple-type? nil)))
    t))



(defun* (make-simple-key -> key) ((ch character))
  (make-key :vk :char :c ch))


(defun* (same-keys? -> boolean) ((key1 key) (key2 key))
  (and (key-p key1) (key-p key2)
       (eql (key-vk key1) (key-vk key2))
       (eql (key-c key1) (key-c key2))
       (eql (key-shift key1) (key-shift key2))
       (eql (or (key-lalt key1) (key-ralt key1))
	    (or (key-lalt key2) (key-ralt key2)))
       (eql (or (key-lctrl key1) (key-rctrl key1))
	    (or (key-lctrl key2) (key-rctrl key2)))))



;;;; <<Colours>> ==============================================================


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* (compose-colour -> uint32) ((r uint8) (g uint8) (b uint8))
    "Given three integer values R, G and B, representing the red, green and
blue components of a colour, return a 3 byte integer whose value is #xBBGGRR."
    (+ (ash b 16) (ash g 8) r))
  (declaim (inline compose-color))
  (defun compose-color (r g b) (compose-colour r g b)))


(defun* (decompose-colour -> (values uint8 uint8 uint8)) ((num colournum))
  "Given a colournum #xBBGGRR, return R, G and B integer values
as 3 separate return values."
  (values
	 (logand num #x0000ff)
	 (ash (logand num #x00ff00) -8)
	 (ash (logand num #xff0000) -16)
         ))
(declaim (inline decompose-color))
(defun decompose-color (num) (decompose-colour num))


(defun* (invert-colour -> colournum) ((num colournum))
  (multiple-value-bind (r g b) (decompose-colour num)
    (compose-colour (- 255 r) (- 255 g) (- 255 b))))
(declaim (inline invert-color))
(defun invert-color (num) (invert-colour num))


(defvar *colour-table* nil)
(defvar *initial-colours*
  `((:true-black	#x00 #x00 #x00)
    (:true-pink		#xFF #x00 #xFF)
    (:true-white	#xFF #xFF #xFF)
    (:true-red		#xFF #x00 #x00)
    (:true-green	#x00 #xFF #x00)
    (:true-blue		#x00 #x00 #xFF)
    (:black		#x00 #x00 #x00)
    (:dark-grey 	96 96 96)
    (:grey 		196 196 196)
    (:white 		255 255 255)
    (:blue		13 103 196)
    (:dark-blue 	40 40 128)
    (:light-blue 	120 120 255)
    (:dark-red 		128 0 0)
    (:light-red 	255 100 50)
    (:dark-brown 	32 16 0)
    (:light-yellow 	255 255 150)
    (:yellow 		255 255 0)
    (:dark-yellow 	164 164 0)
    (:green 		0 220 0)
    (:cyan		86 163 205)
    (:orange 		255 150 0)
    (:red 		255 0 0)
    (:silver 		203 203 203)
    (:gold 		255 255 102)
    (:purple 		204 51 153)
    (:dark-purple 	51 0 51)
    ;; Some colours not defined in TCOD.
    (:slate-grey 	#x80 #x80 #x80)
    (:umber 		#x80 #x40 0)
    (:pink 		#xFF #x00 #xFF)
    (:chocolate 	210 105 30)))


;;#define TCOD_BKGND_ALPHA(alpha)
;;    ((TCOD_bkgnd_flag_t)(TCOD_BKGND_ALPH|(((uint8)(alpha*255))<<8)))
(defun background-alpha (alpha)
  (foreign-enum-keyword 'background-flag
			(logior (foreign-enum-value 'background-flag :alph)
				(ash (mod (* alpha 255) 256) 8))))

;;
;;#define TCOD_BKGND_ADDALPHA(alpha)
;;    ((TCOD_bkgnd_flag_t)(TCOD_BKGND_ADDA|(((uint8)(alpha*255))<<8)))
(defun background-add-alpha (alpha)
  (foreign-enum-keyword 'background-flag
			(logior (foreign-enum-value 'background-flag :adda)
				(ash (mod (* alpha 255) 256) 8))))


(defun start-colours ()
  (setf *colour-table* (make-hash-table :test #'eql))
  (dolist (term *initial-colours*)
    (destructuring-bind (name r g b) term
      (make-colour name r g b)))
  (make-rgb.txt-colours))
(defun start-colors () (start-colours))


(defun make-colour (kwd r g b)
  (unless (hash-table-p *colour-table*)
    (start-colours))
  (setf (gethash kwd *colour-table*)
	(compose-colour r g b)))
(defun make-color (kwd r g b) (make-colour kwd r g b))


(defun colour->grayscale (col)
  (multiple-value-bind (r g b) (decompose-colour col)
    (let ((brightness (round (+ r g b) 3)))
      (compose-colour brightness brightness brightness))))
(defun color->grayscale (col) (colour->grayscale col))


(defun* (colour -> colournum) ((keywd (or colournum symbol)))
  "Given a colour keyword such as :GREY, return its corresponding RGB
value (#xRRGGBB)."
  (cond
    ((integerp keywd)
     keywd)
    (t
     (unless *colour-table*
       (start-colours))
     (gethash keywd *colour-table*))))
(declaim (inline color))
(defun color (keywd) (colour keywd))


(defun colctrl->char (ctrl)
  (code-char (foreign-enum-value 'colctrl ctrl)))



;; TCODLIB_API bool TCOD_color_equals (TCOD_color_t c1, TCOD_color_t c2);
(define-c-function ("TCOD_color_equals_wrapper" colour-equals?) :boolean
	((c1 colournum) (c2 colournum)))
(declaim (inline color-equals?))
(defun color-equals? (c1 c2)
  (colour-equals? c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_add (TCOD_color_t c1, TCOD_color_t c2);
(define-c-function ("TCOD_color_add_wrapper" colour-add) colournum
	((c1 colournum) (c2 colournum)))
(declaim (inline color-add))
(defun color-add (c1 c2)
  (colour-add c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply (TCOD_color_t c1,
;; TCOD_color_t c2);
(define-c-function ("TCOD_color_multiply_wrapper" colour-multiply) colournum
	((c1 colournum) (c2 colournum)))
(declaim (inline color-multiply))
(defun color-multiply (c1 c2)
  (colour-multiply c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply_scalar (TCOD_color_t c1,
;; float value);
(define-c-function ("TCOD_color_multiply_scalar_wrapper" colour-multiply-scalar)
    colournum
    ((c1 colournum) (value :float)))
(declaim (inline color-multiply-scalar))
(defun color-multiply-scalar (c1 value)
  (colour-multiply-scalar c1 value))


;; TCODLIB_API TCOD_color_t TCOD_color_lerp(TCOD_color_t c1, TCOD_color_t c2,
;; float coef);
(define-c-function ("TCOD_color_lerp_wrapper" colour-lerp) colournum
	((c1 colournum) (c2 colournum) (coef :float)))
(declaim (inline color-lerp))
(defun color-lerp (c1 c2 coef)
  (colour-lerp c1 c2 coef))


;; TCODLIB_API void TCOD_color_set_HSV(TCOD_color_t *c,float h, float s,
;; float v);
(define-c-function ("TCOD_color_set_HSV" colour-set-hsv) :void
	((con :pointer) (hue :float) (sat :float) (v :float)))
(declaim (inline color-set-hsv))
(defun color-set-hsv (con hue sat v)
  (colour-set-hsv con hue sat v))


(define-c-function ("TCOD_color_get_hue_" colour-get-hue) :int
    ((c colournum)))

(define-c-function ("TCOD_color_get_saturation_" colour-get-saturation) :int
    ((c colournum)))

(define-c-function ("TCOD_color_get_value_" colour-get-value) :int
    ((c colournum)))


(defun* (colour-get-hsv -> list) ((c colournum))
  (list (colour-get-hue c)
        (colour-get-saturation c)
        (colour-get-value c)))

(declaim (inline color-get-hsv color-get-hue color-get-saturation
                 color-get-value))

(defun color-get-hsv (colour)
  (colour-get-hsv colour))

(defun color-get-hue (colour)
  (colour-get-hue colour))

(defun color-get-saturation (colour)
  (colour-get-hue colour))

(defun color-get-value (colour)
  (colour-get-hue colour))



;;;; <<Console>> ==============================================================


(defvar *console-width-table* (make-hash-table))
(defvar *console-height-table* (make-hash-table))


;; (defun* (legal-console-coordinates? -> boolean) ((con console) (x fixnum) (y fixnum))
;;   (and (< x (console-get-width con))
;;        (< y (console-get-height con))))

(defmacro legal-console-coordinates? (con x y)
  "Are the relative coordinates X,Y within the bounds of console CON?"
  `(and (< ,x (console-get-width ,con))
        (< ,y (console-get-height ,con))))


(define-c-function ("TCOD_console_credits" console-credits) :void
    ())

(define-c-function ("TCOD_console_credits_reset" console-credits-reset) :void
    ())

(define-c-function ("TCOD_console_credits_render" console-credits-render) :boolean
  ((x :int) (y :int) (alpha :boolean)))


;;TCODLIB_API void TCOD_console_init_root(int w, int h, const char * title,
;;                                        bool fullscreen);
(define-c-function ("TCOD_console_init_root" console-init-root) :void
    ((width :int) (height :int) (title :string) (fullscreen? :boolean)
     (renderer renderer))
  (check-type width ucoord)
  (check-type height ucoord)
  (setf (gethash *root* *console-width-table*) width)
  (setf (gethash *root* *console-height-table*) height)
  (call-it))


;;TCODLIB_API void TCOD_console_set_custom_font(const char *fontFile,
;;                        int char_width, int char_height, int nb_char_horiz,
;;                        int nb_char_vertic, bool chars_by_row,
;;                        TCOD_color_t key_color);
(define-c-function ("TCOD_console_set_custom_font" console-set-custom-font) :void
    ((fontfile :string) (flags custom-font-flags)
     (chars-horizontal :int) (chars-vertical :int))
  (assert (probe-file fontfile))
  (check-type chars-horizontal (unsigned-byte 16))
  (check-type chars-vertical (unsigned-byte 16))
  (call-it))


;; TCODLIB_API void TCOD_console_map_ascii_code_to_font(int asciiCode,
;;      int fontCharX, int fontCharY);
(define-c-function ("TCOD_console_map_ascii_code_to_font"
                    console-map-ascii-code-to-font) :void
    ((asciicode :int) (fontchar-x :int) (fontchar-y :int)))

;; TCODLIB_API void TCOD_console_map_ascii_codes_to_font(int asciiCode,
;;      int nbCodes, int fontCharX, int fontCharY);
(define-c-function ("TCOD_console_map_ascii_codes_to_font"
                    console-map-ascii-codes-to-font) :void
    ((asciicode :int) (num-codes :int) (fontchar-x :int) (fontchar-y :int)))

;; TCODLIB_API void TCOD_console_map_string_to_font(const char *s,
;;      int fontCharX, int fontCharY);
(define-c-function ("TCOD_console_map_string_to_font"
                    console-map-string-to-font) :void
    ((str :string) (fontchar-x :int) (fontchar-y :int)))


;;TCODLIB_API void TCOD_console_set_window_title(const char *title);
(define-c-function ("TCOD_console_set_window_title" console-set-window-title) :void
  ((title :string)))

;;TCODLIB_API void TCOD_console_set_fullscreen(bool fullscreen);
(define-c-function ("TCOD_console_set_fullscreen" console-set-fullscreen) :void
  ((full? :boolean)))

;;TCODLIB_API bool TCOD_console_is_fullscreen();
(define-c-function ("TCOD_console_is_fullscreen" console-is-fullscreen?) :boolean
    ())

;;TCODLIB_API bool TCOD_console_is_window_closed();
(define-c-function ("TCOD_console_is_window_closed" console-is-window-closed?) :boolean
    ())


;;TCODLIB_API void TCOD_console_set_background_color(TCOD_console_t con,
;; TCOD_color_t col);
(define-c-function ("TCOD_console_set_default_background_wrapper"
	  console-set-default-background) :void
	((con console) (col colournum)))
(declaim (inline console-set-default-background))


;;TCODLIB_API void TCOD_console_set_foreground_color(TCOD_console_t con,
;;                                                   TCOD_color_t col);
(define-c-function ("TCOD_console_set_default_foreground_wrapper"
	  console-set-default-foreground) :void
	((con console) (col colournum)))
(declaim (inline console-set-default-foreground))


;;TCODLIB_API TCOD_color_t TCOD_console_get_background_color(TCOD_console_t
;;con);
(define-c-function ("TCOD_console_get_default_background_wrapper"
                    console-get-default-background) colournum
  ((con console)))
(declaim (inline console-get-default-background))


;;TCODLIB_API TCOD_color_t TCOD_console_get_foreground_color(TCOD_console_t con);
(define-c-function ("TCOD_console_get_default_foreground_wrapper"
                    console-get-default-foreground) colournum
  ((con console)))
(declaim (inline console-get-default-foreground))


;;TCODLIB_API void TCOD_console_clear(TCOD_console_t con);
(define-c-function ("TCOD_console_clear" console-clear) :void
	((con console)))


;; New in 1.5.0rc1
;;TCODLIB_API void TCOD_console_set_dirty(int dx, int dy, int dw, int dh);
(define-c-function ("TCOD_console_set_dirty" console-set-dirty) :void
  ((rootx :int) (rooty :int) (width :int) (height :int))
  "Declares an area of the =*root*= console to be 'dirty'."
  (assert (legal-console-coordinates? *root* rootx rooty))
  (assert (legal-console-coordinates?
           *root* (+ rootx (1- width)) (+ rooty (1- height))))
  (call-it))


;;TCODLIB_API TCOD_color_t TCOD_console_get_back(TCOD_console_t con,int x, int y)
(define-c-function ("TCOD_console_get_char_background_wrapper"
                    console-get-char-background) colournum
  ((con console) (x :int) (y :int))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API TCOD_color_t TCOD_console_get_fore(TCOD_console_t con,
;;                                               int x, int y);
(define-c-function ("TCOD_console_get_char_foreground_wrapper"
                    console-get-char-foreground) colournum
  ((con console) (x :int) (y :int))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_set_back(TCOD_console_t con,int x, int y,
;;                                       TCOD_color_t col,
;;                                       TCOD_bkgnd_flag_t flag);
(define-c-function ("TCOD_console_set_char_background_wrapper"
                    console-set-char-background) :void
    ((con console) (x :int) (y :int) (col colournum) (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it con x y col flag))


;;TCODLIB_API void TCOD_console_set_fore(TCOD_console_t con,int x, int y,
;;                                       TCOD_color_t col);
(define-c-function ("TCOD_console_set_char_foreground_wrapper"
                    console-set-char-foreground) :void
    ((con console) (x :int) (y :int) (col colournum))
  (assert (legal-console-coordinates? con x y))
  (call-it con x y col))


;;TCODLIB_API void TCOD_console_set_char(TCOD_console_t con,int x, int y,
;; int c);
(defcfun ("TCOD_console_set_char" %console-set-char) :void
    (con console) (x :int) (y :int) (ch :unsigned-char))


(defun* console-set-char ((con console) (x integer) (y integer) ch)
  (assert (legal-console-coordinates? con x y))
  (when (characterp ch)
    (setf ch (char-code ch)))
  (%console-set-char con x y ch))


(defun* (console-fill-char -> null)  ((con console) (ch (or character uchar))
                                      (fx ucoord) (fy ucoord)
                                      (fw ucoord) (fh ucoord))
  "Fill a rectangular area with the character CH."
  (if (characterp ch)
      (setf ch (char-code ch)))
  (loop for x from fx below (+ fx fw) do
       (loop for y from fy below (+ fy fh) do
            (when (legal-console-coordinates? con x y)
            (console-set-char con x y ch)))))


;;TCODLIB_API void TCOD_console_put_char(TCOD_console_t con,int x, int y,
;;                                       int c, TCOD_bkgnd_flag_t flag);
(define-c-function ("TCOD_console_put_char" console-put-char) :void
    ((con console) (x :int) (y :int) (ch :unsigned-char)
     (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


(define-c-function ("TCOD_console_put_char_ex_wrapper" console-put-char-ex) :void
    ((con console) (x :int) (y :int) (ch :unsigned-char) (fg colournum)
     (bg colournum))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_print_left(TCOD_console_t con,int x, int y,
;;                                         TCOD_bkgnd_flag_t flag,
;;                                         const char *fmt, ...);

;; This has to have a separate lisp wrapper, as we need to be able
;; to pass 'args...' to lisp.
;; (defcfun ("TCOD_console_print_left" %console-print-left) :void
;;   (con console) (x :int) (y :int) (flag background-flag) (fmt :string)
;;   &rest)
;;
;; (defun* console-print-left ((con console) (x ucoord) (y ucoord)
;;                             (flag background-flag) (fmt string)
;;                             &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-left con x y flag (apply #'format nil fmt args)))


(define-c-function ("TCOD_console_set_background_flag"
                    console-set-background-flag) :void
    ((con console) (flag background-flag))
  (call-it))


(define-c-function ("TCOD_console_get_background_flag"
                    console-get-background-flag) background-flag
    ((con console))
  (call-it))


(define-c-function ("TCOD_console_set_alignment"
                    console-set-alignment) :void
    ((con console) (align alignment))
  (call-it))


(define-c-function ("TCOD_console_get_alignment"
                    console-get-alignment) alignment
    ((con console))
  (call-it))


(defcfun ("TCOD_console_print" %console-print) :void
  (con console) (x :int) (y :int) (fmt :string) &rest)


(defun* console-print ((con console) (x ucoord) (y ucoord)
                            (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (%console-print con x y (apply #'format nil fmt args)))


(defcfun ("TCOD_console_print_ex" %console-print-ex) :void
  (con console) (x :int) (y :int) (flag background-flag)
  (align alignment) (fmt :string) &rest)


(defun* console-print-ex ((con console) (x ucoord) (y ucoord)
                          (flag background-flag) (align alignment)
                            (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (%console-print-ex con x y flag align
                     (apply #'format nil fmt args)))


;; In wrapper.c
(define-c-function ("TCOD_console_print_return_string"
                    console-print-return-string) :string
    ((con console) (x :int) (y :int) (rw :int) (rh :int)
     (flag background-flag) (align alignment) (str :string)
     (can-split? :boolean) (count-only? :boolean))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_print_right(TCOD_console_t con,int x, int y,
;; TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_right" %console-print-right) :void
;; 	(con console) (x :int) (y :int) (flag background-flag) (fmt :string)
;; 	&rest)
;;
;; (defun* console-print-right ((con console) (x ucoord) (y ucoord)
;;                              (flag background-flag) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-right con x y flag (apply #'format nil fmt args)))

;;TCODLIB_API void TCOD_console_print_center(TCOD_console_t con,int x, int y,
;; TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_center" %console-print-centre) :void
;; 	(con console) (x :int) (y :int) (flag background-flag) (fmt :string)
;; 	&rest)
;;
;; (defun* console-print-centre ((con console) (x ucoord) (y ucoord)
;;                               (flag background-flag) (fmt string)
;;                               &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-centre con x y flag (apply #'format nil fmt args)))
;;
;; (declaim (inline console-print-center))
;; (defun console-print-center (con x y flag fmt &rest args)
;;   (apply #'console-print-centre con x y flag fmt args))


;;TCODLIB_API int TCOD_console_print_left_rect(TCOD_console_t con,int x, int y,
;; int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_left_rect" %console-print-left-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int)
;; 	(flag background-flag) (fmt :string)
;; 	&rest)
;;
;; (defun* console-print-left-rect ((con console) (x ucoord) (y ucoord)
;;                                  (w ucoord) (h ucoord)
;;                                  (flag background-flag) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-left-rect con x y w h flag
;;                             (apply #'format nil fmt args)))

;;TCODLIB_API int TCOD_console_print_right_rect(TCOD_console_t con,int x,
;; int y, int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_right_rect" %console-print-right-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int)
;; 	(flag background-flag) (fmt :string)
;; 	&rest)
;;
;; (defun* console-print-right-rect ((con console) (x ucoord) (y ucoord)
;;                                   (w ucoord) (h ucoord)
;;                                   (flag background-flag) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-right-rect con x y w h flag
;;                              (apply #'format nil fmt args)))


(defcfun ("TCOD_console_print_rect" %console-print-rect) :int
	(con console) (x :int) (y :int) (w :int) (h :int)
	(fmt :string) &rest)

(defun* console-print-rect ((con console) (x ucoord) (y ucoord)
                                  (w ucoord) (h ucoord)
                                  (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (%console-print-rect con x y w h (apply #'format nil fmt args)))


(defcfun ("TCOD_console_print_rect_ex" %console-print-rect-ex) :int
	(con console) (x :int) (y :int) (w :int) (h :int)
	(flag background-flag) (align alignment) (fmt :string) &rest)

(defun* console-print-rect-ex ((con console) (x ucoord) (y ucoord)
                               (w ucoord) (h ucoord)
                               (flag background-flag) (align alignment)
                               (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (%console-print-rect-ex con x y w h flag align
                          (apply #'format nil fmt args)))


;;TCODLIB_API int TCOD_console_print_center_rect(TCOD_console_t con,int x,
;; int y, int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_center_rect" %console-print-centre-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int)
;; 	(flag background-flag) (fmt :string)
;; 	&rest)
;;
;; (defun* console-print-centre-rect ((con console) (x ucoord) (y ucoord)
;;                                    (w ucoord) (h ucoord)
;;                                    (flag background-flag) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-centre-rect con x y w h flag
;;                               (apply #'format nil fmt args)))
;;
;; (declaim (inline console-print-center-rect))
;; (defun console-print-center-rect (con x y w h flag fmt &rest args)
;;   (apply #'console-print-centre-rect con x y w h flag fmt args))



;;TCODLIB_API void TCOD_console_rect(TCOD_console_t con,int x, int y, int w,
;; int h, bool clear, TCOD_bkgnd_flag_t flag);
(define-c-function ("TCOD_console_rect" console-rect) :void
  ((con console) (x :int) (y :int) (width :int) (height :int) (clear? :boolean)
   (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (check-type width ucoord)
  (check-type height ucoord)
  (call-it))



(defcfun ("TCOD_console_get_height_rect" %console-get-height-rect) :int
	(con console) (x :int) (y :int) (w :int) (h :int) (fmt :string)
	&rest)

(defun* console-get-height-rect ((con console) (x ucoord) (y ucoord)
                                 (w ucoord) (h ucoord) (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (%console-get-height-rect con x y w h (apply #'format nil fmt args)))

;;TCODLIB_API int TCOD_console_height_left_rect(TCOD_console_t con,
;;     int x, int y, int w, int h, const char *fmt, ...);

;; (defcfun ("TCOD_console_height_left_rect" %console-height-left-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int) (fmt :string)
;; 	&rest)
;;
;; (defun* console-height-left-rect ((con console) (x ucoord) (y ucoord)
;;                                  (w ucoord) (h ucoord) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-height-left-rect con x y w h (apply #'format nil fmt args)))

;;TCODLIB_API int TCOD_console_height_right_rect(TCOD_console_t con,
;;     int x, int y, int w, int h, const char *fmt, ...);

;; (defcfun ("TCOD_console_height_right_rect" %console-height-right-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int) (fmt :string)
;; 	&rest)
;;
;; (defun* console-height-right-rect ((con console) (x ucoord) (y ucoord)
;;                                  (w ucoord) (h ucoord) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-height-right-rect con x y w h (apply #'format nil fmt args)))

;;TCODLIB_API int TCOD_console_height_center_rect(TCOD_console_t con,
;;     int x, int y, int w, int h, const char *fmt, ...);

;; (defcfun ("TCOD_console_height_center_rect" %console-height-centre-rect) :int
;; 	(con console) (x :int) (y :int) (w :int) (h :int) (fmt :string)
;; 	&rest)
;;
;; (defun* console-height-centre-rect ((con console) (x ucoord) (y ucoord)
;;                                  (w ucoord) (h ucoord) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-height-centre-rect con x y w h (apply #'format nil fmt args)))
;;
;; (declaim (inline console-height-center-rect))
;; (defun console-height-center-rect (con x y w h fmt &rest args)
;;   (apply #'console-height-centre-rect con x y w h fmt args))


;;TCODLIB_API void TCOD_console_hline(TCOD_console_t con,int x,int y, int l,
;; TCOD_bkgnd_flag_t flag);

(define-c-function ("TCOD_console_hline" console-hline) :void
    ((con console) (x :int) (y :int) (len :int) (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_vline(TCOD_console_t con,int x,int y, int l,
;; TCOD_bkgnd_flag_t flag);

(define-c-function ("TCOD_console_vline" console-vline) :void
    ((con console) (x :int) (y :int) (len :int) (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_print_frame(TCOD_console_t con,int x,int y,
;; int w,int h, bool empty, const char *fmt, ...);
;;#-libtcod-old
(defcfun ("TCOD_console_print_frame" %console-print-frame) :void
  (con console) (x :int) (y :int) (width :int) (height :int)
  (empty? :boolean) (flag background-flag)
  (fmt :string) &rest)

(defun* console-print-frame ((con console) (x ucoord) (y ucoord)
                             (width ucoord) (height ucoord)
                             (empty? boolean) (flag background-flag)
                             (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (check-type width ucoord)
  (check-type height ucoord)
  (%console-print-frame con x y width height empty? flag
                        (apply #'format nil fmt args)))


;; Added in wrappers.c
(defcfun ("TCOD_console_print_double_frame" %console-print-double-frame) :void
  (con console) (x :int) (y :int) (width :int) (height :int)
  (empty? :boolean) (flag background-flag)
  (fmt :string) &rest)

(defun* console-print-double-frame ((con console) (x ucoord) (y ucoord)
                                    (width ucoord) (height ucoord)
                                    (empty? boolean) (flag background-flag)
                                    (fmt string) &rest args)
  (assert (legal-console-coordinates? con x y))
  (check-type width ucoord)
  (check-type height ucoord)
  (%console-print-double-frame con x y width height empty? flag
                               (apply #'format nil fmt args)))



;;TCODLIB_API int TCOD_console_get_char(TCOD_console_t con,int x, int y);
(define-c-function ("TCOD_console_get_char" console-get-char) :int
  ((con console) (x :int) (y :int))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_console_set_fade(uint8 val, TCOD_color_t fade);
(define-c-function ("TCOD_console_set_fade_wrapper" console-set-fade) :void
  ((val :uint8) (fade colournum)))

;;TCODLIB_API uint8 TCOD_console_get_fade();
(define-c-function ("TCOD_console_get_fade" console-get-fade) :uint8
    ())


;;TCODLIB_API TCOD_color_t TCOD_console_get_fading_color();
(define-c-function ("TCOD_console_get_fading_color_wrapper"
	  console-get-fading-color) colournum
    ())
(declaim (inline console-get-fading-colour))
(defun console-get-fading-colour ()
  (console-get-fading-color))


;;TCODLIB_API void TCOD_console_flush();
(define-c-function ("TCOD_console_flush" console-flush) :void
    ())


;;TCODLIB_API void TCOD_console_set_color_control(TCOD_colctrl_t con,
;;     TCOD_color_t fore, TCOD_color_t back);
;; This is to do with "colour control" strings
(define-c-function ("TCOD_console_set_color_control_wrapper"
	  console-set-colour-control) :void
  ((control-num colctrl) (fore colournum) (back colournum)))

(declaim (inline console-set-color-control))
(defun console-set-color-control (control-num fore back)
  (console-set-colour-control control-num fore back))



;;TCODLIB_API TCOD_console_t TCOD_console_new(int w, int h);
(define-c-function ("TCOD_console_new" console-new) console
    ((width :int) (height :int))
  (check-type width ucoord)
  (check-type height ucoord)
  (let ((newcon (call-it width height)))
    (setf (gethash newcon *console-width-table*) width)
    (setf (gethash newcon *console-height-table*) height)
    newcon))


;; (defun* (console-new -> console) ((width ucoord) (height ucoord))
;;   (let ((newcon (%console-new width height)))
;;     (setf (gethash newcon *console-width-table*) width)
;;     (setf (gethash newcon *console-height-table*) height)
;;     newcon))


;;TCODLIB_API int TCOD_console_get_width(TCOD_console_t con);
(define-c-function ("TCOD_console_get_width" console-get-width) :int
  ((con console))
  (or (gethash con *console-width-table*)
      (call-it)))


;; (defun* (console-get-width -> ucoord) ((con console))
;;   (or (gethash con *console-width-table*)
;;       (%console-get-width con)))


;;TCODLIB_API int TCOD_console_get_height(TCOD_console_t con);
(define-c-function ("TCOD_console_get_height" console-get-height) :int
  ((con console))
  (or (gethash con *console-height-table*)
      (call-it)))


;; (defun* (console-get-height -> ucoord) ((con console))
;;   (or (gethash con *console-height-table*)
;;       (%console-get-height con)))


;;TCODLIB_API void TCOD_console_blit(TCOD_console_t src,int xSrc, int ySrc,
;; int wSrc, int hSrc, TCOD_console_t dst, int xDst, int yDst, int fade);
(define-c-function ("TCOD_console_blit" console-blit) :void
    ((src console)
     (xsrc :int) (ysrc :int)
     (wsrc :int) (hsrc :int)
     (dest console)
     (xdest :int) (ydest :int)
     (foreground-alpha :float) (background-alpha :float))
  (check-type xsrc ucoord)
  (check-type ysrc ucoord)
  (check-type wsrc ucoord)
  (check-type hsrc ucoord)
  (check-type xdest ucoord)
  (check-type ydest ucoord)
  (check-type foreground-alpha (real 0 1.0))
  (check-type background-alpha (real 0 1.0))
  ;; Blitting a console to a position that lies completely outside the
  ;; destination console's bounds will do nothing, rather than causing
  ;; an error.
  (when (legal-console-coordinates? dest xdest ydest)
    ;; TCOD_console_blit unceremoniously crashes libtcod if this assertion is
    ;; not true when it is called. We therefore check the assertion here first,
    ;; so we have access to lisp debugging facilities if the conditions are not
    ;; met.
    (assert (and (plusp wsrc) (plusp hsrc)
                 (>= (+ xdest wsrc) 0) (>= (+ ydest hsrc) 0)))
    (call-it src xsrc ysrc wsrc hsrc dest xdest ydest
             foreground-alpha background-alpha)))




;; (defun* console-blit ((src console)
;;                       (xsrc fixnum) (ysrc fixnum) (wsrc fixnum) (hsrc fixnum)
;;                       (dest console)
;;                       (xdest fixnum) (ydest fixnum)
;;                       (foreground-alpha float) (background-alpha float))
;;   (check-type xsrc (integer 0))
;;   (check-type ysrc (integer 0))
;;   (check-type wsrc (integer 0))
;;   (check-type hsrc (integer 0))
;;   (check-type xdest (integer 0))
;;   (check-type ydest (integer 0))
;;   (check-type foreground-alpha (real 0 1.0))
;;   (check-type background-alpha (real 0 1.0))
;;   ;; Blitting a console to a position that lies completely outside the
;;   ;; destination console's bounds will do nothing, rather than causing
;;   ;; an error.
;;   (unless (or (>= xdest (console-get-width dest))
;;               (>= ydest (console-get-height dest)))
;;     ;; TCOD_console_blit unceremoniously crashes libtcod if this assertion
;;     ;; is not true when it is called. We therefore check the assertion here
;;     ;; first, so we have access to debugging facilities if the conditions
;;     ;; are not met.
;;     (assert (and (plusp wsrc) (plusp hsrc)
;;                  (>= (+ xdest wsrc) 0) (>= (+ ydest hsrc) 0)))
;;     (%console-blit src xsrc ysrc wsrc hsrc dest xdest ydest
;;                    foreground-alpha background-alpha)))


;;TCODLIB_API void TCOD_console_delete(TCOD_console_t console);
(define-c-function ("TCOD_console_delete" console-delete) :void
    ((con console)))


;; void TCOD_console_set_key_color(TCOD_console_t con,TCOD_color_t col);
;; (define-c-function ("TCOD_console_set_key_color" console-set-key-color) :void
;;     ((con console)))



;;;; <<Keyboard input>> ========================================================


;; (defun key->keypress (keyptr)
;;   (let ((flags (foreign-slot-value keyptr 'key-press 'flags)))
;;     (make-key :vk (foreign-slot-value keyptr 'key-press 'vk)
;; 	      :c (code-char (foreign-slot-value keyptr 'key-press 'c))
;; 	      :pressed (get-bit flags 1)
;; 	      :lalt (get-bit flags 2)
;; 	      :lctrl (get-bit flags 3)
;; 	      :ralt (get-bit flags 4)
;; 	      :rctrl (get-bit flags 5)
;; 	      :shift (get-bit flags 6))))

(defmacro and& (a b)
  "Shorthand for (BOOLE 'BOOLE-AND A B)."
  `(boole boole-and ,a ,b))


(defun key-bitfield->vk (key-bf)
  (foreign-enum-keyword 'keycode
                        (and& (ash key-bf -16) #x00FF)))


(defun* key->keypress ((key-bf (unsigned-byte 32)))
  (let ((flags (ash key-bf -24)))
    (make-key :vk (key-bitfield->vk key-bf) ;;(ldb (byte 8 16) key-bf)
              :c (code-char (and& key-bf #x0000FFFF))  ;;(ldb (byte 16 0) key-bf)
	      :pressed (get-bit flags 1)
	      :lalt (get-bit flags 2)
	      :lctrl (get-bit flags 3)
	      :ralt (get-bit flags 4)
	      :rctrl (get-bit flags 5)
	      :shift (get-bit flags 6))))


;;TCODLIB_API TCOD_key_t TCOD_console_check_for_keypress(int flags);
(defcfun ("TCOD_console_check_for_keypress_bitfield"
          %console-check-for-keypress) :int
    (flags key-state))


(defun* (console-check-for-keypress -> (or null key)) ((flags key-state))
  (let ((key-bf (%console-check-for-keypress flags)))
    (if (eql (key-bitfield->vk key-bf) :none)
        nil
        (key->keypress key-bf))))


;; (defun* console-check-for-keypress ((flags key-state))
;;   ;; (unless *key*
;;   ;;   (setf *key* (foreign-alloc 'key-press)))
;;   (with-foreign-object (key 'key-press)
;;     (%console-check-for-keypress key flags)
;;     (unless (eql :none (foreign-slot-value key 'key-press 'vk))
;;       (key->keypress key))))


;;TCODLIB_API TCOD_key_t TCOD_console_wait_for_keypress(bool flush);
(defcfun ("TCOD_console_wait_for_keypress_bitfield"
          %console-wait-for-keypress) :int
  (flush? :boolean))


(defun* console-wait-for-keypress ((flush? boolean))
  (key->keypress (%console-wait-for-keypress flush?)))



;; (defun* console-wait-for-keypress ((flush? boolean))
;;   ;; (unless *key*
;;   ;;   (setf *key* (foreign-alloc 'key-press)))
;;   (with-foreign-object (key 'key-press)
;;     (%console-wait-for-keypress key flush?)
;;     (key->keypress key)))


;;TCODLIB_API void TCOD_console_set_keyboard_repeat(int initial_delay,
;; int interval);
(define-c-function ("TCOD_console_set_keyboard_repeat"
                    console-set-keyboard-repeat) :void
    ((initial-delay :int) (interval :int)))


;;TCODLIB_API void TCOD_console_disable_keyboard_repeat();
(define-c-function ("TCOD_console_disable_keyboard_repeat"
                    console-disable-keyboard-repeat) :void
    ())


;;TCODLIB_API bool TCOD_console_is_key_pressed(TCOD_keycode_t key);
(define-c-function ("TCOD_console_is_key_pressed" is-key-pressed?) :boolean
  ((code keycode)))


;;;; <<System>> ===============================================================


;;TCODLIB_API uint32 TCOD_sys_elapsed_milli();
;;TCODLIB_API float TCOD_sys_elapsed_seconds();
;;TCODLIB_API void TCOD_sys_sleep_milli(uint32 val);
(define-c-function ("TCOD_sys_sleep_milli" sys-sleep-milli) :void
    ((val :unsigned-int)))

;;TCODLIB_API void TCOD_sys_save_screenshot(const char *filename);
(defcfun ("TCOD_sys_save_screenshot" %sys-save-screenshot) :void
  (filename :string))

(defun sys-save-screenshot (&optional (filename (null-pointer)))
  (%sys-save-screenshot filename))

;;TCODLIB_API void TCOD_sys_force_fullscreen_resolution(int width, int height);
;;TCODLIB_API void TCOD_sys_set_fps(int val);
(define-c-function ("TCOD_sys_set_fps" sys-set-fps) :void
    ((val :int)))

;;TCODLIB_API int TCOD_sys_get_fps();
(define-c-function ("TCOD_sys_get_fps" sys-get-fps) :int
    ())


;; Lisp wrapper needed because actual function returns nothing, whereas we
;; want to return resolution.
(defcfun ("TCOD_sys_get_current_resolution_x" sys-get-current-resolution-x) :int)
(defcfun ("TCOD_sys_get_current_resolution_y" sys-get-current-resolution-y) :int)

(defun sys-get-current-resolution ()
  (values (sys-get-current-resolution-x)
          (sys-get-current-resolution-y)))



;;;; <<Random>> ===============================================================


;;; mersenne.h

;;TCODLIB_API TCOD_random_t TCOD_random_get_instance();
;;TCODLIB_API TCOD_random_t TCOD_random_new();
(define-c-function ("TCOD_random_new" random-new) randomptr
  ((algorithm rng-algorithm)))
(define-c-function ("TCOD_random_new_from_seed" random-new-from-seed) randomptr
  ((algorithm rng-algorithm) (seed :uint32)))
(define-c-function ("TCOD_random_get_instance" random-get-instance) randomptr
    ())
(define-c-function ("TCOD_random_delete" random-delete) :void
  ((rng randomptr)))

;;TCODLIB_API TCOD_random_t TCOD_random_new_from_seed(uint32 seed);
;;TCODLIB_API int TCOD_random_get_int(TCOD_random_t mersenne, int min, int max);
(define-c-function ("TCOD_random_get_int" random-get-int) :int
  ((rng randomptr) (min :int) (max :int)))

;;TCODLIB_API float TCOD_random_get_float(TCOD_random_t mersenne, float min,
;;   float max);
(define-c-function ("TCOD_random_get_float" random-get-float) :float
  ((rng randomptr) (min :float) (max :float)))

;;TCODLIB_API int TCOD_random_get_int_from_byte_array(int min, int max,
;;   const char *data,int len);
;;TCODLIB_API void TCOD_random_delete(TCOD_random_t mersenne);


;;;; <<Mouse>> ================================================================


;; This may not work, because each time any one of these functions is called,
;; a mouse state is fetched. Events such as release of a mouse button might
;; only appear in a single mouse state.
(defcfun ("TCOD_mouse_get_x" mouse-get-x) :int)
(defcfun ("TCOD_mouse_get_y" mouse-get-y) :int)
(defcfun ("TCOD_mouse_get_cx" mouse-get-cx) :int)
(defcfun ("TCOD_mouse_get_cy" mouse-get-cy) :int)
(defcfun ("TCOD_mouse_get_dx" mouse-get-dx) :int)
(defcfun ("TCOD_mouse_get_dy" mouse-get-dy) :int)
(defcfun ("TCOD_mouse_get_dcx" mouse-get-dcx) :int)
(defcfun ("TCOD_mouse_get_dcy" mouse-get-dcy) :int)
(defcfun ("TCOD_mouse_get_lbutton" mouse-get-lbutton) :unsigned-int)
(defcfun ("TCOD_mouse_get_mbutton" mouse-get-mbutton) :unsigned-int)
(defcfun ("TCOD_mouse_get_rbutton" mouse-get-rbutton) :unsigned-int)
(defcfun ("TCOD_mouse_get_lbutton_pressed" mouse-get-lbutton-pressed)
    :unsigned-int)
(defcfun ("TCOD_mouse_get_mbutton_pressed" mouse-get-mbutton-pressed)
    :unsigned-int)
(defcfun ("TCOD_mouse_get_rbutton_pressed" mouse-get-rbutton-pressed)
    :unsigned-int)


#+nil
(defun* (mouse-state->mouse -> mouse) (ms)
  ;;(let ((flags (foreign-slot-value ms 'mouse-state 'flags)))
  (break)
  (make-mouse :x (foreign-slot-value ms 'mouse-state 'x)
              :y (foreign-slot-value ms 'mouse-state 'y)
              :dx (foreign-slot-value ms 'mouse-state 'dx)
              :dy (foreign-slot-value ms 'mouse-state 'dy)
              :cx (foreign-slot-value ms 'mouse-state 'cx)
              :cy (foreign-slot-value ms 'mouse-state 'cy)
              :dcx (foreign-slot-value ms 'mouse-state 'dcx)
              :dcy (foreign-slot-value ms 'mouse-state 'dcy)
              ;; :lbutton (get-bit flags 1)
              ;; :rbutton (get-bit flags 2)
              ;; :mbutton (get-bit flags 3)
              ;; :lbutton-pressed (get-bit flags 4)
              ;; :rbutton-pressed (get-bit flags 5)
              ;; :mbutton-pressed (get-bit flags 6))))
              :lbutton (foreign-slot-value ms 'mouse-state 'lbutton)
              :rbutton (foreign-slot-value ms 'mouse-state 'rbutton)
              :mbutton (foreign-slot-value ms 'mouse-state 'mbutton)
              :lbutton-pressed (foreign-slot-value ms 'mouse-state 'lbutton-pressed)
              :rbutton-pressed (foreign-slot-value ms 'mouse-state 'rbutton-pressed)
              :mbutton-pressed (foreign-slot-value ms 'mouse-state 'mbutton-pressed)))



;;TCODLIB_API TCOD_mouse_t TCOD_mouse_get_status();
(defcfun ("TCOD_mouse_get_status_wrapper" %mouse-get-status) :void
  (mouseptr :pointer))

;; Old version - creates a foreign struct.
#+nil
(defun* (mouse-get-status -> mouse) ()
  (with-foreign-object (rodent 'mouse-state)
    (%mouse-get-status rodent)
    (mouse-state->mouse rodent)))


;; New version - gets all data from foreign functions.
(defun mouse-get-status ()
  (%mouse-get-status (null-pointer))
  (make-mouse :x (mouse-get-x)
              :y (mouse-get-y)
              :dx (mouse-get-dx)
              :dy (mouse-get-dy)
              :cx (mouse-get-cx)
              :cy (mouse-get-cy)
              :dcx (mouse-get-dcx)
              :dcy (mouse-get-dcy)
              :lbutton (plusp (mouse-get-lbutton))
              :rbutton (plusp (mouse-get-rbutton))
              :mbutton (plusp (mouse-get-mbutton))
              :lbutton-pressed (plusp (mouse-get-lbutton-pressed))
              :rbutton-pressed (plusp (mouse-get-rbutton-pressed))
              :mbutton-pressed (plusp (mouse-get-mbutton-pressed))))



;;TCODLIB_API void TCOD_mouse_show_cursor(bool visible);
;;TCODLIB_API bool TCOD_mouse_is_cursor_visible();
;;TCODLIB_API void TCOD_mouse_move(int x, int y);
(define-c-function ("TCOD_mouse_move" mouse-move) :void
  ((pixel-x :int) (pixel-y :int)))



;;;; <<Image>> ================================================================


;;; image.h


;;TCODLIB_API TCOD_image_t TCOD_image_new(int width, int height);
(define-c-function ("TCOD_image_new" image-new) image
  ((width :int) (height :int))
  "Return a new image, filled with black.")

;;TCODLIB_API TCOD_image_t TCOD_image_from_console(TCOD_console_t console);
(define-c-function ("TCOD_image_from_console" image-from-console) image
  ((con console))
  "Return a new image whose contents are a 'screenshot' of the
console =CON=.")

;;TCODLIB_API TCOD_image_t TCOD_image_load(const char *filename);
(define-c-function ("TCOD_image_load" image-load) image
  ((filename :string))
  "Read an image from a file and return it.")


;;TCODLIB_API void TCOD_image_clear(TCOD_image_t image, TCOD_color_t color);
(define-c-function ("TCOD_image_clear_wrapper" image-clear) :void
  ((image image) (colour colournum))
  "Fill the image =IMAGE= with the colour =COLOUR=.")

;;TCODLIB_API void TCOD_image_save(TCOD_image_t image, const char *filename);
(define-c-function ("TCOD_image_save" image-save) :void
  ((image image) (filename :string))
  "Write the image =IMAGE= to a file. The filename must end in =.BMP=
or =.PNG=.")


;;TCODLIB_API void TCOD_image_get_size(TCOD_image_t image, int *w,int *h);
;;TCODLIB_API TCOD_color_t TCOD_image_get_pixel(TCOD_image_t image,int x, int y);
(define-c-function ("TCOD_image_get_pixel_wrapper" image-get-pixel) colournum
  ((image image) (pixel-x :int) (pixel-y :int))
  "Return the colour of the pixel at =(PIXEL-X, PIXEL-Y)= in =IMAGE=.")


;;TCODLIB_API TCOD_color_t TCOD_image_get_mipmap_pixel(TCOD_image_t image,
;; float x0,float y0, float x1, float y1);
(define-c-function ("TCOD_image_get_mipmap_pixel_wrapper"
	  image-get-mipmap-pixel) colournum
  ((image image) (x0 :float) (y0 :float) (x1 :float) (y1 :float))
  "Calculate the interpolated colour of the pixel at =(PIXEL-X, PIXEL-Y)=
in =IMAGE=.")


;;TCODLIB_API void TCOD_image_put_pixel(TCOD_image_t image,int x, int y,
;; TCOD_color_t col);
(define-c-function ("TCOD_image_put_pixel_wrapper" image-put-pixel) :void
  ((image image) (pixel-x :int) (pixel-y :int) (colour colournum))
  "Set the colour of the pixel at =(PIXEL-X, PIXEL-Y)= in =IMAGE= to =COLOUR=.")

;;TCODLIB_API void TCOD_image_blit(TCOD_image_t image, TCOD_console_t console,
;; float x, float y,
;;	TCOD_bkgnd_flag_t bkgnd_flag, float scalex, float scaley, float angle);
(define-c-function ("TCOD_image_blit" image-blit) :void
    ((image image) (con console) (x :int) (y :int) (flag background-flag)
     (scalex :float) (scaley :float) (angle :float))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_image_blit_rect(TCOD_image_t image,
;; TCOD_console_t console, int x, int y, int w, int h,
;;	TCOD_bkgnd_flag_t bkgnd_flag);
(define-c-function ("TCOD_image_blit_rect" image-blit-rect) :void
    ((image image) (con console) (x :int) (y :int) (width :int) (height :int)
     (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_image_delete(TCOD_image_t image);
;;TCODLIB_API void TCOD_image_set_key_color(TCOD_image_t image,
;; TCOD_color_t key_color);
(define-c-function ("TCOD_image_set_key_color" image-set-key-color) :void
  ((image image) (key-color colournum)))
(declaim (inline image-set-key-colour))
(defun image-set-key-colour (image key-colour)
  (image-set-key-color image key-colour))


;;TCODLIB_API bool TCOD_image_is_pixel_transparent(TCOD_image_t image, int x,
;;  int y);



;;;; <<Noise>> ================================================================


;; TCODLIB_API TCOD_noise_t TCOD_noise_new(int dimensions, float hurst,
;; float lacunarity, TCOD_random_t random);
;; For randomptr, use +NULL+ to use the default RNG
(defcfun ("TCOD_noise_new" %noise-new) noise
  (dimensions :int) (hurst :float) (lacunarity :float) (randomptr :pointer))

(defun* (noise-new -> noise) ((dimensions uint8)
                              &key ((hurst float) +NOISE-DEFAULT-HURST+)
                              ((lacunarity float) +NOISE-DEFAULT-LACUNARITY+)
                              ((rng randomptr) +NULL+))
  "Return a new noise object with the given characteristics."
  (%noise-new dimensions hurst lacunarity rng))


;; TCODLIB_API void TCOD_noise_delete(TCOD_noise_t noise);
(define-c-function ("TCOD_noise_delete" noise-delete) :void
  ((noise noise))
  "Destroy a noise object.")


(define-c-function ("TCOD_noise_set_type" noise-set-type) :void
  ((noise noise) (noise-type noise-type))
  "Set the type of noise produced by a noise object.")

;;; noise-get =================================================================

(defcfun ("TCOD_noise_get" %noise-get) :float
  (noise noise) (f :pointer))

(defun* (noise-get -> float) ((noise noise) &rest nums)
  "Returns the flat noise function at the given coordinates."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get noise f)))

(defcfun ("TCOD_noise_get_ex" %noise-get-ex) :float
  (noise noise) (f :pointer) (noise-type noise-type))

(defun* (noise-get-ex -> float) ((noise noise) (noise-type noise-type)
                                 &rest nums)
  "Returns the flat noise function at the given coordinates,
using noise type NOISE-TYPE."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get-ex noise f noise-type)))

;;; noise-get-FBM =============================================================

(defcfun ("TCOD_noise_get_fbm" %noise-get-fbm) :float
  (noise noise) (f :pointer) (octaves :float))

(defun* (noise-get-fbm -> float) ((noise noise) (octaves float) &rest nums)
  "Returns the fractional Brownian motion function at the given coordinates."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get-fbm noise f octaves)))

(defcfun ("TCOD_noise_get_fbm_ex" %noise-get-fbm-ex) :float
  (noise noise) (f :pointer) (octaves :float) (noise-type noise-type))

(defun* (noise-get-fbm-ex -> float) ((noise noise) (noise-type noise-type)
                                     (octaves float) &rest nums)
  "Returns the fractional Brownian motion function at the given coordinates,
using noise type NOISE-TYPE."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get-fbm-ex noise f octaves noise-type)))


;;; noise-get-turbulence ======================================================


(defcfun ("TCOD_noise_get_turbulence" %noise-get-turbulence) :float
  (noise noise) (f :pointer) (octaves :float))

(defun* (noise-get-turbulence -> float) ((noise noise) (octaves float)
                                         &rest nums)
  "Returns the turbulence function at the given coordinates."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get-turbulence noise f octaves)))

(defcfun ("TCOD_noise_get_turbulence_ex" %noise-get-turbulence-ex) :float
  (noise noise) (f :pointer) (octaves :float) (noise-type noise-type))

(defun* (noise-get-turbulence-ex -> float) ((noise noise)
                                            (noise-type noise-type)
                                            (octaves float) &rest nums)
  "Returns the turbulence function at the given coordinates,
using noise type NOISE-TYPE."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-get-turbulence-ex noise f octaves noise-type)))



;;;; <<Heightmap>> ============================================================



(define-c-function ("TCOD_heightmap_new" heightmap-new) heightmap
  ((width :int) (height :int))
  "Return a new heightmap with the given dimensions.")


(define-c-function ("TCOD_heightmap_get_value" heightmap-get-value) :float
  ;; 0 <= x < WIDTH
  ((heightmap heightmap) (x :int) (y :int))
  "Return the height at position =(X, Y)= in the heightmap.")


(define-c-function ("TCOD_heightmap_get_interpolated_value"
          heightmap-get-interpolated-value) :float
  ;; 0 <= x < WIDTH
  ((heightmap heightmap) (x :float) (y :float))
  "Calculate the height at position =(X, Y)= in the heightmap, where the
coordinates might not be integers.")


(define-c-function ("TCOD_heightmap_get_slope" heightmap-get-slope) :float
  ((heightmap heightmap) (x :int) (y :int))
  "Return the slope at position =(X, Y)= in the heightmap. The value returned
will be between 0 and pi/2.")


(define-c-function ("TCOD_heightmap_set_value" heightmap-set-value) :void
  ;; 0 <= x < WIDTH
  ((heightmap heightmap) (x :int) (y :int) (value :float))
  "Set the height at position =(X, Y)= in the heightmap to =VALUE=.")


(define-c-function ("TCOD_heightmap_add" heightmap-add) :void
  ((heightmap heightmap) (value :float))
  "Add =VALUE= to all heights in the heightmap.")


(define-c-function ("TCOD_heightmap_add_fbm" heightmap-add-fbm) :void
    ((heightmap heightmap) (noise noise) (mulx :float) (muly :float)
     (addx :float) (addy :float) (octaves :float) (delta :float) (scale :float))
  "Add values from the random noise object =NOISE= to all heights in
equivalent positions in =HEIGHTMAP=.")


(define-c-function ("TCOD_heightmap_scale" heightmap-scale) :void
  ((heightmap heightmap) (factor :float))
  "Multiply all the heights in the heightmap by =SCALE=.")


(define-c-function ("TCOD_heightmap_lerp_hm" heightmap-lerp) :void
  ((hm1 heightmap) (hm2 heightmap) (result heightmap) (coef :float))
  "Fill the heightmap =RESULT= with the results of a lerp operation between
the two heightmaps =HM1= and =HM2=.")


(define-c-function ("TCOD_heightmap_add_hm" heightmap-add-hm) :void
  ((hm1 heightmap) (hm2 heightmap) (result heightmap))
  "Add the heights in =HM1= to heights in equivalent positions in
=HM2=, and store the results in the heightmap =RESULT=.")


(define-c-function ("TCOD_heightmap_multiply_hm" heightmap-multiply-hm) :void
  ((hm1 heightmap) (hm2 heightmap) (result heightmap))
  "Multiply the heights in =HM1= by the heights in equivalent positions in
=HM2=, and store the results in the heightmap =RESULT=.")


(define-c-function ("TCOD_heightmap_clear" heightmap-clear) :void
  ((heightmap heightmap))
  "Set all the heights in the heightmap to zero.")


(define-c-function ("TCOD_heightmap_delete" heightmap-delete) :void
  ((heightmap heightmap))
  "Destroy the heightmap object =HEIGHTMAP=.")


(define-c-function ("TCOD_heightmap_clamp" heightmap-clamp) :void
  ((heightmap heightmap) (min :float) (max :float))
  "If any height in =HEIGHTMAP= is below =MIN= or above =MAX=, set it
equal to =MIN= or =MAX= respectively.")


(define-c-function ("TCOD_heightmap_count_cells" heightmap-count-cells) :int
  ((heightmap heightmap) (min :float) (max :float))
  "Return the number of cells in =HEIGHTMAP= which contain heights between
=MIN= and =MAX=.")


(define-c-function ("TCOD_heightmap_has_land_on_border"
                    heightmap-has-land-on-border?) :boolean
  ((heightmap heightmap) (waterlevel :float))
  "Return true if any of the border cells of =HEIGHTMAP= have heights greater
than =WATERLEVEL=.")


(defcfun ("TCOD_heightmap_get_minmax" %heightmap-get-minmax) :void
  (heightmap heightmap) (minfloat :pointer) (maxfloat :pointer))


(defun* (heightmap-get-min -> float) ((heightmap heightmap))
  "Return the lowest height in =HEIGHTMAP=."
  (with-foreign-object (minf :float)
    (with-foreign-object (maxf :float)
      (%heightmap-get-minmax heightmap minf maxf)
      (mem-aref minf :float))))


(defun* (heightmap-get-max -> float) ((heightmap heightmap))
  "Return the highest height in =HEIGHTMAP=."
  (with-foreign-object (minf :float)
    (with-foreign-object (maxf :float)
      (%heightmap-get-minmax heightmap minf maxf)
      (mem-aref maxf :float))))



(define-c-function ("TCOD_heightmap_normalize" heightmap-normalize) :void
  ((heightmap heightmap) (min :float) (max :float))
  "Scale all the heights in =HEIGHTMAP= so that the lowest is equal to
=MIN= and the highest is equal to =MAX=.")


(declaim (inline heightmap-normalise))
(defun heightmap-normalise (heightmap min max)
  (heightmap-normalize heightmap min max))


(define-c-function ("TCOD_heightmap_copy" heightmap-copy) :void
  ((source heightmap) (dest heightmap))
  "Copy the heightmap =SOURCE= into the heightmap object =DEST=.")


(defcfun ("TCOD_heightmap_rain_erosion" %heightmap-rain-erosion) :void
  (heightmap heightmap) (num-drops :int) (erosion-coef :float)
  (sediment-coef :float) (randomptr :pointer))


(defun* heightmap-rain-erosion ((heightmap heightmap) (num-drops fixnum)
                                (erosion-coef float)
                                (sedimentation-coef float)
                               &optional ((rng randomptr) +NULL+))
  "'Erode' the heightmap =HEIGHTMAP= by dropping =NUM-DROPS= 'raindrops' in
random locations."
  (%heightmap-rain-erosion heightmap num-drops erosion-coef
                           sedimentation-coef rng))


(defcfun ("TCOD_heightmap_dig_bezier" %heightmap-dig-bezier) :void
  (heightmap heightmap) (px :pointer) (py :pointer) (start-radius :float)
  (start-depth :float) (end-radius :float) (end-depth :float))


(defun* heightmap-dig-bezier ((heightmap heightmap) (coords list)
                              (start-radius float) (start-depth float)
                              (end-radius float) (end-depth float))
  "Carve a path through =HEIGHTMAP= using a cubic Bezier curve."
  (with-foreign-object (px :int 4)
    (with-foreign-object (py :int 4)
      (dotimes (i 4)
        (assert (realp (car (nth i coords))))
        (assert (realp (cdr (nth i coords))))
        (setf (mem-aref px :int i) (coerce (car (nth i coords)) 'integer))
        (setf (mem-aref py :int i) (coerce (cdr (nth i coords)) 'integer)))
      (%heightmap-dig-bezier heightmap px py ;(mem-ref px :int) (mem-ref py :int)
                             start-radius start-depth
                             end-radius end-depth))))


(defun* heightmap-dig-line ((heightmap heightmap)
                            (x1 fixnum) (y1 fixnum)
                            (x2 fixnum) (y2 fixnum) (radius float)
                            (depth float))
  (heightmap-dig-bezier heightmap `((,x1 . ,y1) (,x1 . ,y1) (,x2 . ,y2) (,x2 . ,y2))
                        radius depth radius depth))


;;;; <<Field of view>> =========================================================


;; Create a map
(define-c-function ("TCOD_map_new" map-new) mapptr
    ((width :int) (height :int))
  "Return a new map object of the given dimensions.")

(define-c-function ("TCOD_map_set_properties" map-set-properties) :void
    ((map mapptr) (x :int) (y :int) (transparent? :boolean) (walkable? :boolean))
  "Set the properties of the map cell at =(X, Y)=. It is walkable if
=walkable?= is true, and transparent if =transparent?= is true.")

(define-c-function ("TCOD_map_compute_fov" map-compute-fov) :void
    ((map mapptr) (player-x :int) (player-y :int) (max-radius :int)
     (light-walls? :boolean) (algorithm fov-algorithm))
  "Compute field of view information for =MAP=, assuming the player is at
=(PLAYER-X, PLAYER-Y)=, and using the field of view algorithm =ALGORITHM=.")

(define-c-function ("TCOD_map_is_in_fov" map-is-in-fov?) :boolean
    ((map mapptr) (x :int) (y :int))
  "Return true if position =(X, Y)= on the map is visible.")

(define-c-function ("TCOD_map_is_transparent" map-is-transparent?) :boolean
    ((map mapptr) (x :int) (y :int))
  "Return true if position =(X, Y)= on the map is set to be transparent.")

(define-c-function ("TCOD_map_is_walkable" map-is-walkable?) :boolean
    ((map mapptr) (x :int) (y :int))
  "Return true if position =(X, Y)= on the map is set to be walkable.")

(define-c-function ("TCOD_map_clear" map-clear) :void
    ((map mapptr))
  "Set all cells in =MAP= to be neither walkable nor transparent.")

(define-c-function ("TCOD_map_delete" map-delete) :void
    ((map mapptr))
  "Destroy the map object =MAP=.")

(define-c-function ("TCOD_map_copy" map-copy) :void
    ((map-src mapptr) (map-dest mapptr))
  "Copy the map object =SRC= into the new map object =DEST=.")



;;;; <<A* pathfinding>> =======================================================


;; Example of how to define a lisp function which can be called from C,
;; for use with PATH-NEW-USING-FUNCTION.
(defcallback my-a*-callback :float
    ((from-x :int) (from-y :int) (to-x :int) (to-y :int) (userdata :pointer))
  ;; In the function body: all args will have been converted to lisp.
  ;; The function's return value will be converted to C.
  (declare (ignore from-x from-y to-x to-y userdata))
  )


(define-c-function ("TCOD_path_new_using_map" path-new-using-map) a*-path
    ((map mapptr) (diagonal-cost :float))
  "Return a new A* path object, using the map =MAP=.")

;; Call like this:
;;   (tcod:path-new-using-function x y (callback my-a*-callback) ptr)
;; Where 'my-a*-callback' is a lisp function defined using defcallback
;; (see above).
(define-c-function ("TCOD_path_new_using_function" path-new-using-function) a*-path
    ((xdim :int) (ydim :int) (callback :pointer) (user-data :pointer)
     (diagonal-cost :float))
  "Return a new A* path object, which will call the function =CALLBACK= to
calculate movement costs.")

(define-c-function ("TCOD_path_delete" path-delete) :void
    ((a*-path a*-path))
  "Delete an A* path object.")

(define-c-function ("TCOD_path_compute" path-compute) :boolean
    ((a*-path a*-path) (ox :int) (oy :int) (dx :int) (dy :int))
  "Compute the path between the two points =(OX,OY)= and =(DX,DY)=, using the
A* algorithm.")

(defcfun ("TCOD_path_get_origin" %path-get-origin) :void
  (a*-path a*-path) (xptr :pointer) (yptr :pointer))

(defun* (path-get-origin -> (cons fixnum fixnum)) ((a*-path a*-path))
  "Return the coordinates of the current origin of the A* path =PATH=."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (%path-get-origin a*-path x y)
      (cons (mem-aref x :int) (mem-aref y :int)))))

(defcfun ("TCOD_path_get_destination" %path-get-destination) :void
  (a*-path a*-path) (xptr :pointer) (yptr :pointer))

(defun* (path-get-destination -> (cons fixnum fixnum)) ((a*-path a*-path))
  "Return the coordinates of the current destination of the A* path =PATH=."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (%path-get-destination a*-path x y)
      (cons (mem-aref x :int) (mem-aref y :int)))))

(define-c-function ("TCOD_path_size" path-size) :int
    ((a*-path a*-path))
  "Return the number of steps in the path.")

(defcfun ("TCOD_path_get" %path-get) :void
  (a*-path a*-path) (index :int) (xptr :pointer) (yptr :pointer))

(defun* (path-get -> (cons fixnum fixnum)) ((a*-path a*-path) (index fixnum))
  "Return the INDEXth step in the path from its current origin to its current
destination."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (%path-get a*-path index x y)
      (cons (mem-aref x :int) (mem-aref y :int)))))

(defcfun ("TCOD_path_walk" %path-walk) :boolean
  (a*-path a*-path) (xptr :pointer) (yptr :pointer) (recalc-when-needed? :boolean))

(defun* (path-walk -> (or null (cons fixnum fixnum))) ((a*-path a*-path)
                                                       (recalc-when-needed? boolean))
  "Move one step along =PATH=. The path becomes one step shorter. Returns
the coordinates of the new location."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (if (%path-walk a*-path x y recalc-when-needed?)
          (cons (mem-aref x :int) (mem-aref y :int))
          nil))))

(define-c-function ("TCOD_path_is_empty" path-is-empty?) :boolean
  ((a*-path a*-path))
  "Return true if the path object is empty (has zero steps).")


;; <<Dijkstra pathfinding>> ===================================================


(define-c-function ("TCOD_dijkstra_new" dijkstra-new) dijkstra-path
    ((map mapptr) (diagonal-cost :float))
  "Return a new Dijkstra path object which uses =MAP=.")

(define-c-function ("TCOD_dijkstra_new_using_function" dijkstra-new-using-function)
    dijkstra-path
    ((xdim :int) (ydim :int) (callback :pointer) (user-data :pointer)
     (diagonal-cost :float))
  "Return a new Dijkstra path object which calls the function =CALLBACK= to
calculate movement costs.")

(define-c-function ("TCOD_dijkstra_delete" dijkstra-delete) :void
    ((dijkstra-path dijkstra-path))
  "Delete a Dijkstra path object.")

(define-c-function ("TCOD_dijkstra_compute" dijkstra-compute) :void
    ((dijkstra-path dijkstra-path) (rootx :int) (rooty :int))
  "Compute paths leading to the point at =(ROOTX, ROOTY)=, using the
Dijkstra algorithm.")

(define-c-function ("TCOD_dijkstra_path_set" dijkstra-path-set) :boolean
    ((dijkstra-path dijkstra-path) (to-x :int) (to-y :int))
  "Return true if a path can be found leading from the root node to the
point at =(TO-X, TO-Y)=.")

(define-c-function ("TCOD_dijkstra_size" dijkstra-size) :int
    ((dijkstra-path dijkstra-path))
  "Return the number of steps in the path.")

(define-c-function ("TCOD_dijkstra_get_distance" dijkstra-get-distance) :float
    ((dijkstra-path dijkstra-path) (to-x :int) (to-y :int))
  "Return the number of steps on the path leading from the root node to
the point at =(TO-X, TO-Y)=.")

(defcfun ("TCOD_dijkstra_get" %dijkstra-get) :void
  (dijkstra-path dijkstra-path) (index :int) (xptr :pointer) (yptr :pointer))

(defun* (dijkstra-get -> (cons fixnum fixnum)) ((dijkstra-path dijkstra-path)
                                                  (index fixnum))
  "Return the INDEXth step in the path from its current origin to its current
destination."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (%dijkstra-get dijkstra-path index x y)
      (cons (mem-aref x :int) (mem-aref y :int)))))

(define-c-function ("TCOD_dijkstra_is_empty" dijkstra-is-empty?) :boolean
    ((dijkstra-path dijkstra-path))
  "Return true if the path object is empty (has zero steps).")

(defcfun ("TCOD_dijkstra_path_walk" %dijkstra-path-walk) :boolean
  (dijkstra-path dijkstra-path) (xptr :pointer) (yptr :pointer))

(defun* (dijkstra-path-walk -> (or null (cons fixnum fixnum)))
    ((dijkstra-path dijkstra-path))
  "Move one step along =PATH=. The path becomes one step shorter. Returns
the coordinates of the new location."
  (with-foreign-object (x :int)
    (with-foreign-object (y :int)
      (if (%dijkstra-path-walk dijkstra-path x y)
          (cons (mem-aref x :int) (mem-aref y :int))
          nil))))



;;;; <<Testing>> ==============================================================


(defun hello-world ()
  (tcod:console-init-root 80 50 "Libtcod Hello World" nil :renderer-sdl)
  (tcod:console-set-alignment *root* :center)
  (tcod:console-print tcod:*root* 40 25 "Hello World!")
  (tcod:console-flush)
  (tcod:console-wait-for-keypress t))



;;;; tcod.lisp ends here ======================================================
