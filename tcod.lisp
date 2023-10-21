;;;; -*- Mode: lisp; coding: utf-8-unix -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80

(in-package :cl-user)

;;;; The documentation for this package is generated using the CLOD library.
;;;;
;;;; The following command is used: (tcod and clod systems must be
;;;; loaded into the running lisp image first)
;;;;

#+nil
(clod:document-package :tcod "tcod.org"
                       :title "CL-TCOD"
                       :internal-symbols? nil
                       :brief-methods t
                       :author "Paul Sexton"
                       :email "eeeickythump@gmail.com")


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
  (:nicknames :cl-tcod)
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
   #:colour->keyword
   #:color->keyword
   #:colour-rgb
   #:color-rgb
   #:colour-hsv
   #:color-hsv
   #:invert-colour
   #:invert-color
   #:colour->grayscale
   #:color->grayscale
   #:colour-set-hsv
   #:colour-get-hsv
   #:colour-get-hue
   #:colour-get-saturation
   #:colour-get-value
   #:colour-set-hue
   #:colour-set-saturation
   #:colour-set-value
   #:colour-shift-hue
   #:colour-equals?
   #:colour-add
   #:colour-subtract
   #:colour-multiply
   #:colour-multiply-scalar
   #:colour-lerp
   #:make-colour
   #:color-set-hsv
   #:color-get-hsv
   #:color-get-hue
   #:color-get-saturation
   #:color-get-value
   #:color-set-hue
   #:color-set-saturation
   #:color-set-value
   #:color-shift-hue
   #:color-equals?
   #:color-add
   #:color-subtract
   #:color-multiply
   #:color-multiply-scalar
   #:color-lerp
   #:make-color
   #:background-alpha
   #:background-add-alpha
   ;; [[Console]] ==========================================================
   #:console-init-root
   #:console-initialised?
   #:console-set-window-title
   #:console-is-fullscreen?
   #:console-set-fullscreen
   #:console-is-window-closed?
   #:console-set-custom-font
   #:console-map-ascii-code-to-font
   #:console-map-ascii-codes-to-font
   #:console-map-string-to-font
   #:console-set-dirty
   #:console-set-default-foreground
   #:console-set-default-background
   #:console-set-char-foreground
   #:console-set-char-background
   #:console-set-char
   #:console-put-char
   #:console-put-char-ex
   #:console-set-background-flag
   #:console-get-background-flag
   #:console-set-alignment
   #:console-get-alignment
   #:console-print
   #:console-print-ex
   #:console-print-rect
   #:console-print-rect-ex
   #:console-get-height-rect
   #:console-rect
   #:console-hline
   #:console-vline
   #:console-print-frame
   #:console-print-double-frame
   #:console-get-default-foreground
   #:console-get-default-background
   #:console-get-char-foreground
   #:console-get-char-background
   #:console-get-char
   #:console-set-fade
   #:console-get-fade
   #:console-get-fading-colour
   #:console-flush
   #:console-set-colour-control
   #:console-new
   #:console-get-height
   #:console-get-width
   #:console-set-color-control
   #:console-get-fading-color
   #:console-clear
   #:console-blit
   #:console-delete
   #:console-credits
   #:console-credits-reset
   #:console-credits-renderer
   #:legal-console-coordinates?
   #:console-fill-char
   #:console-set-key-colour
   #:console-set-key-color
   #:drawing-character
   #:colctrl
   #:colctrl->char
   #:background-flag
   #:console
   ;; [[Unicode]] =============================================================
   #:console-map-string-to-font-utf
   #:console-print-utf
   #:console-print-ex-utf
   #:console-print-rect-utf
   #:console-print-rect-ex-utf
   #:console-get-rect-height-utf
   ;; [[Keyboard input]] ======================================================
   #:key
   #:console-check-for-keypress
   #:console-wait-for-keypress
   #:keycode
   #:key-p
   #:key-c
   #:key-vk
   #:key-lalt
   #:key-ralt
   #:key-lctrl
   #:key-rctrl
   #:key-lmeta
   #:key-rmeta
   #:key-shift
   #:make-key
   #:make-simple-key
   #:same-keys?
   #:key-state
   #:key-press
   #:key-pressed
   #:is-key-pressed?
   #:console-set-keyboard-repeat
   #:console-disable-keyboard-repeat
   ;; [[Mouse]] ===============================================================
   #:mouse
   #:mouse-state
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
   #:mouse-wheel-up
   #:mouse-wheel-down
   #:mouse-move
   #:mouse-get-status
   #:mouse-is-cursor-visible?
   #:mouse-show-cursor
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
   #:image-from-console
   #:image-refresh-console
   #:image-load
   #:image-clear
   #:image-invert
   #:image-hflip
   #:image-vflip
   #:image-rotate90
   #:image-scale
   #:image-save
   #:image-get-width                    ; these replace image-get-size
   #:image-get-height
   #:image-get-pixel
   #:image-get-alpha
   #:image-get-mipmap-pixel
   #:image-put-pixel
   #:image-blit
   #:image-blit-rect
   #:image-blit-2x
   #:image-delete
   #:image-set-key-colour
   #:image-set-key-color
   #:image-is-pixel-transparent?
   ;; [[Random]] ==============================================================
   #:random-new
   #:random-get-instance
   #:random-save
   #:random-restore
   #:random-new-from-seed
   #:random-set-distribution
   #:random-delete
   #:random-get-int
   #:random-get-float
   #:random-get-double
   #:random-get-int-mean
   #:random-get-float-mean
   #:random-get-double-mean
   #:random-dice-new ;; not yet mentioned in libtcod docs
   #:random-dice-roll ;;
   #:random-dice-roll-s ;;
   ;; [[Noise]] ===============================================================
   #:noise-new
   #:noise-delete
   #:noise-set-type
   #:noise-get-ex
   #:noise-get-fbm-ex
   #:noise-get-turbulence-ex
   #:noise-get
   #:noise-get-fbm
   #:noise-get-turbulence
   ;; [[Heightmap]] ===========================================================
   #:heightmap
   #:heightmap-new
   #:heightmap-delete
   #:heightmap-set-value
   #:heightmap-add
   #:heightmap-scale
   #:heightmap-clear
   #:heightmap-clamp
   #:heightmap-copy
   #:heightmap-normalize
   #:heightmap-normalise
   #:heightmap-lerp-hm
   #:heightmap-add-hm
   #:heightmap-multiply-hm
   #:heightmap-add-hill
   #:heightmap-dig-hill
   #:heightmap-rain-erosion
   #:heightmap-kernel-transform
   #:heightmap-add-voronoi
   #:heightmap-add-fbm
   #:heightmap-scale-fbm
   #:heightmap-dig-bezier
   #:heightmap-get-value
   #:heightmap-get-interpolated-value
   #:heightmap-get-slope
   #:heightmap-get-normal
   #:heightmap-count-cells
   #:heightmap-has-land-on-border?
   #:heightmap-get-min
   #:heightmap-get-max
   #:heightmap-islandify
   #:heightmap-dig-line    ; defined in cl-tcod
   ;; [[Field of view]] =======================================================
   #:fov-algorithm
   #:mapptr
   #:map-new
   #:map-set-properties
   #:map-clear
   #:map-copy
   #:map-delete
   #:map-compute-fov
   #:map-is-in-fov?
   #:map-set-in-fov
   #:map-is-transparent?
   #:map-is-walkable?
   #:map-get-width
   #:map-get-height
   #:map-get-nb-cells
   ;; [[A* pathfinding]] ======================================================
   #:a*-path
   #:path-new-using-map
   #:path-new-using-function
   #:path-delete
   #:path-compute
   #:path-reverse
   #:path-get
   #:path-get-origin
   #:path-get-destination
   #:path-size
   #:path-walk
   #:path-is-empty?
   ;; [[Dijkstra pathfinding]] ================================================
   #:dijkstra-path
   #:dijkstra-new
   #:dijkstra-new-using-function
   #:dijkstra-delete
   #:dijkstra-compute
   #:dijkstra-reverse
   #:dijkstra-path-set
   #:dijkstra-size
   #:dijkstra-get
   #:dijkstra-is-empty?
   #:dijkstra-path-walk
   #:dijkstra-get-distance
   ;; [[Bresenham line drawing]] ==============================================
   #:line-init
   #:line-step
   #:line-line
   ;; [[BSP trees]] ===========================================================
   #:bsp-new-with-size
   #:bsp-remove-sons
   #:bsp-split-once
   #:bsp-split-recursive
   #:bsp-delete
   #:bsp-resize
   #:bsp-left
   #:bsp-right
   #:bsp-father
   #:bsp-is-leaf?
   #:bsp-contains?
   #:bsp-find-node
   #:bsp-traverse-pre-order
   #:bsp-traverse-in-order
   #:bsp-traverse-post-order
   #:bsp-traverse-level-order
   #:bsp-traverse-inverted-level-order
   ;; [[Name generation]] =====================================================
   #:namegen-parse
   #:namegen-destroy
   #:namegen-generate
   #:namegen-generate-custom
   ;;#:namegen-get-sets -- not yet implemented as returns a TCOD_list_t type
   ;; [[Compression toolkit]] =================================================
   #:zipptr
   #:zip-new
   #:zip-delete
   #:zip-put
   #:zip-put-char
   #:zip-put-int
   #:zip-put-float
   #:zip-put-string
   #:zip-put-colour
   #:zip-put-color
   #:zip-put-image
   #:zip-put-console
   #:zip-put-data
   #:zip-get-char
   #:zip-get-int
   #:zip-get-float
   #:zip-get-string
   #:zip-get-image
   #:zip-get-colour
   #:zip-get-color
   #:zip-get-console
   #:zip-get-data
   #:zip-get-current-bytes
   #:zip-get-remaining-bytes
   #:zip-skip-bytes
   #:zip-save-to-file
   #:zip-load-from-file
   ;; [[System]] ==============================================================
   #:sys-set-fps
   #:sys-get-fps
   #:sys-get-last-frame-length
   #:sys-sleep-milli
   #:sys-elapsed-milli
   #:sys-elapsed-seconds
   #:sys-save-screenshot
   #:sys-create-directory ;;
   #:sys-delete-directory ;;
   #:sys-get-current-resolution
   #:sys-force-fullscreen-resolution ;;
   #:sys-get-fullscreen-offsets ;;
   #:sys-get-renderer
   #:sys-set-renderer
   #:sys-register-sdl-renderer ;;
   #:sys-get-char-size
   #:sys-update-char ;;
   #:sys-check-for-event
   #:sys-wait-for-event
   #:sys-get-events
   #:sys-wait-events
   #:sys-clipboard-set ;;
   #:sys-clipboard-get ;;
   #:sys-flush
   ;; [[SDL]] =================================================================
   #:sdl-get-mouse-state 
   ;; [[Testing]] =============================================================
   #:hello-world
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
2. Ensure either [[http://www.quicklisp.org/][Quicklisp]] (recommended) or the
   ASDF lisp library is installed.
3. If CFFI or DEFSTAR are not installed, download and install them somewhere
   ASDF can find them. CFFI requires several third-party lisp libraries -- see
   the CFFI documentation for more details.  Note that if you have
   Quicklisp installed, you can install CFFI and its dependencies
   easily using the command =(ql:quickload \"cffi\")= at the Lisp prompt.
4. Put the CL-TCOD files in a directory where ASDF can find them.
5. Make sure libtcod is installed and compiled. Make sure the libtcod and libSDL
   dynamically linked libraries (=.DLL= or =.SO= files) are somewhere your lisp
   system can find them. They probably are, but if CFFI complains about being unable
   to find the libraries, you can either copy them to an appropriate directory or
   add their directory to the list variable =cffi:*foreign-library-directories*=
   e.g. by typing the following in the lisp interpreter:

;;;   (push #P\"/my/libtcod/directory/\" cffi:*foreign-library-directories*)

   *On windows*, DLL files should be put in one of the directories listed in the
   =PATH= environment variable. You will need to put =SDL.dll= in the same place
   if you don't already have SDL installed.

   *On Linux*, you can usually put .SO files in =/usr/local/lib/=.
   Use your package installer to install =libSDL=.
   Try running the libtcod demo programs to check everything works.

6. Start lisp, then load CL-TCOD. Using Quicklisp (recommended):

;;;   (ql:quickload :tcod)

   Using ASDF:

;;;   (load \"/path/to/asdf/asdf.lisp\")
;;;   (asdf:oos 'asdf:load-op :tcod)

7. Type something like the following commands at the lisp prompt to start using
   TCOD from within Lisp. Alternatively you can type =(tcod:hello-world)=, which
   is a function containing the code below.

;;;   (tcod:console-set-custom-font \"terminal.png\" '(:font-layout-ascii-in-row) 16 16)
;;;   (tcod:console-init-root 80 25 :title \"Test\")
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

** Coverage

Does not provide wrappers for:
- File parser. Using this from lisp would be a very cumbersome way to read
  values from a file, as the resulting values are not lisp objects. You would
  be better to either consider using the lisp
  `read' function, or looking into lisp libraries for parser generation.
- =namegen-get-sets= -- I haven't yet implemented this as it will have to
  involve converting from libtcod's bespoke 'linked list' to a lisp list.
  You may be better to write your random name generator in lisp (fairly trivial).
- =sys-get-directory-content=, =sys-file-exists=, =sys-is-directory=,
  =sys-delete-file=: Common Lisp already has functions that do the same thing.

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

[[http://www.quicklisp.org/][Quicklisp]] allows you to very easily install
libraries -- it automatically downloads and installs a library and its
dependencies, from within Lisp.  If you don't decide to go with Lisp in a
Box (below), then Quicklisp should be the first thing you install once you have
your lisp running.

*\"Lisp in a Box\"* -- aims to make it easy to start using Common Lisp by
providing a single download with everything set up in advance (Lisp, Emacs,
SLIME, and Quicklisp).

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
  has a builtin graphical IDE.
- Some editors with good lisp syntax highlighting include jEdit and Notepad++.

** A note on editors and IDEs

Emacs is a very powerful program. It is mainly used as a programmers' text and
source code editor, but it can do -- and plugins exist to make it do -- just
about anything you can imagine. It is mostly written in a dialect of lisp, and
this is also its extension language. When combined with SLIME, a plugin
that allows it to communicate directly with a running common lisp
compiler/interpreter, Emacs is not only the best IDE for common lisp, but
one of the best and most advanced IDEs available for any programming language.

The downside: because Emacs + SLIME is so good, common lisp programmers have
put very little effort into getting other popular programming editors/IDEs to
support common lisp, at least beyond simple syntax highlighting. Emacs is an
idiosyncratic program (though development is active, it is about 34 years old)
and despite good efforts to modernise/regularise its interface it still has a
steeper learning curve than many other IDEs, especially when you are also
struggling to set up SLIME and get it to communicate through a socket with
your lisp process...

My advice is that while all roads lead to Emacs, you don't have to hurry to get
there. Initially you should concentrate on getting common lisp set up and
starting to learn the language. Think about using the trial version of one of
the big commercial implementations (Allegro or LispWorks), as they have
built-in IDEs. Once you are ready to move on from them, install Emacs and
SLIME.

** Commercial Common Lisp implementations

These are both high quality, but painfully expensive. Luckily they have
'trial' versions that can be downloaded for free, and which I recommend you
use when beginning to learn Common Lisp as they come with integrated
graphical editors/development environments (although if you have a Mac
you may wish to investigate Clozure CL's IDE -- see below).

- [[http://www.franz.com/products/allegrocl/][Allegro]] -- starts at $599 USD
- [[http://www.lispworks.com/][LispWorks]] -- starts at $900 USD for a
  noncommercial license. The trial version quits automatically after 5 hours.

** Full-featured, free Common Lisp implementations

Move on to one of these if and when you outgrow Allegro or LispWorks.

For the title of the best, most robust free multiplatform Common Lisp compiler,
it is currently a very close call between these two:
- [[http://www.sbcl.org][Steel Bank Common Lisp (SBCL)]] Compiles to
  machine code, great on Linux/Mac,
  still nominally 'experimental' on Windows but actually seems very stable
  on that platform.
- [[http://ccl.clozure.com][Clozure CL]] Compiles to machine code; native to
  Mac but recently ported to Linux and Windows. Formerly known as OpenMCL.
  The Mac version has a graphical IDE.
  Not to be confused with [[http://clojure.org][Clojure]], which is a different
  dialect of lisp from Common Lisp.

Other worthwhile free implementations:
- [[http://clisp.cons.org][GNU CLISP]] Bytecode compiler, so programs won't run
  as fast as in the compiled lisps discussed above. However it runs pretty much
  everywhere, and is easy to install on Windows.
- [[http://ecls.sourceforge.net/][Embeddable Common Lisp]] Promising, compiles
  to C and then passes code to your C compiler. Does this 'on the fly' when
  running as an interpreter. Also designed to be easily embeddable in non-Lisp
  applications as a scripting language.
- [[http://common-lisp.net/project/armedbear/][Armed Bear Common Lisp]]
  Common Lisp compiler running inside the Java virtual machine, so your
  code will run on any platform and can use all the Java libraries. I doubt
  you'll be able to use libtcod with this though.

Help & advice with lisp:

    [[http://www.lispforum.com]]
"))

(in-package :tcod)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (pushnew :tcod *features*))

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


;;;; <<Libraries>> ============================================================


#+darwin
(dolist (path '(#p"/opt/local/lib/" #p"/usr/local/lib/"))
  (pushnew path cffi:*foreign-library-directories*))

(define-foreign-library libtcod
    (:darwin "libtcod.dylib")
  (:windows #-libtcod-debug "libtcod-mingw.dll"
            #+libtcod-debug "libtcod-mingw-debug.dll")
  (:unix #+libtcod-debug "libtcod-debug.so"
         #-libtcod-debug "libtcod.so")
  (t (:default "libtcod")))

(defvar *libtcod-loaded* nil
  "Global variable, set to non-nil once libtcod is loaded. This is to
avoid crashes which occur in some CL implementations when you load
an already-loaded foreign library.")

(defvar *root-console-initialised?* nil
  "Set to T once `console-init-root' has been called.")


(eval-when (:load-toplevel :execute)
        (unless *libtcod-loaded*
                (use-foreign-library libtcod)
                (setf *libtcod-loaded* t)))

;;; We need direct access to SDL because libtcod 1.5.1rc1 does not report
;;; mouse buttons correctly (or at least, reading them via CFFI gives
;;; strange, random results.)

(define-foreign-library libsdl2
  (:darwin "libSDL2.dylib")
  (:unix "libSDL2.so")
  (:windows "SDL2.dll")
  (t (:default "libsdl2")))

(defvar *libsdl2-loaded* nil)

(eval-when (:load-toplevel :execute)
  (unless *libsdl2-loaded*
    (use-foreign-library libsdl2)
    (setf *libsdl2-loaded* t)))

;; Returns an 8-bit integer.
;; bit 1: lbutton
;; bit 2: mbutton
;; bit 3: rbutton
;; The arguments xptr and yptr can be null pointers.
(defcfun ("SDL_GetMouseState" sdl-get-mouse-state) :int
  (xptr :pointer) (yptr :pointer))


;;;; <<Macros>> ===============================================================


;;; The following are some wrapper macros to ease the creation
;;; of `type-safe' CFFI wrappers to C functions.


(defmacro define-c-enum (name &rest vals)
  "Defines both the CFFI =enum= type, and a lisp type of the same
name which is satisified by any of the values allowed by the enum type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (defcenum ,name ,@vals)
       (deftype ,(if (listp name) (car name) name) ()
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
      (:int 'int)
      (:unsigned-int 'uint)
      (:char 'signed-char)
      (:unsigned-char 'uchar)
      (:uint8 'uint8)
      (:uint32 'uint32)
      (:float 'single-float)
      (:double 'double-float)
      (:pointer (type-of (null-pointer)))
      (:string 'string)
      (:void t)
      (otherwise
       (if (simple-type? c-type)
           c-type
           (error "In C-TYPE->LISP-TYPE: unrecognised C type `~S'." c-type))))))


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
       (cond
         ((null (cffi:foreign-symbol-pointer ,foreign-fn-name))
          (warn "Foreign function not found: ~S" ,foreign-fn-name))
         (t
          (defcfun (,foreign-fn-name ,(prepend-percent fn-name)) ,return-type
            ,@args)
          (defun* (,fn-name -> ,(c-type->lisp-type return-type))
              ,(mapcar #'(lambda (clause)
                           `(,(first clause) ,(c-type->lisp-type (second clause))
                             ,@(cddr clause)))
                args-no-rest)
            ,@(if (stringp (car body)) (list (pop body)) nil)
            ,(if body
                 `(macrolet ((call-it (&rest callargs)
                               (cons ',(prepend-percent fn-name)
                                     (or callargs '(,@(mapcar #'car
                                                       args-no-rest))))))
                    ,@body)
                 `(,(prepend-percent fn-name) ,@(mapcar #'car
                                                        args-no-rest)))))))))



(defmacro define-c-type (name foreign-type)
  "Define both a CFFI foreign type, and a corresponding lisp type of the same
name."
  `(progn
     (defctype ,name ,foreign-type)
     (deftype ,name () ',(c-type->lisp-type foreign-type))))


(defmacro clamp (low hi expr)
  "Return the numeric value of EXPR, constrained to the range [LOW ... HI]."
  `(min ,hi (max ,low ,expr)))


;;;; <<Types>> ================================================================


(deftype uint32 () `(unsigned-byte 32))
(deftype uint24 () `(unsigned-byte 24))
(deftype uint16 () `(unsigned-byte 16))
(deftype uint8 () `(unsigned-byte 8))
(deftype uint () `(unsigned-byte ,(* 8 (foreign-type-size :int))))
(deftype int () `(signed-byte ,(* 8 (foreign-type-size :int))))
(deftype uchar () `(unsigned-byte ,(* 8 (foreign-type-size :unsigned-char))))
(deftype signed-char () `(signed-byte ,(* 8 (foreign-type-size :char))))

(deftype sint16 () `(signed-byte 16))

(deftype ucoord () `(integer 0 1000))


(define-c-type colournum :unsigned-int)


;; TCOD_color_t
;; This is seldom used -- colournums are used instead (see above).
(defcstruct colour-struct
  (r :uint8)
  (g :uint8)
  (b :uint8))


;; TCOD_renderer_t (enum)
(define-c-enum renderer
  :RENDERER-GLSL
  :RENDERER-OPENGL
  :RENDERER-SDL)


;; TCOD_keycode_t (enum)
(define-c-enum
    #-darwin keycode
    #+darwin (keycode :uint32)
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
  :CHAR
  :TEXT)


;;; TCOD_key_t
#-darwin
(defcstruct key-press
  (vk keycode)
  (c :char)
  (text :char :count 32)
  (flag-pressed :bool)
  (flag-lalt :bool)
  (flag-lctrl :bool)
  (flag-lmeta :bool)
  (flag-ralt :bool)
  (flag-rctrl :bool)
  (flag-rmeta :bool)
  (flag-shift :bool))

;;; Horrendous problems getting this to work on OSX. The `keycode' enum type
;;; must be a 4-byte int (forced in its definition above).
#+darwin
(defcstruct key-press
  (vk keycode)
  (c :uint8)
  (text :uint8 :count 32)
  (flag-pressed :uint8)
  (flag-lalt :uint8)
  (flag-lctrl :uint8)
  (flag-lmeta :uint8)
  (flag-ralt :uint8)
  (flag-rctrl :uint8)
  (flag-rmeta :uint8)
  (flag-shift :uint8))


(defstruct key
  "The structure used by CL-TCOD to represent key-press events. Corresponds
to the structure used by libtcod."
  (vk :none :type keyword)
  (c #\null :type character)
  (pressed nil :type boolean)
  (lalt nil :type boolean)
  (lctrl nil :type boolean)
  (lmeta nil :type boolean)
  (ralt nil :type boolean)
  (rctrl nil :type boolean)
  (rmeta nil :type boolean)
  (shift nil :type boolean))


(define-c-enum event
  (:EVENT-NONE 0)
  (:EVENT-KEY-PRESS 1)
  (:EVENT-KEY-RELEASE 2)
  (:EVENT-KEY 3)                        ; PRESS | RELEASE
  (:EVENT-MOUSE-MOVE 4)
  (:EVENT-MOUSE-PRESS 8)
  (:EVENT-MOUSE-RELEASE 16)
  (:EVENT-MOUSE 28)                     ; MOVE | PRESS | RELEASE
  (:EVENT-ANY 31))


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
  (:CHAR-SUBP-SW 232)
  ;; Miscellaneous     
  (:CHAR-SMILIE 1)
  (:CHAR-SMILIE-INV 2)
  (:CHAR-HEART 3)
  (:CHAR-DIAMOND 4)
  (:CHAR-CLUB 5)
  (:CHAR-SPADE 6)
  (:CHAR-BULLET 7)
  (:CHAR-BULLET-INV 8)
  (:CHAR-MALE 11)
  (:CHAR-FEMALE 12)
  (:CHAR-NOTE 13)
  (:CHAR-NOTE-DOUBLE 14)
  (:CHAR-LIGHT 15)
  (:CHAR-EXCLAM-DOUBLE 19)
  (:CHAR-PILCROW 20)
  (:CHAR-SECTION 21)
  (:CHAR-POUND 156)
  (:CHAR-MULTIPLICATION 158)
  (:CHAR-FUNCTION 159)
  (:CHAR-RESERVED 169)
  (:CHAR-HALF 171)
  (:CHAR-ONE-QUARTER 172)
  (:CHAR-COPYRIGHT 184)
  (:CHAR-CENT 189)
  (:CHAR-YEN 190)
  (:CHAR-CURRENCY 207)
  (:CHAR-THREE-QUARTERS 243)
  (:CHAR-DIVISION 246)
  (:CHAR-GRADE 248)
  (:CHAR-UMLAUT 249)
  (:CHAR-POW1 251)
  (:CHAR-POW3 252)
  (:CHAR-POW2 253)
  (:CHAR-BULLET-SQUARE 254))

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


(define-c-enum rng-algorithm            ; TCOD_random_algo_t
  :RNG-MT
  :RNG-CMWC)

(define-c-enum rng-distribution         ; TCOD_distribution_t
  :DISTRIBUTION-LINEAR
  :DISTRIBUTION-GAUSSIAN
  :DISTRIBUTION-GAUSSIAN-RANGE
  :DISTRIBUTION-GAUSSIAN-INVERSE
  :DISTRIBUTION-GAUSSIAN-RANGE-INVERSE)


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


;; TCOD_bsp_t = a struct
;; but all variables pass a pointer to this struct
(define-c-type bsp-ptr :pointer)


;; TCOD_random_t = pointer to void
(define-c-type randomptr :pointer)


;; TCOD_parser_t = pointer to void
(define-c-type parser :pointer)


;; TCOD_zip_t = pointer to void
(define-c-type zipptr :pointer)


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
  (dcx 0 :type sint16)  ;; movement since last update in console cells
  (dcy 0 :type sint16)
  (lbutton nil :type boolean)                ;; left button status
  (rbutton nil :type boolean)                ;; right button status
  (mbutton nil :type boolean)                ;; middle button status
  (lbutton-pressed nil :type boolean)        ;; left button pressed event
  (rbutton-pressed nil :type boolean)        ;; right button pressed event
  (mbutton-pressed nil :type boolean)        ;; middle button pressed event
  (wheel-up nil :type boolean)
  (wheel-down nil :type boolean))

;; TCOD_image_t = pointer to void
(define-c-type image :pointer)

;; TCOD_noise_t = pointer to void
(define-c-type noise :pointer)

;; TCOD_heightmap_t = a struct
;; but all functions pass/take pointers to heightmaps
(define-c-type heightmap-ptr :pointer)

;; TCOD_map_t = pointer to void
(define-c-type mapptr :pointer)

;; TCOD_path_t = pointer to void
(define-c-type a*-path :pointer)

;; TCOD_dijkstra_t = pointer to void
(define-c-type dijkstra-path :pointer)


;;;; <<Utilities>> ============================================================


(defun* (get-bit -> boolean) ((n integer) (pos uint8))
  "Return the bit at position POS within the integer N (represented as
a bitfield). POS = 1 refers to the 1's (rightmost) bit."
  ;;(/= 0 (logand n (expt 2 (1- pos))))
  (logtest n (ash 1 (1- pos))))



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
            (or (key-lctrl key2) (key-rctrl key2)))
       (eql (or (key-lmeta key1) (key-rmeta key1))
            (or (key-lmeta key2) (key-rmeta key2)))))



;;;; <<Colours>> ==============================================================


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* (compose-colour -> uint32) ((r uint8) (g uint8) (b uint8))
    "Given three integer values R, G and B, representing the red, green and
blue components of a colour, return a 3 byte integer whose value is #xBBGGRR."
    (+ (ash b 16) (ash g 8) r))
  (defun compose-color (r g b) (compose-colour r g b)))


(defun* (colour-rgb -> uint32) ((r uint8) (g uint8) (b uint8))
  (compose-colour r g b))
(defun color-rgb (r g b) (colour-rgb r g b))


(defun* (colour-hsv -> uint32) ((hue single-float) (sat single-float)
                                (val single-float))
  "Return a new colour with the given HSV (hue, saturation and value)
components."
  (cond
   ((zerop sat)
    ;; Achromatic grey
    (compose-colour (truncate (+ 0.5 (* 255.0 val)))
                    (truncate (+ 0.5 (* 255.0 val)))
                    (truncate (+ 0.5 (* 255.0 val)))))
   (t
    (loop while (minusp hue) do (incf hue 360.0))
    (loop while (>= hue 360.0) do (decf hue 360.0))
    (setf hue (/ hue 60.0))
    (let* ((i (truncate hue))
           (f (- hue i))
           (p (* val (- 1 sat)))
           (q (* val (- 1 (* sat f))))
           (z (* val (- 1 (* sat (- 1 f)))))) ; variable t renamed to z
      (compose-colour
       (truncate
        (+ 0.5 (* 255.0 (case i
                          (1 q)
                          ((2 3) p)
                          (4 z)
                          (t val)))))
       (truncate
        (+ 0.5 (* 255.0 (case i
                          ((1 2) val)
                          (3 q)
                          (t p)))))
       (truncate
        (+ 0.5 (* 255.0 (case i
                          ((0 1) p) (2 z) ((3 4) val) (t q))))))))))
(defun color-hsv (hue sat val) (colour-hsv hue sat val))


(defun* (decompose-colour -> (values uint8 uint8 uint8)) ((num colournum))
  "Given a colournum #xBBGGRR, return R, G and B integer values as 3 separate
return values."
  (values
   (logand num #x0000ff)
   (ash (logand num #x00ff00) -8)
   (ash (logand num #xff0000) -16)
   ))
(defun decompose-color (num) (decompose-colour num))


(defun* (invert-colour -> colournum) ((num colournum))
  (multiple-value-bind (r g b) (decompose-colour num)
    (compose-colour (- 255 r) (- 255 g) (- 255 b))))
(defun invert-color (num) (invert-colour num))


(defvar *colour-table* nil)
(defvar *initial-colours*
  `((:true-black        #x00 #x00 #x00)
    (:true-pink                 #xFF #x00 #xFF)
    (:true-white        #xFF #xFF #xFF)
    (:true-red          #xFF #x00 #x00)
    (:true-green        #x00 #xFF #x00)
    (:true-blue                 #x00 #x00 #xFF)
    (:black             #x00 #x00 #x00)
    (:dark-grey         96 96 96)
    (:grey              196 196 196)
    (:white             255 255 255)
    (:blue              13 103 196)
    (:dark-blue         40 40 128)
    (:light-blue        120 120 255)
    (:dark-red                  128 0 0)
    (:light-red         255 100 50)
    (:dark-brown        32 16 0)
    (:light-yellow      255 255 150)
    (:yellow            255 255 0)
    (:dark-yellow       164 164 0)
    (:green             0 220 0)
    (:cyan              86 163 205)
    (:orange            255 150 0)
    (:red               255 0 0)
    (:silver            203 203 203)
    (:gold              255 255 102)
    (:purple            204 51 153)
    (:dark-purple       51 0 51)
    ;; Some colours not defined in TCOD.
    (:slate-grey        #x80 #x80 #x80)
    (:umber             #x80 #x40 0)
    (:pink              #xFF #x00 #xFF)
    (:chocolate         210 105 30)))


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


(defun* (colour -> colournum) ((keywd (or colournum symbol))
                               &optional (error? nil))
  "Given a colour keyword such as :GREY, return its corresponding RGB
value (#xRRGGBB). If the keyword is unrecognised, then either return
a light grey colour, or raise an error (if `error?' is non-nil)."
  (cond
    ((integerp keywd)
     keywd)
    (t
     (unless *colour-table*
       (start-colours))
     (or (gethash keywd *colour-table*)
         (if error? (error "Unrecognised colour name `~S'" keywd))
         #xD3D3D3))))

(defun color (keywd) (colour keywd))


(defun* (colour->keyword -> (or keyword null)) ((colournum colournum))
  (maphash
   (lambda (k v)
     (if (= v colournum)
         (return-from colour->keyword k)))
   *colour-table*))


(defun* (color->keyword -> (or keyword null)) ((colournum colournum))
  (colour->keyword colournum))


(defun colctrl->char (ctrl)
  (code-char (foreign-enum-value 'colctrl ctrl)))



;; TCODLIB_API bool TCOD_color_equals (TCOD_color_t c1, TCOD_color_t c2);
(define-c-function ("TCOD_color_equals_wrapper" colour-equals?) :boolean
  ((c1 colournum) (c2 colournum)))
(defun color-equals? (c1 c2)
  (colour-equals? c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_add (TCOD_color_t c1, TCOD_color_t c2);
(define-c-function ("TCOD_color_add_wrapper" colour-add) colournum
  ((c1 colournum) (c2 colournum)))
(defun color-add (c1 c2)
  (colour-add c1 c2))


(define-c-function ("TCOD_color_subtract_wrapper" colour-subtract) colournum
  ((c1 colournum) (c2 colournum)))
(defun color-subtract (c1 c2)
  (colour-subtract c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply (TCOD_color_t c1,
;; TCOD_color_t c2);
(define-c-function ("TCOD_color_multiply_wrapper" colour-multiply) colournum
  ((c1 colournum) (c2 colournum)))
(defun color-multiply (c1 c2)
  (colour-multiply c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply_scalar (TCOD_color_t c1,
;; float value);
(define-c-function ("TCOD_color_multiply_scalar_wrapper" colour-multiply-scalar)
  colournum
  ((c1 colournum) (value :float)))
(defun color-multiply-scalar (c1 value)
  (colour-multiply-scalar c1 value))


;; TCODLIB_API TCOD_color_t TCOD_color_lerp(TCOD_color_t c1, TCOD_color_t c2,
;; float coef);
(define-c-function ("TCOD_color_lerp_wrapper" colour-lerp) colournum
  ((c1 colournum) (c2 colournum) (coef :float)))
(defun color-lerp (c1 c2 coef)
  (colour-lerp c1 c2 coef))


;; TCODLIB_API void TCOD_color_set_HSV(TCOD_color_t *c,float h, float s,
;; float v);
(define-c-function ("TCOD_color_set_HSV" colour-set-hsv) :void
  ((con :pointer) (hue :float) (sat :float) (v :float)))
(defun color-set-hsv (con hue sat v)
  (colour-set-hsv con hue sat v))


(define-c-function ("TCOD_color_get_hue_wrapper" colour-get-hue) :float
    ((c colournum)))

(define-c-function ("TCOD_color_get_saturation_wrapper" colour-get-saturation) :float
    ((c colournum)))

(define-c-function ("TCOD_color_get_value_wrapper" colour-get-value) :float
    ((c colournum)))


(defun* (colour-get-hsv -> list) ((c colournum))
  (list (colour-get-hue c)
        (colour-get-saturation c)
        (colour-get-value c)))


(defun* (colour-set-hue -> colournum) ((colour colournum) (hue single-float))
  "Return COLOUR with its hue modified to HUE."
  (let ((sat (colour-get-saturation colour))
        (val (colour-get-value colour)))
    (colour-hsv hue sat val)))


(defun* (colour-set-saturation -> colournum) ((colour colournum)
                                              (sat single-float))
  "Return COLOUR with its saturation modified to SAT."
  (let ((hue (colour-get-hue colour))
        (val (colour-get-value colour)))
    (colour-hsv hue sat val)))


(defun* (colour-set-value -> colournum) ((colour colournum) (val single-float))
  "Return COLOUR with its HSV value modified to VAL."
  (let ((sat (colour-get-saturation colour))
        (hue (colour-get-hue colour)))
    (colour-hsv hue sat val)))


(defun* (colour-shift-hue -> colournum) ((colour colournum)
                                         (hshift single-float))
  (if (zerop hshift)
      colour
      (destructuring-bind (h s v) (colour-get-hsv colour)
        (colour-hsv (+ h hshift) s v))))


(defun* (colour-scale-hsv -> colournum) ((colour colournum) (scoef single-float)
                                         (vcoef single-float))
  (destructuring-bind (h s v) (colour-get-hsv colour)
    (colour-hsv h (clamp 0.0 1.0 (* s scoef)) (clamp 0.0 1.0 (* v vcoef)))))


(defun color-get-hsv (colour)
  (colour-get-hsv colour))

(defun color-get-hue (colour)
  (colour-get-hue colour))

(defun color-get-saturation (colour)
  (colour-get-saturation colour))

(defun color-get-value (colour)
  (colour-get-value colour))

(defun color-set-hue (colour hue)
  (colour-set-hue colour hue))

(defun color-set-saturation (colour sat)
  (colour-set-saturation colour sat))

(defun color-set-value (colour val)
  (colour-set-value colour val))

(defun color-shift-hue (colour hshift)
  (colour-shift-hue colour hshift))



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

(define-c-function ("TCOD_console_credits_render" console-credits-render)
    :boolean
  ((x :int) (y :int) (alpha :boolean)))


;;TCODLIB_API void TCOD_console_init_root(int w, int h, const char * title,
;;                                        bool fullscreen);

(defcfun ("TCOD_console_init_root" %console-init-root) :void
  (width :int) (height :int) (title :string) (fullscreen? :boolean)
  (renderer renderer))


(defun* console-init-root ((width uint) (height uint)
                           &key (title "libtcod")
                                 (fullscreen? nil)
                           (renderer :RENDERER-SDL))
  ;; use SDL by default, as that is all that seems to be supported in TCOD currently
  (check-type width ucoord)
  (check-type height ucoord)
  (setf (gethash *root* *console-width-table*) width)
  (setf (gethash *root* *console-height-table*) height)
  (%console-init-root width height title fullscreen? renderer)
  (setf *root-console-initialised?* t)
  *root*)


(defun* (console-initialised? -> boolean) ()
  *root-console-initialised?*)

;;TCODLIB_API void TCOD_console_set_custom_font(const char *fontFile, int
;;                        char_width, int char_height, int nb_char_horiz, int
;;                        nb_char_vertic, bool chars_by_row, TCOD_color_t
;;                        key_color);
(defcfun ("TCOD_console_set_custom_font" %console-set-custom-font)
    :void
  (fontfile :string) (flags custom-font-flags)
  (chars-horizontal :int) (chars-vertical :int))


(defun* (console-set-custom-font -> (values)) ((fontfile (or string pathname))
                                               (flags list)
                                               &optional (chars-horizontal 0)
                                                         (chars-vertical 0))
  (assert (probe-file fontfile))
  (if (pathnamep fontfile)
      (setf fontfile (namestring fontfile)))
  (check-type chars-horizontal (unsigned-byte 16))
  (check-type chars-vertical (unsigned-byte 16))
  (%console-set-custom-font fontfile flags chars-horizontal chars-vertical)
  (values))


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
(define-c-function ("TCOD_console_set_window_title" console-set-window-title)
    :void
  ((title :string)))

;;TCODLIB_API void TCOD_console_set_fullscreen(bool fullscreen);
(define-c-function ("TCOD_console_set_fullscreen" console-set-fullscreen)
    :void
  ((full? :boolean)))

;;TCODLIB_API bool TCOD_console_is_fullscreen();
(define-c-function ("TCOD_console_is_fullscreen" console-is-fullscreen?)
    :boolean
    ())

;;TCODLIB_API bool TCOD_console_is_window_closed();
(define-c-function ("TCOD_console_is_window_closed" console-is-window-closed?)
    :boolean ())


;;TCODLIB_API void TCOD_console_set_background_color(TCOD_console_t con,
;; TCOD_color_t col);
(define-c-function ("TCOD_console_set_default_background_wrapper"
                    console-set-default-background) :void
  ((con console) (col colournum)))


;;TCODLIB_API void TCOD_console_set_foreground_color(TCOD_console_t con,
;;                                                   TCOD_color_t col);
(define-c-function ("TCOD_console_set_default_foreground_wrapper"
                    console-set-default-foreground) :void
  ((con console) (col colournum)))


;;TCODLIB_API TCOD_color_t TCOD_console_get_background_color(TCOD_console_t
;;con);
(define-c-function ("TCOD_console_get_default_background_wrapper"
                    console-get-default-background) colournum
  ((con console)))


;;TCODLIB_API TCOD_color_t TCOD_console_get_foreground_color(TCOD_console_t con);
(define-c-function ("TCOD_console_get_default_foreground_wrapper"
                    console-get-default-foreground) colournum
  ((con console)))


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
    (con console) (x :int) (y :int) (ch :unsigned-int))


(defun* console-set-char ((con console) (x integer) (y integer)
                          (ch (or character integer)))
  (assert (legal-console-coordinates? con x y))
  (when (characterp ch)
    (setf ch (char-code ch)))
  (%console-set-char con x y ch))


(defun* (console-fill-char -> null)  ((con console) (ch (or character uint))
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
    ((con console) (x :int) (y :int) (ch :unsigned-int)
     (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


(define-c-function ("TCOD_console_put_char_ex_wrapper" console-put-char-ex)
    :void
    ((con console) (x :int) (y :int) (ch :unsigned-int) (fg colournum)
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
;;      (con console) (x :int) (y :int) (flag background-flag) (fmt :string)
;;      &rest)
;;
;; (defun* console-print-right ((con console) (x ucoord) (y ucoord)
;;                              (flag background-flag) (fmt string) &rest args)
;;   (assert (legal-console-coordinates? con x y))
;;   (%console-print-right con x y flag (apply #'format nil fmt args)))

;;TCODLIB_API void TCOD_console_print_center(TCOD_console_t con,int x, int y,
;; TCOD_bkgnd_flag_t flag, const char *fmt, ...);
;; (defcfun ("TCOD_console_print_center" %console-print-centre) :void
;;      (con console) (x :int) (y :int) (flag background-flag) (fmt :string)
;;      &rest)
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
;;      (con console) (x :int) (y :int) (w :int) (h :int)
;;      (flag background-flag) (fmt :string)
;;      &rest)
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
;;      (con console) (x :int) (y :int) (w :int) (h :int)
;;      (flag background-flag) (fmt :string)
;;      &rest)
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


(define-c-function ("TCOD_console_hline" console-hline) :void
    ((con console) (x :int) (y :int) (len :int) (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


(define-c-function ("TCOD_console_vline" console-vline) :void
    ((con console) (x :int) (y :int) (len :int) (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))

(defcfun ("TCOD_console_print_frame" %console-print-frame) :void
  (con console) (x :int) (y :int) (width :int) (height :int)
  (empty? :boolean) (flag background-flag)
  (fmt :string) &rest)

(defun* console-print-frame ((con console) (x ucoord) (y ucoord)
                             (width ucoord) (height ucoord)
                             (empty? boolean) (flag background-flag)
                             (fmt (or string null)) &rest args)
  (assert (legal-console-coordinates? con x y))
  (check-type width ucoord)
  (check-type height ucoord)
  (%console-print-frame con x y width height empty? flag
                        (if fmt (apply #'format nil fmt args)
                            +NULL+)))


;; Added in wrappers.c
(defcfun ("TCOD_console_print_double_frame" %console-print-double-frame) :void
  (con console) (x :int) (y :int) (width :int) (height :int)
  (empty? :boolean) (flag background-flag)
  (fmt :string) &rest)

(defun* console-print-double-frame ((con console) (x ucoord) (y ucoord)
                                    (width ucoord) (height ucoord)
                                    (empty? boolean) (flag background-flag)
                                    (fmt (or string null)) &rest args)
  (assert (legal-console-coordinates? con x y))
  (check-type width ucoord)
  (check-type height ucoord)
  (%console-print-double-frame con x y width height empty? flag
                               (if fmt (apply #'format nil fmt args)
                                   +NULL+)))



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
    ;; In libtcod 1.5.1, the whole console is blitted if wsrc and hsrc are 0
    (assert (and (or (and (zerop wsrc) (zerop hsrc))
                     (and (plusp wsrc) (plusp hsrc)))
                 (>= (+ xdest wsrc) 0) (>= (+ ydest hsrc) 0)))
    (call-it src xsrc ysrc wsrc hsrc dest xdest ydest
             foreground-alpha background-alpha)))



;;TCODLIB_API void TCOD_console_delete(TCOD_console_t console);
(define-c-function ("TCOD_console_delete" console-delete) :void
    ((con console)))


#+nil
(define-c-function ("TCOD_console_set_key_color_wrapper" console-set-key-colour)
    :void
    ((con console) (colour colournum)))


;;;; <<Unicode>> ==============================================================



(define-c-function ("TCOD_console_map_string_to_font_utf"
                    %console-map-string-to-font-utf) :void
    ((str :string) (fontchar-x :int) (fontchar-y :int)))


(defun* console-map-string-to-font-utf ((str string) (fontchar-x uint)
                                        (fontchar-y uint))
  (with-foreign-pointer-as-string (strbuf (length str))
    (%console-map-string-to-font-utf
     (lisp-string-to-foreign str strbuf (length str)
                             :encoding :utf-16)
     fontchar-x fontchar-y)))


(defcfun ("TCOD_console_print_utf" %console-print-utf) :void
  (con console) (x :int) (y :int) (fmt :string) &rest)

(defun* console-print-utf ((con console) (x ucoord) (y ucoord)
                           (fmt string) &rest args)
  (let ((str (apply #'format nil fmt args)))
    (with-foreign-pointer-as-string (strbuf (length str))
      (%console-print-utf con x y
                          (lisp-string-to-foreign str strbuf (length str)
                                                  :encoding :utf-16)))))


(defcfun ("TCOD_console_print_ex_utf" %console-print-ex-utf) :void
  (con console) (x :int) (y :int) (flag background-flag) (align alignment)
  (fmt :string) &rest)


(defun* console-print-ex-utf ((con console) (x ucoord) (y ucoord)
                              (flag background-flag) (align alignment)
                              (fmt string) &rest args)
  (let ((str (apply #'format nil fmt args)))
    (with-foreign-pointer-as-string (strbuf (length str))
      (%console-print-ex-utf con x y flag align
                             (lisp-string-to-foreign str strbuf (length str)
                                                     :encoding :utf-16)))))


(defcfun ("TCOD_console_print_rect_utf" %console-print-rect-utf) :void
  (con console) (x :int) (y :int) (w :int) (h :int)
  (fmt :string) &rest)


(defun* console-print-rect-utf ((con console) (x ucoord) (y ucoord)
                                (w ucoord) (h ucoord)
                                (fmt string) &rest args)
  (let ((str (apply #'format nil fmt args)))
    (with-foreign-pointer-as-string (strbuf (length str))
      (%console-print-rect-utf con x y w h
                               (lisp-string-to-foreign str strbuf (length str)
                                                       :encoding :utf-16)))))


(defcfun ("TCOD_console_print_rect_ex_utf" %console-print-rect-ex-utf) :void
  (con console) (x :int) (y :int) (w :int) (h :int)
  (flag background-flag) (align alignment) (fmt :string) &rest)


(defun* console-print-rect-ex-utf ((con console) (x ucoord) (y ucoord)
                                                 (w ucoord) (h ucoord)
                                                 (flag background-flag)
                                                 (align alignment)
                                                 (fmt string) &rest args)
  (let ((str (apply #'format nil fmt args)))
    (with-foreign-pointer-as-string (strbuf (length str))
      (%console-print-rect-ex-utf con x y w h flag align
                                  (lisp-string-to-foreign str strbuf
                                                          (length str)
                                                          :encoding :utf-16)))))
`<

(defcfun ("TCOD_console_get_height_rect_utf" %console-get-height-rect-utf) :void
  (con console) (x :int) (y :int) (w :int) (h :int)
  (fmt :string) &rest)


(defun* console-get-height-rect-utf ((con console) (x ucoord) (y ucoord)
                                                   (w ucoord) (h ucoord)
                                                   (fmt string) &rest args)
  (let ((str (apply #'format nil fmt args)))
    (with-foreign-pointer-as-string (strbuf (length str))
      (%console-get-height-rect-utf con x y w h
                                    (lisp-string-to-foreign
                                     str strbuf
                                     (length str)
                                     :encoding :utf-16)))))



;;;; <<Keyboard input>> ========================================================

#-darwin
(defun key->keypress (keyptr)
  (with-foreign-slots ((vk c text flag-pressed flag-lalt flag-lctrl flag-lmeta flag-ralt
                           flag-rctrl flag-rmeta flag-shift)
                       keyptr (:struct key-press))
    (make-key :vk vk
              :c (code-char c)
              :pressed flag-pressed
              :lalt flag-lalt
              :ralt flag-ralt
              :lctrl flag-lctrl
              :rctrl flag-rctrl
              :lmeta flag-lmeta
              :rmeta flag-rmeta
              :shift flag-shift)))


#+darwin
(defun key->keypress (keyptr)
  (with-foreign-slots ((vk c text flag-pressed flag-lalt flag-lctrl flag-lmeta flag-ralt
                           flag-rctrl flag-rmeta flag-shift)
                       keyptr (:struct key-press))
    ;; (loop for i from 0 upto 45 do
    ;;   (format t "~D:~S " i (mem-ref keyptr :uint8 i)))
    ;; (terpri)
    (make-key :vk vk :c (code-char c)
              :pressed (not (zerop flag-pressed))
              :lalt (not (zerop flag-lalt))
              :ralt (not (zerop flag-ralt))
              :lctrl (not (zerop flag-lctrl))
              :rctrl (not (zerop flag-rctrl))
              :lmeta (not (zerop flag-lmeta))
              :rmeta (not (zerop flag-rmeta))
              :shift (not (zerop flag-shift)))))


  ;; (make-key
  ;;  :vk (foreign-slot-value keyptr '(:struct key-press) 'vk)
  ;;  :c (code-char (foreign-slot-value keyptr '(:struct key-press) 'c))
  ;;  :pressed (foreign-slot-value keyptr '(:struct key-press) 'flag-pressed)
  ;;  :lalt (foreign-slot-value keyptr '(:struct key-press) 'flag-lalt)
  ;;  :lctrl (foreign-slot-value keyptr '(:struct key-press) 'flag-lctrl)
  ;;  :ralt (foreign-slot-value keyptr '(:struct key-press) 'flag-ralt)
  ;;  :rctrl (foreign-slot-value keyptr '(:struct key-press) 'flag-rctrl)
  ;;  :shift (foreign-slot-value keyptr '(:struct key-press) 'flag-shift)))


(defun key-bitfield->vk (key-bf)
  (foreign-enum-keyword 'keycode
                        (logand (ash key-bf -16) #x00FF)))


(defcfun ("TCOD_console_check_for_keypress" console-check-for-keypress)
    (:struct key-press)
  (flags key-state))

;;; changed from :bool to :int even though it is a bool in TCOD definition
;;; CFFI bug, see https://bugs.launchpad.net/cffi/+bug/1517578
(defcfun ("TCOD_console_wait_for_keypress" %console-wait-for-keypress)
    (:struct key-press)
  (flush? :int))

(defun console-wait-for-keypress (flush?)
  (%console-wait-for-keypress (if flush? 1 0)))

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


;;;; <<Name generation>> ======================================================


(define-c-function ("TCOD_namegen_parse" namegen-parse) :void
    ((filename :string) (rng randomptr)))

(define-c-function ("TCOD_namegen_generate" namegen-generate) :string
    ((name :string) (allocate? :boolean)))

(define-c-function ("TCOD_namegen_generate_custom" namegen-generate-custom)
    :string
    ((name :string) (rule :string) (allocate? :boolean)))

(define-c-function ("TCOD_namegen_destroy" namegen-destroy) :void
    ())


;;;; <<Compression toolkit>> ==================================================


(define-c-function ("TCOD_zip_new" zip-new) zipptr
    ())

(define-c-function ("TCOD_zip_delete" zip-delete) :void
    ((zip zipptr)))

(define-c-function ("TCOD_zip_put_char" zip-put-char) :void
    ((zip zipptr) (ch :char)))

(define-c-function ("TCOD_zip_put_int" zip-put-int) :void
    ((zip zipptr) (val :int)))

(define-c-function ("TCOD_zip_put_float" zip-put-float) :void
    ((zip zipptr) (val :float)))

(define-c-function ("TCOD_zip_put_string" zip-put-string) :void
    ((zip zipptr) (val :string)))

(define-c-function ("TCOD_zip_put_data" zip-put-data) :void
    ((zip zipptr) (nbytes :int) (data :pointer)))

(define-c-function ("TCOD_zip_put_image" zip-put-image) :void
    ((zip zipptr) (image image)))

(define-c-function ("TCOD_zip_put_console" zip-put-console) :void
    ((zip zipptr) (con console)))

(defun zip-put (zip val)
  (typecase val
    (string (zip-put-string zip val))
    (character (zip-put-char zip (char-code val)))
    (integer (zip-put-int zip val))
    (float (zip-put-float zip val))
    (otherwise (error "ZIP-PUT: don't know how to translate value ~S" val))))

(defun* zip-put-colour (zip (colour colournum))
  (multiple-value-bind (r g b) (decompose-colour colour)
    (zip-put-char zip r)
    (zip-put-char zip g)
    (zip-put-char zip b)))

(defun zip-put-color (zip color)
  (zip-put-colour zip color))

(define-c-function ("TCOD_zip_save_to_file" zip-save-to-file) :int
    ((zip zipptr) (filename :string)))

(define-c-function ("TCOD_zip_load_from_file" zip-load-from-file) :int
    ((zip zipptr) (filename :string)))

(define-c-function ("TCOD_zip_get_char" zip-get-char) :char
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_int" zip-get-int) :int
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_float" zip-get-float) :float
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_string" zip-get-string) :string
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_data" zip-get-data) :int
    ((zip zipptr) (nbytes :int) (data :pointer)))

(define-c-function ("TCOD_zip_get_image" zip-get-image) image
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_console" zip-get-console) console
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_current_bytes" zip-get-current-bytes) :uint32
    ((zip zipptr)))

(define-c-function ("TCOD_zip_get_remaining_bytes"
                    zip-get-remaining-bytes) :uint32
    ((zip zipptr)))

(define-c-function ("TCOD_zip_skip_bytes" zip-skip-bytes) :void
    ((zip zipptr) (nbytes :uint32)))


(defun* zip-get-colour ((zip zipptr))
  (let ((r (zip-get-char zip))
        (g (zip-get-char zip))
        (b (zip-get-char zip)))
    (compose-colour r g b)))

(defun zip-get-color (zip)
  (zip-get-colour zip))


;;;; <<System>> ===============================================================

;;TCODLIB_API TCOD_event_t TCOD_sys_wait_for_event(int eventMask,
;;  TCOD_key_t *key, TCOD_mouse_t *mouse, bool flush);
(define-c-function ("TCOD_sys_wait_for_event" sys-wait-for-event) event
  ((eventmask event) (key :pointer) (mouseptr :pointer) (flush? :boolean)))

;;TCODLIB_API TCOD_event_t TCOD_sys_check_for_event(int eventMask,
;;  TCOD_key_t *key, TCOD_mouse_t *mouse);
(define-c-function ("TCOD_sys_check_for_event" sys-check-for-event) event
  ((eventmask event) (key :pointer) (mouseptr :pointer)))


(defun sys-get-events ()
  "User-friendly wrapper for the new input event model in libtcod 1.5.1rc1.
When called, returns a list of all queued events (calling the function
also EMPTIES the queue). Each element in the list is a cons cell of the
form (EVENT-TYPE . DATA) where EVENT-TYPE is a member of the `event' enum,
and DATA is either a key struct or a mouse-state struct."
  (cffi:with-foreign-objects ((keyptr '(:struct key-press))
                              (mouseptr '(:struct mouse-state)))
    (loop
      for event = (sys-check-for-event :event-any keyptr mouseptr)
      when (member event '(:event-key-release :event-key-press :event-key))
        collect (cons event (key->keypress keyptr))
      when (member event '(:event-mouse-release :event-mouse-press
                           :event-mouse-move))
        collect (let ((mouse (parse-mouse-state mouseptr))
                      (bits (sdl-get-mouse-state +null+ +null+)))
                  (setf (mouse-lbutton mouse) (plusp (boole boole-and bits 1)))
                  (setf (mouse-mbutton mouse) (plusp (boole boole-and bits 2)))
                  (setf (mouse-rbutton mouse) (plusp (boole boole-and bits 4)))
                  (setf (mouse-lbutton-pressed mouse) nil
                        (mouse-mbutton-pressed mouse) nil
                        (mouse-rbutton-pressed mouse) nil)
                  (cons event mouse))
      until (eql event :event-none))))

(defun sys-wait-events (filter flush)
  "Like the wrapper sys-get-events, but using TCOD_sys_wait_for_event. Takes a filter
   for what event (`event' enum) to wait for as well as whether or not to flush
   the event buffer of all pending events."
  (cffi:with-foreign-objects ((keyptr '(:struct key-press))
                              (mouseptr '(:struct mouse-state)))
    (let ((event (sys-wait-for-event filter keyptr mouseptr flush)))
      (if (member event '(:event-key-release :event-key-press :event-key))
          (cons event (key->keypress keyptr))
          (if (member event '(:event-mouse-release :event-mouse-press
                              :event-mouse-move))
              (let ((mouse (parse-mouse-state mouseptr))
                    (bits (sdl-get-mouse-state +null+ +null+)))
                (setf (mouse-lbutton mouse) (plusp (boole boole-and bits 1)))
                (setf (mouse-mbutton mouse) (plusp (boole boole-and bits 2)))
                (setf (mouse-rbutton mouse) (plusp (boole boole-and bits 4)))
                (setf (mouse-lbutton-pressed mouse) nil
                      (mouse-mbutton-pressed mouse) nil
                      (mouse-rbutton-pressed mouse) nil)
                (cons event mouse)))))))

(define-c-function ("TCOD_sys_create_directory" sys-create-directory) :boolean
    ((path :string)))

(define-c-function ("TCOD_sys_delete_directory" sys-delete-directory) :boolean
    ((path :string)))

(define-c-function ("TCOD_sys_elapsed_milli" sys-elapsed-milli) :uint32
    ())

(define-c-function ("TCOD_sys_elapsed_seconds" sys-elapsed-seconds) :float
    ())

(define-c-function ("TCOD_sys_get_last_frame_length"
                    sys-get-last-frame-length) :float
    ())

(define-c-function ("TCOD_sys_sleep_milli" sys-sleep-milli) :void
    ((val :unsigned-int)))

(defcfun ("TCOD_sys_save_screenshot" %sys-save-screenshot) :void
  (filename :string))

(defun sys-save-screenshot (&optional (filename (null-pointer)))
  (%sys-save-screenshot filename))

(define-c-function ("TCOD_sys_set_fps" sys-set-fps) :void
    ((val :int)))

(define-c-function ("TCOD_sys_get_fps" sys-get-fps) :int
    ())

(define-c-function ("TCOD_sys_register_SDL_renderer"
                    sys-register-SDL-renderer) :void
    ((callback :pointer)))

;; Lisp wrapper needed because actual function returns nothing, whereas we
;; want to return resolution.
(defcfun ("TCOD_sys_get_current_resolution_x" sys-get-current-resolution-x)
    :int)
(defcfun ("TCOD_sys_get_current_resolution_y" sys-get-current-resolution-y)
    :int)

(defun sys-get-current-resolution ()
  (values (sys-get-current-resolution-x)
          (sys-get-current-resolution-y)))

(define-c-function ("TCOD_sys_force_fullscreen_resolution"
                    sys-force-fullscreen-resolution) :void
    ((width :int) (height :int)))

(define-c-function ("TCOD_sys_get_renderer" sys-get-renderer) renderer
    ()
  "Return the currently active renderer.")


(define-c-function ("TCOD_sys_set_renderer" sys-set-renderer) :void
    ((renderer renderer))
  "Change the currently active renderer.")


(defcfun ("TCOD_sys_get_char_size" %sys-get-char-size) :void
  (widthptr :pointer) (heightptr :pointer))

(defun* (sys-get-char-size -> (values fixnum fixnum)) ()
  "Return the dimensions of each character in the current font bitmap."
  (with-foreign-object (width :int)
    (with-foreign-object (height :int)
      (%sys-get-char-size width height)
      (values (mem-aref width :int) (mem-aref height :int)))))

(defcfun ("TCOD_sys_get_fullscreen_offsets"
          %sys-get-fullscreen-offsets) :void
  (offx-ptr :pointer) (offy-ptr :pointer))

(defun* (sys-get-fullscreen-offsets -> (values fixnum fixnum)) ()
  (with-foreign-object (offx :int)
    (with-foreign-object (offy :int)
      (%sys-get-fullscreen-offsets offx offy)
      (values (mem-aref offx :int) (mem-aref offy :int)))))

(define-c-function ("TCOD_sys_clipboard_set" sys-clipboard-set) :void
    ((text :string)))

(define-c-function ("TCOD_sys_clipboard_get" sys-clipboard-get) :string
    ())

(define-c-function ("TCOD_sys_update_char" sys-update-char) :void
    ((ascii :int) (fontx :int) (fonty :int) (image image) (x :int) (y :int)))


;;;; <<Random>> ===============================================================


;;; mersenne.h

(define-c-function ("TCOD_random_new" random-new) randomptr
  ((algorithm rng-algorithm)))

(define-c-function ("TCOD_random_new_from_seed" random-new-from-seed) randomptr
  ((algorithm rng-algorithm) (seed :uint32)))

(define-c-function ("TCOD_random_get_instance" random-get-instance) randomptr
    ())

(define-c-function ("TCOD_random_delete" random-delete) :void
  ((rng randomptr)))

(define-c-function ("TCOD_random_save" random-save) randomptr
  ((rng randomptr)))

(define-c-function ("TCOD_random_restore" random-restore) :void
  ((rng randomptr) (backup randomptr)))

(define-c-function ("TCOD_random_set_distribution" random-set-distribution)
    :void
  ((rng randomptr) (dist rng-distribution)))

(define-c-function ("TCOD_random_get_int" random-get-int) :int
  ((rng randomptr) (min :int) (max :int)))

(define-c-function ("TCOD_random_get_float" random-get-float) :float
  ((rng randomptr) (min :float) (max :float)))

(define-c-function ("TCOD_random_get_double" random-get-double) :double
  ((rng randomptr) (min :double) (max :double)))

(define-c-function ("TCOD_random_get_int_mean" random-get-int-mean) :int
  ((rng randomptr) (min :int) (max :int) (mean :int)))

(define-c-function ("TCOD_random_get_float_mean" random-get-float-mean) :float
  ((rng randomptr) (min :float) (max :float) (mean :float)))

(define-c-function ("TCOD_random_get_double_mean" random-get-double-mean)
    :double
  ((rng randomptr) (min :double) (max :double) (mean :double)))



;;;; <<Mouse>> ================================================================


;; (defcfun ("TCOD_mouse_get_x" mouse-get-x) :int)
;; (defcfun ("TCOD_mouse_get_y" mouse-get-y) :int)
;; (defcfun ("TCOD_mouse_get_cx" mouse-get-cx) :int)
;; (defcfun ("TCOD_mouse_get_cy" mouse-get-cy) :int)
;; (defcfun ("TCOD_mouse_get_dx" mouse-get-dx) :int)
;; (defcfun ("TCOD_mouse_get_dy" mouse-get-dy) :int)
;; (defcfun ("TCOD_mouse_get_dcx" mouse-get-dcx) :int)
;; (defcfun ("TCOD_mouse_get_dcy" mouse-get-dcy) :int)
;; (defcfun ("TCOD_mouse_get_lbutton" mouse-get-lbutton) :unsigned-int)
;; (defcfun ("TCOD_mouse_get_mbutton" mouse-get-mbutton) :unsigned-int)
;; (defcfun ("TCOD_mouse_get_rbutton" mouse-get-rbutton) :unsigned-int)
;; (defcfun ("TCOD_mouse_get_lbutton_pressed" mouse-get-lbutton-pressed)
;;     :unsigned-int)
;; (defcfun ("TCOD_mouse_get_mbutton_pressed" mouse-get-mbutton-pressed)
;;     :unsigned-int)
;; (defcfun ("TCOD_mouse_get_rbutton_pressed" mouse-get-rbutton-pressed)
;;     :unsigned-int)



(defun* (mouse-get-status -> mouse) (&optional (update? nil))
  "Note that as of libtcod 1.5.1rc1, `mouse-get-status' returns
information about the status of the mouse as at the last time
`sys-check-for-event' was called. If you want the *current* status
of the mouse to be returned instead, UPDATE? should be non-nil."
  (with-foreign-object (msptr '(:struct mouse-state))
    (cond
      (update?
       ;; sys-check-for-event only checks and removes ONE event.
       (sys-check-for-event :event-any +null+ msptr))
      (t
       (%mouse-get-status msptr)))
    (parse-mouse-state msptr)))


;;(defctype mouse-state-pointer (:pointer (:struct mouse-state)))


(defun parse-mouse-state (mouseptr)
  (with-foreign-slots ((x y dx dy cx cy dcx dcy
                          lbutton rbutton mbutton
                          lbutton-pressed rbutton-pressed
                          mbutton-pressed wheel-up wheel-down)
                       mouseptr (:struct mouse-state))
    (make-mouse :x x :y y :dx dx :dy dy
                :cx cx :cy cy :dcx dcx :dcy dcy
                :lbutton lbutton :rbutton rbutton :mbutton mbutton
                :lbutton-pressed lbutton-pressed
                :rbutton-pressed rbutton-pressed
                :mbutton-pressed mbutton-pressed
                :wheel-up wheel-up :wheel-down wheel-down)))


;;TCODLIB_API TCOD_mouse_t TCOD_mouse_get_status();
(defcfun ("TCOD_mouse_get_status_wrapper" %mouse-get-status) :void
  (mouseptr :pointer))

;; Old version - creates a foreign struct.

;; (defun* (mouse-get-status -> mouse) ()
;;   (with-foreign-object (rodent 'mouse-state)
;;     (%mouse-get-status rodent)
;;     (mouse-state->mouse rodent)))


;; New version - gets all data from foreign functions.
;; (defun mouse-get-status ()
;;   (%mouse-get-status (null-pointer))
;;   (make-mouse :x (mouse-get-x)
;;               :y (mouse-get-y)
;;               :dx (mouse-get-dx)
;;               :dy (mouse-get-dy)
;;               :cx (mouse-get-cx)
;;               :cy (mouse-get-cy)
;;               :dcx (mouse-get-dcx)
;;               :dcy (mouse-get-dcy)
;;               :lbutton (plusp (mouse-get-lbutton))
;;               :rbutton (plusp (mouse-get-rbutton))
;;               :mbutton (plusp (mouse-get-mbutton))
;;               :lbutton-pressed (plusp (mouse-get-lbutton-pressed))
;;               :rbutton-pressed (plusp (mouse-get-rbutton-pressed))
;;               :mbutton-pressed (plusp (mouse-get-mbutton-pressed))))



(define-c-function ("TCOD_mouse_show_cursor" mouse-show-cursor) :void
  ((visible? :boolean)))

(define-c-function ("TCOD_mouse_is_cursor_visible"
                    mouse-is-cursor-visible?) :boolean
  ())

(define-c-function ("TCOD_mouse_move" mouse-move) :void
  ((pixel-x :int) (pixel-y :int)))



;;;; <<Image>> ================================================================


;;; image.h


;;TCODLIB_API TCOD_image_t TCOD_image_new(int width, int height);
(define-c-function ("TCOD_image_new" image-new) image
  ((width :int) (height :int))
  "Return a new image, filled with black.")

(define-c-function ("TCOD_image_refresh_console" image-refresh-console) :void
  ((image image) (con console)))

(define-c-function ("TCOD_image_delete" image-delete) :void
  ((image image)))

;;TCODLIB_API TCOD_image_t TCOD_image_from_console(TCOD_console_t console);
(define-c-function ("TCOD_image_from_console" image-from-console) image
  ((con console))
  "Return a new image whose contents are a 'screenshot' of the
console =CON=.")

;;TCODLIB_API TCOD_image_t TCOD_image_load(const char *filename);
(define-c-function ("TCOD_image_load" image-load) image
  ((filename :string))
  "Read an image from a file and return it.")

(define-c-function ("TCOD_image_invert" image-invert) :void
  ((image image)))

(define-c-function ("TCOD_image_hflip" image-hflip) :void
  ((image image)))

(define-c-function ("TCOD_image_vflip" image-vflip) :void
  ((image image)))

(define-c-function ("TCOD_image_rotate90" image-rotate90) :void
  ((image image) (num-rotations :int)))

(define-c-function ("TCOD_image_scale" image-scale) :void
  ((image image) (new-width :int) (new-height :int)))

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
;;      TCOD_bkgnd_flag_t bkgnd_flag, float scalex, float scaley, float angle);
(define-c-function ("TCOD_image_blit" image-blit) :void
    ((image image) (con console) (x :int) (y :int) (flag background-flag)
     (scalex :float) (scaley :float) (angle :float))
  (assert (legal-console-coordinates? con x y))
  (call-it))


;;TCODLIB_API void TCOD_image_blit_rect(TCOD_image_t image,
;; TCOD_console_t console, int x, int y, int w, int h,
;;      TCOD_bkgnd_flag_t bkgnd_flag);
(define-c-function ("TCOD_image_blit_rect" image-blit-rect) :void
    ((image image) (con console) (x :int) (y :int) (width :int) (height :int)
     (flag background-flag))
  (assert (legal-console-coordinates? con x y))
  (call-it))


(define-c-function ("TCOD_image_blit_2x" image-blit-2x) :void
    ((image image) (dest console) (dx :int) (dy :int) (sx :int) (sy :int)
     (width :int) (height :int)))


;;TCODLIB_API void TCOD_image_delete(TCOD_image_t image);
;;TCODLIB_API void TCOD_image_set_key_color(TCOD_image_t image,
;; TCOD_color_t key_color);
(define-c-function ("TCOD_image_set_key_color" image-set-key-color) :void
  ((image image) (key-color colournum)))
(defun image-set-key-colour (image key-colour)
  (image-set-key-color image key-colour))


(defcfun ("TCOD_image_get_size" %image-get-size) :void
  (image image) (widthptr :pointer) (heightptr :pointer))


(defun* (image-get-width -> uint32) ((image image))
  (with-foreign-object (width :int)
    (with-foreign-object (height :int)
      (%image-get-size image width height)
      (mem-aref width :int))))


(defun* (image-get-height -> uint32) ((image image))
  (with-foreign-object (width :int)
    (with-foreign-object (height :int)
      (%image-get-size image width height)
      (mem-aref height :int))))


(define-c-function ("TCOD_image_get_alpha" image-get-alpha) :int
  ((image image) (x :int) (y :int)))

(define-c-function ("TCOD_image_is_pixel_transparent"
                    image-is-pixel-transparent?) :boolean
  ((image image) (x :int) (y :int)))


;;;; <<Noise>> ================================================================


;; TCODLIB_API TCOD_noise_t TCOD_noise_new(int dimensions, float hurst,
;; float lacunarity, TCOD_random_t random);
;; For randomptr, use +NULL+ to use the default RNG
(defcfun ("TCOD_noise_new" %noise-new) noise
  (dimensions :int) (hurst :float) (lacunarity :float) (randomptr :pointer))

(defun* (noise-new -> noise) ((dimensions (integer 1 4))
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
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
    (%noise-get noise f)))

(defcfun ("TCOD_noise_get_ex" %noise-get-ex) :float
  (noise noise) (f :pointer) (noise-type noise-type))

(defun* (noise-get-ex -> float) ((noise noise) (noise-type noise-type)
                                 &rest nums)
  "Returns the flat noise function at the given coordinates,
using noise type NOISE-TYPE."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
    (%noise-get-ex noise f noise-type)))

;;; noise-get-FBM =============================================================

(defcfun ("TCOD_noise_get_fbm" %noise-get-fbm) :float
  (noise noise) (f :pointer) (octaves :float))

(defun* (noise-get-fbm -> float) ((noise noise) (octaves float) &rest nums)
  "Returns the fractional Brownian motion function at the given coordinates."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
    (%noise-get-fbm noise f octaves)))

(defcfun ("TCOD_noise_get_fbm_ex" %noise-get-fbm-ex) :float
  (noise noise) (f :pointer) (octaves :float) (noise-type noise-type))

(defun* (noise-get-fbm-ex -> float) ((noise noise) (noise-type noise-type)
                                     (octaves float) &rest nums)
  "Returns the fractional Brownian motion function at the given coordinates,
using noise type NOISE-TYPE."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
    (%noise-get-fbm-ex noise f octaves noise-type)))


;;; noise-get-turbulence ======================================================


(defcfun ("TCOD_noise_get_turbulence" %noise-get-turbulence) :float
  (noise noise) (f :pointer) (octaves :float))

(defun* (noise-get-turbulence -> float) ((noise noise) (octaves float)
                                         &rest nums)
  "Returns the turbulence function at the given coordinates."
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
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
      (setf (mem-aref f :float i) (coerce (coerce (nth i nums) 'real)
                                          'single-float)))
    (%noise-get-turbulence-ex noise f octaves noise-type)))



;;;; <<Heightmap>> ============================================================



(define-c-function ("TCOD_heightmap_new" heightmap-new) heightmap-ptr
  ((width :int) (height :int))
  "Return a new heightmap with the given dimensions.")


(define-c-function ("TCOD_heightmap_get_value" heightmap-get-value) :float
  ;; 0 <= x < WIDTH
  ((heightmap heightmap-ptr) (x :int) (y :int))
  "Return the height at position =(X, Y)= in the heightmap.")


(define-c-function ("TCOD_heightmap_get_interpolated_value"
          heightmap-get-interpolated-value) :float
  ;; 0 <= x < WIDTH
  ((heightmap heightmap-ptr) (x :float) (y :float))
  "Calculate the height at position =(X, Y)= in the heightmap, where the
coordinates might not be integers.")


(define-c-function ("TCOD_heightmap_get_slope" heightmap-get-slope) :float
  ((heightmap heightmap-ptr) (x :int) (y :int))
  "Return the slope at position =(X, Y)= in the heightmap. The value returned
will be between 0 and pi/2.")


(define-c-function ("TCOD_heightmap_set_value" heightmap-set-value) :void
  ;; 0 <= x < WIDTH
  ((heightmap heightmap-ptr) (x :int) (y :int) (value :float))
  "Set the height at position =(X, Y)= in the heightmap to =VALUE=.")


(define-c-function ("TCOD_heightmap_add" heightmap-add) :void
  ((heightmap heightmap-ptr) (value :float))
  "Add =VALUE= to all heights in the heightmap.")


(define-c-function ("TCOD_heightmap_add_fbm" heightmap-add-fbm) :void
    ((heightmap heightmap-ptr) (noise noise) (mulx :float) (muly :float)
     (addx :float) (addy :float) (octaves :float) (delta :float) (scale :float))
  "Add values from the random noise object =NOISE= to all heights in
equivalent positions in =HEIGHTMAP=.")


(define-c-function ("TCOD_heightmap_scale" heightmap-scale) :void
  ((heightmap heightmap-ptr) (factor :float))
  "Multiply all the heights in the heightmap by =SCALE=.")


(define-c-function ("TCOD_heightmap_lerp_hm" heightmap-lerp-hm) :void
  ((hm1 heightmap-ptr) (hm2 heightmap-ptr) (result heightmap-ptr) (coef :float))
  "Fill the heightmap =RESULT= with the results of a lerp operation between
the two heightmaps =HM1= and =HM2=.")


(define-c-function ("TCOD_heightmap_add_hm" heightmap-add-hm) :void
  ((hm1 heightmap-ptr) (hm2 heightmap-ptr) (result heightmap-ptr))
  "Add the heights in =HM1= to heights in equivalent positions in
=HM2=, and store the results in the heightmap =RESULT=.")


(define-c-function ("TCOD_heightmap_multiply_hm" heightmap-multiply-hm) :void
  ((hm1 heightmap-ptr) (hm2 heightmap-ptr) (result heightmap-ptr))
  "Multiply the heights in =HM1= by the heights in equivalent positions in
=HM2=, and store the results in the heightmap =RESULT=.")


(define-c-function ("TCOD_heightmap_clear" heightmap-clear) :void
  ((heightmap heightmap-ptr))
  "Set all the heights in the heightmap to zero.")


(define-c-function ("TCOD_heightmap_delete" heightmap-delete) :void
  ((heightmap heightmap-ptr))
  "Destroy the heightmap object =HEIGHTMAP=.")


(define-c-function ("TCOD_heightmap_clamp" heightmap-clamp) :void
  ((heightmap heightmap-ptr) (min :float) (max :float))
  "If any height in =HEIGHTMAP= is below =MIN= or above =MAX=, set it
equal to =MIN= or =MAX= respectively.")


(define-c-function ("TCOD_heightmap_count_cells" heightmap-count-cells) :int
  ((heightmap heightmap-ptr) (min :float) (max :float))
  "Return the number of cells in =HEIGHTMAP= which contain heights between
=MIN= and =MAX=.")


(define-c-function ("TCOD_heightmap_has_land_on_border"
                    heightmap-has-land-on-border?) :boolean
  ((heightmap heightmap-ptr) (waterlevel :float))
  "Return true if any of the border cells of =HEIGHTMAP= have heights greater
than =WATERLEVEL=.")


(defcfun ("TCOD_heightmap_get_minmax" %heightmap-get-minmax) :void
  (heightmap heightmap-ptr) (minfloat :pointer) (maxfloat :pointer))


(defun* (heightmap-get-min -> float) ((heightmap heightmap-ptr))
  "Return the lowest height in =HEIGHTMAP=."
  (with-foreign-object (minf :float)
    (with-foreign-object (maxf :float)
      (%heightmap-get-minmax heightmap minf maxf)
      (mem-aref minf :float))))


(defun* (heightmap-get-max -> float) ((heightmap heightmap-ptr))
  "Return the highest height in =HEIGHTMAP=."
  (with-foreign-object (minf :float)
    (with-foreign-object (maxf :float)
      (%heightmap-get-minmax heightmap minf maxf)
      (mem-aref maxf :float))))



(define-c-function ("TCOD_heightmap_normalize" heightmap-normalize) :void
  ((heightmap heightmap-ptr) (min :float) (max :float))
  "Scale all the heights in =HEIGHTMAP= so that the lowest is equal to
=MIN= and the highest is equal to =MAX=.")


(defun heightmap-normalise (heightmap min max)
  (heightmap-normalize heightmap min max))


(define-c-function ("TCOD_heightmap_copy" heightmap-copy) :void
  ((source heightmap-ptr) (dest heightmap-ptr))
  "Copy the heightmap =SOURCE= into the heightmap object =DEST=.")


(defcfun ("TCOD_heightmap_rain_erosion" %heightmap-rain-erosion) :void
  (heightmap heightmap-ptr) (num-drops :int) (erosion-coef :float)
  (sediment-coef :float) (randomptr :pointer))


(defun* heightmap-rain-erosion ((heightmap heightmap-ptr) (num-drops fixnum)
                                (erosion-coef float)
                                (sedimentation-coef float)
                               &optional ((rng randomptr) +NULL+))
  "'Erode' the heightmap =HEIGHTMAP= by dropping =NUM-DROPS= 'raindrops' in
random locations."
  (%heightmap-rain-erosion heightmap num-drops erosion-coef
                           sedimentation-coef rng))


(defcfun ("TCOD_heightmap_dig_bezier" %heightmap-dig-bezier) :void
  (heightmap heightmap-ptr) (px :pointer) (py :pointer) (start-radius :float)
  (start-depth :float) (end-radius :float) (end-depth :float))


(defun* heightmap-dig-bezier ((heightmap heightmap-ptr) (coords list)
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


(defun* heightmap-dig-line ((heightmap heightmap-ptr)
                            (x1 fixnum) (y1 fixnum)
                            (x2 fixnum) (y2 fixnum) (radius float)
                            (depth float))
  (heightmap-dig-bezier heightmap `((,x1 . ,y1) (,x1 . ,y1) (,x2 . ,y2) (,x2 . ,y2))
                        radius depth radius depth))


(define-c-function ("TCOD_heightmap_add_hill" heightmap-add-hill) :void
  ((heightmap heightmap-ptr) (hx :float) (hy :float)
   (hradius :float) (hheight :float)))

(define-c-function ("TCOD_heightmap_dig_hill" heightmap-dig-hill) :void
  ((heightmap heightmap-ptr) (hx :float) (hy :float)
   (hradius :float) (hheight :float)))


(define-c-function ("TCOD_heightmap_kernel_transform"
                    %heightmap-kernel-transform) :void
  ((heightmap heightmap-ptr) (kernel-size :int)
   (dx-ptr :pointer) (dy-ptr :pointer)
   (weight-ptr :pointer) (min-level :float) (max-level :float)))


(defun* heightmap-kernel-transform ((heightmap heightmap-ptr)
                                    (coords list) (weights list)
                                    (min-level single-float)
                                    (max-level single-float))
  "* Arguments:
- HEIGHTMAP :: pointer to a heightmap object.
- COORDS :: a list of (X . Y) cons cells specifying coordinates relative to the
cell being processed. For example (-1 . 0) is the cell to the west, (0 . 1) is
the cell to the south, etc.
- WEIGHTS :: a list of factors by which to scale the values in processed cells.
The list must be the same length as COORDS.
- MIN-LEVEL, MAX-LEVEL :: Cells are only processed if their values lies within
these limits."
  (assert (= (length coords) (length weights)))
  (let ((ksize (length coords)))
    (with-foreign-objects ((dx-array :int ksize)
                           (dy-array :int ksize)
                           (weight-array :float ksize))
      (dotimes (i ksize)
        (setf (mem-aref dx-array :int i) (car (nth i coords))
              (mem-aref dx-array :int i) (cdr (nth i coords))
              (mem-aref weight-array :float i) (nth i weights)))
      (%heightmap-kernel-transform heightmap ksize dx-array dy-array
                                   weight-array min-level max-level))))



(define-c-function ("TCOD_heightmap_add_voronoi" %heightmap-add-voronoi) :void
    ((heightmap heightmap-ptr) (num-points :int) (num-coefs :int)
                               (coef-ptr :pointer)
     (rng randomptr)))

(defun* heightmap-add-voronoi ((heightmap heightmap-ptr) (num-points uint32)
                               (coefs list) (rng randomptr))
  "* Arguments:
- HEIGHTMAP :: pointer to a heightmap object.
- NUM-POINTS :: number of Voronoi sites to create.
- COEFS :: list of floats to use to scale the distance to each site.
- RNG :: pointer to a random number generator object."
  (with-foreign-object (coef-array :float (length coefs))
    (dotimes (i (length coefs))
      (setf (mem-aref coef-array :float i) (nth i coefs)))
    (%heightmap-add-voronoi heightmap num-points (length coefs)
                            coef-array rng)))


(define-c-function ("TCOD_heightmap_scale_fbm" heightmap-scale-fbm) :void
    ((heightmap heightmap-ptr) (noise noise) (mulx :float) (muly :float)
     (addx :float) (addy :float) (octaves :float) (delta :float)
                               (scale :float)))

(define-c-function ("TCOD_heightmap_get_normal" %heightmap-get-normal) :void
    ;; n is a pointer to an array of 3 floats.
  ((heightmap heightmap-ptr) (x :float) (y :float) (n :pointer)
   (water-level :float)))

(defun* (heightmap-get-normal -> list) ((heightmap heightmap-ptr)
                                        (x single-float) (y single-float)
                                        (water-level single-float))
  "* Returns: a list of 3 floats, representing the normalised normal
vector of the point at X, Y."
  (with-foreign-object (n-array :float 3)
    (%heightmap-get-normal heightmap x y n-array water-level)
    (loop for i from 0 below 3 collecting (mem-aref n-array :float i))))


(define-c-function ("TCOD_heightmap_islandify" heightmap-islandify) :void
    ((heightmap heightmap-ptr) (sea-level :float) (rng randomptr)))


;;;; <<Field of view>> =========================================================


;; Create a map
(define-c-function ("TCOD_map_new" map-new) mapptr
    ((width :int) (height :int))
  "Return a new map object of the given dimensions.")

(define-c-function ("TCOD_map_set_properties" map-set-properties) :void
    ((map mapptr) (x :int) (y :int) (transparent? :boolean)
                  (walkable? :boolean))
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

(define-c-function ("TCOD_map_set_in_fov" map-set-in-fov) :void
    ((map mapptr) (x :int) (y :int) (fov? :boolean))
  "Set whether the cell at X, Y in =MAP= is in field of view.")

(define-c-function ("TCOD_map_is_transparent" map-is-transparent?) :boolean
    ((map mapptr) (x :int) (y :int))
  "Return true if position =(X, Y)= on the map is set to be transparent.")

(define-c-function ("TCOD_map_is_walkable" map-is-walkable?) :boolean
    ((map mapptr) (x :int) (y :int))
  "Return true if position =(X, Y)= on the map is set to be walkable.")

(define-c-function ("TCOD_map_clear" map-clear) :void
    ((map mapptr) (transparent? :boolean) (walkable? :boolean))
  "Set all cells in =MAP= to be neither walkable nor transparent.")

(define-c-function ("TCOD_map_get_width" map-get-width) :int
    ((map mapptr))
  "Return the width of the map object =MAP=.")

(define-c-function ("TCOD_map_get_height" map-get-height) :int
    ((map mapptr))
  "Return the height of the map object =MAP=.")

(define-c-function ("TCOD_map_get_nb_cells" map-get-nb-cells) :int
    ((map mapptr))
  "Return the number of cells in the map object =MAP=.")

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
(define-c-function ("TCOD_path_new_using_function" path-new-using-function)
    a*-path
    ((xdim :int) (ydim :int) (callback :pointer) (user-data :pointer)
     (diagonal-cost :float))
  "Return a new A* path object, which will call the function =CALLBACK= to
calculate movement costs.")

(define-c-function ("TCOD_path_delete" path-delete) :void
    ((a*-path a*-path))
  "Delete an A* path object.")

(define-c-function ("TCOD_path_reverse" path-reverse) :void
    ((a*-path a*-path))
  "Swap origin and destination for an A* path object.")

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
  (a*-path a*-path) (xptr :pointer) (yptr :pointer)
  (recalc-when-needed? :boolean))

(defun* (path-walk -> (or null (cons fixnum fixnum))) ((a*-path a*-path)
                                                       (recalc-when-needed?
                                                        boolean))
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

(define-c-function ("TCOD_dijkstra_new_using_function"
                    dijkstra-new-using-function)
    dijkstra-path
    ((xdim :int) (ydim :int) (callback :pointer) (user-data :pointer)
     (diagonal-cost :float))
  "Return a new Dijkstra path object which calls the function =CALLBACK= to
calculate movement costs.")

(define-c-function ("TCOD_dijkstra_delete" dijkstra-delete) :void
    ((dijkstra-path dijkstra-path))
  "Delete a Dijkstra path object.")

(define-c-function ("TCOD_dijkstra_reverse" dijkstra-reverse) :void
    ((dijkstra-path dijkstra-path))
  "Swap origin and destination for a Dijkstra path object.")

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


;;;; <<Bresenham line drawing>> ===============================================


(define-c-function ("TCOD_line_init" line-init) :void
    ((xfrom :int) (yfrom :int) (xto :int) (yto :int)))

(define-c-function ("TCOD_line_step" line-step) :boolean
    ((xcur :pointer) (ycur :pointer)))

(define-c-function ("TCOD_line" line-line) :boolean
    ((xfrom :int) (yfrom :int) (xto :int) (yto :int) (callback :pointer)))

;; Example of how to define a lisp function which can be called from C
;; for use as the callback function for `line-line'.
(defcallback my-bresenham-callback :boolean
    ((x :int) (y :int))
  ;; In the function body: all args will have been converted to lisp.
  ;; The function's return value will be converted to C.
  (declare (ignore x y))
  )


;;;; <<BSP trees>> ============================================================


(define-c-function ("TCOD_bsp_new_with_size" bsp-new-with-size) bsp-ptr
    ((x :int) (y :int) (w :int) (h :int)))

(define-c-function ("TCOD_bsp_remove_sons" bsp-remove-sons) :void
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_delete" bsp-delete) :void
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_split_once" bsp-split-once) :void
    ((node bsp-ptr) (horizontal? :boolean) (pos :int)))

(define-c-function ("TCOD_bsp_split_recursive" bsp-split-recursive) :void
    ((node bsp-ptr) (randomiser randomptr) (nb :int) (min-h-size :int)
     (min-v-size :int) (max-h-ratio :float) (max-v-ratio :float)))

(define-c-function ("TCOD_bsp_resize" bsp-resize) :void
    ((node bsp-ptr) (x :int) (y :int) (w :int) (h :int)))

(define-c-function ("TCOD_bsp_left" bsp-left) bsp-ptr
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_right" bsp-right) bsp-ptr
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_father" bsp-father) bsp-ptr
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_is_leaf" bsp-is-leaf?) :boolean
    ((node bsp-ptr)))

(define-c-function ("TCOD_bsp_contains" bsp-contains?) :boolean
    ((node bsp-ptr) (cx :int) (cy :int)))

(define-c-function ("TCOD_bsp_find_node" bsp-find-node) bsp-ptr
    ((node bsp-ptr) (cx :int) (cy :int)))

(define-c-function ("TCOD_bsp_traverse_pre_order"
                    bsp-traverse-pre-order) :boolean
    ((node bsp-ptr) (callback :pointer) (userdata :pointer)))

(define-c-function ("TCOD_bsp_traverse_in_order"
                    bsp-traverse-in-order) :boolean
    ((node bsp-ptr) (callback :pointer) (userdata :pointer)))

(define-c-function ("TCOD_bsp_traverse_post_order"
                    bsp-traverse-post-order) :boolean
    ((node bsp-ptr) (callback :pointer) (userdata :pointer)))

(define-c-function ("TCOD_bsp_traverse_level_order"
                    bsp-traverse-level-order) :boolean
    ((node bsp-ptr) (callback :pointer) (userdata :pointer)))

(define-c-function ("TCOD_bsp_traverse_inverted_level_order"
                    bsp-traverse-inverted-level-order) :boolean
    ((node bsp-ptr) (callback :pointer) (userdata :pointer)))

;; Example of how to define a lisp function which can be called from C
;; for use as the callback function for `bsp-traverse-X-order'.
(defcallback my-bsp-callback :boolean
    ((node bsp-ptr) (userdata :pointer))
  ;; In the function body: all args will have been converted to lisp.
  ;; The function's return value will be converted to C.
  (declare (ignore node userdata))
  )


;;;; <<Testing>> ==============================================================


(defun hello-world ()
  (tcod:console-init-root 80 50 :title "Libtcod Hello World")
  (tcod:console-set-alignment *root* :center)
  (tcod:console-print tcod:*root* 40 25 "Hello World!")
  (tcod:console-flush)
  (tcod:console-wait-for-keypress t))

;;;; tcod.lisp ends here ======================================================
