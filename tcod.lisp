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




(defpackage :tcod
  (:use :cl :cffi)
  (:export
   #:*root*
   #:+null+
   ;; == Colours ==
   #:start-colours
   #:start-colors
   #:colour
   #:compose-colour
   #:compose-color
   #:decompose-colour
   #:decompose-color
   #:invert-colour
   #:invert-color
   #:colour->grayscale
   #:color->grayscale
   ;; == Console ==
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
   #:console-get-fore
   #:console-get-back
   #:console-get-foreground-colour
   #:console-get-background-colour
   #:console-get-foreground-color
   #:console-get-background-color
   #:console-print-left
   #:console-print-right
   #:console-print-centre
   #:console-print-left-rect
   #:console-print-right-rect
   #:console-print-centre-rect
   #:console-print-frame
   #:console-print-double-frame
   #:legal-console-coordinates?
   #:console-put-char
   #:console-put-char-ex
   #:console-set-char
   #:console-set-fore
   #:console-set-back
   #:console-clear
   #:console-set-foreground-colour
   #:console-set-background-colour
   #:console-set-foreground-color
   #:console-set-background-color
   #:console-init-root
   #:console-is-fullscreen?
   #:console-set-fullscreen
   #:console-is-window-closed?
   #:console-credits
   #:console-credits-render
   #:console-set-custom-font
   #:console-set-window-title
   #:console-rect
   #:colour-set-hsv
   #:colour-get-hsv
   #:colour-equals?
   #:colour-add
   #:colour-multiply
   #:colour-multiply-scalar
   #:colour-lerp
   #:make-colour
   #:color-set-hsv
   #:color-get-hsv
   #:color-equals?
   #:color-add
   #:color-multiply
   #:color-multiply-scalar
   #:color-lerp
   #:make-color
   #:keycode
   #:drawing-character
   #:colctrl
   #:colctrl->char
   #:background-flag
   #:key
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
   #:console
   #:is-key-pressed?
   #:console-new
   #:console-delete
   #:console-get-width
   #:console-get-height
   #:console-blit
   ;; == Unicode ==
   ;; todo not yet implemented
   ;; == Mouse ==
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
   #:mouse-wheel-down
   #:mouse-wheel-up
   #:mouse-flags
   #:mouse-move
   #:mouse-get-status
   ;; == Image ==
   #:image-load
   #:image-save
   #:image-from-console
   #:image-clear
   #:image-put-pixel
   #:image-blit
   #:image-set-key-color
   #:image-set-key-colour
   ;; == Random ==
   #:random-new
   #:random-get-int
   #:random-get-float
   ;; == Noise ==
   #:noise-new
   #:noise-perlin
   #:noise-fbm-perlin
   #:noise-turbulence-perlin
   #:noise-delete
   ;; == System layer ==
   #:sys-save-screenshot
   #:sys-sleep-milli
   #:sys-set-fps
   #:sys-get-fps
   #:sys-get-current-resolution
   #:sys-flush
   ))

(in-package :tcod)

;;; CFFI 0.10.0 started using Babel to "encode" strings. This breaks extended
;;; ASCII characters when the default encoding scheme of :UTF-8 is used, ie C
;;; will receive different characters from those which are sent to it by the
;;; Lisp program. To actually pass the literal string to C, we need to change
;;; the encoding scheme to ISO-8859-1.
;;;

(setf cffi:*default-foreign-encoding* :iso-8859-1)

(define-foreign-library libtcod
	(:unix "libtcod.so")
	(:windows "libtcod-mingw.dll")
	;; (:macintosh "name-of-libtcod-file-in-macos")
	(t (:default "libtcod")))

(defvar *libtcod-loaded* nil)

(eval-when (:load-toplevel :execute)
	(unless *libtcod-loaded*
		(use-foreign-library libtcod)
		(setf *libtcod-loaded* t)))

(defvar *root* (null-pointer) "The root console.")
(defparameter +NULL+ (null-pointer))
(defconstant +NOISE-DEFAULT-HURST+ 0.5)
(defconstant +NOISE-DEFAULT-LACUNARITY+ 2.0)


;;; Foreign types.

(defctype colournum :unsigned-int)

(defcstruct colour  ; TCOD_color_t
	(r :uint8)
	(g :uint8)
	(b :uint8))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compose-colour (r g b)
    "Given three integer values R, G and B, representing the red, green and
blue components of a colour, return a 3 byte integer whose value is #xRRGGBB."
    (+ (ash r 16) (ash g 8) b))
  (defun compose-color (r g b) (compose-colour r g b)))


(defun decompose-colour (num)
  "Given a colournum #xRRGGBB, return R, G and B integer values
as 3 separate return values."
  (values
	 (ash (logand num #xff0000) -16)
	 (ash (logand num #x00ff00) -8)
	 (logand num #x0000ff)))
(defun decompose-color (num) (decompose-colour num))


(defun invert-colour (num)
  (multiple-value-bind (r g b) (decompose-colour num)
    (compose-colour (- 255 r) (- 255 g) (- 255 b))))
(defun invert-color (num) (invert-colour num))


;; (defvar *black* (compose-colour 0 0 0))
;; (defvar *dark-grey* (compose-colour 96 96 96))
;; (defvar *grey* (compose-colour 196 196 196))
;; (defvar *white* (compose-colour 255 255 255))
;; (defvar *dark-blue* (compose-colour 40 40 128))
;; (defvar *light-blue* (compose-colour 120 120 255))
;; (defvar *dark-red* (compose-colour 128 0 0))
;; (defvar *light-red* (compose-colour 255 100 50))
;; (defvar *dark-brown* (compose-colour 32 16 0))
;; (defvar *light-yellow* (compose-colour 255 255 150))
;; (defvar *yellow* (compose-colour 255 255 0))
;; (defvar *dark-yellow* (compose-colour 164 164 0))
;; (defvar *green* (compose-colour 0 220 0))
;; (defvar *orange* (compose-colour 255 150 0))
;; (defvar *red* (compose-colour 255 0 0))
;; (defvar *silver* (compose-colour 203 203 203))
;; (defvar *gold* (compose-colour 255 255 102))
;; (defvar *purple* (compose-colour 204 51 153))
;; (defvar *dark-purple* (compose-colour 51 0 51))
;; ;;; Colours not defined in TCOD.
;; (defvar *slate-grey* (compose-colour #x80 #x80 #x80))
;; (defvar *umber* (compose-colour #x80 #x40 0))
;; (defvar *pink* (compose-colour 255 0 255))
;; (defvar *chocolate* (compose-colour 210 105 30))
;; ;;; ...etc...


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
    ;; Colours not defined in TCOD.
    (:slate-grey 	#x80 #x80 #x80)
    (:umber 		#x80 #x40 0)
    (:pink 		#xFF #x00 #xFF)
    (:chocolate 	210 105 30)))

;; TCOD_keycode_t
(defcenum keycode
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
(defcstruct key-press
	(vk keycode)     ; character if vk == TCODK_CHAR else 0
	(c :unsigned-char)
	(flags :uint8))  ; does this correspond to a key press or key
					; release event ?

(defstruct key
  (vk :none :type keyword)
  (c #\null :type character)
  (pressed nil :type boolean)
  (lalt nil :type boolean)
  (lctrl nil :type boolean)
  (ralt nil :type boolean)
  (rctrl nil :type boolean)
  (shift nil :type boolean))


(defun make-simple-key (ch)
  (make-key :vk :char :c ch))


(defun same-keys? (key1 key2)
  (and (key-p key1) (key-p key2)
       (eql (key-vk key1) (key-vk key2))
       (eql (key-c key1) (key-c key2))
       (eql (key-shift key1) (key-shift key2))
       (eql (or (key-lalt key1) (key-ralt key1))
	    (or (key-lalt key2) (key-ralt key2)))
       (eql (or (key-lctrl key1) (key-rctrl key1))
	    (or (key-lctrl key2) (key-rctrl key2)))))
       


(defcenum drawing-character
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


;; TCOD_colctrl_t
(defcenum colctrl
	(:COLCTRL-1 1)
	:COLCTRL-2
	:COLCTRL-3
	:COLCTRL-4
	:COLCTRL-5
	(:COLCTRL-NUMBER 5)
	:COLCTRL-FORE-RGB
	:COLCTRL-BACK-RGB
	:COLCTRL-STOP )

;; TCOD_bkgnd_flag_t
(defcenum background-flag
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


(defcenum alignment
	:LEFT
	:CENTER
	:RIGHT)


(defcenum fov-algorithm
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
  :FOV-RESTRICTIVE
  :NB-FOV-ALGORITHMS)


(defbitfield key-state
	(:KEY-PRESSED 1)
	(:KEY-RELEASED 2))

(defbitfield custom-font-flags
	(:FONT-LAYOUT-ASCII-IN-COL 1)
	(:FONT-LAYOUT-ASCII-IN-ROW 2)
	(:FONT-TYPE-GREYSCALE 4)
	(:FONT-LAYOUT-TCOD 8))


;; TCOD_console_t
(defctype console :pointer)

;;#define TCOD_BKGND_ALPHA(alpha) 
;;((TCOD_bkgnd_flag_t)(TCOD_BKGND_ALPH|(((uint8)(alpha*255))<<8)))
(defun background-alpha (alpha)
  (foreign-enum-keyword 'background-flag
			(logior (foreign-enum-value 'background-flag :alph)
				(ash (mod (* alpha 255) 256) 8))))

;;
;;#define TCOD_BKGND_ADDALPHA(alpha) 
;;((TCOD_bkgnd_flag_t)(TCOD_BKGND_ADDA|(((uint8)(alpha*255))<<8)))
(defun background-add-alpha (alpha)
  (foreign-enum-keyword 'background-flag
			(logior (foreign-enum-value 'background-flag :adda)
				(ash (mod (* alpha 255) 256) 8))))


(defun start-colours ()
  (setf *colour-table* (make-hash-table :test #'eql))
  (dolist (term *initial-colours*)
    (destructuring-bind (name r g b) term
      (make-colour name r g b))))
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
  

(defun colour (keywd)
  "Given a colour keyword such as :GREY, return its corresponding RGB
value (#xRRGGBB)."
  (cond
    ((integerp keywd)
     keywd)
    (t
     (unless *colour-table*
       (start-colours))
     (gethash keywd *colour-table*))))
(defun color (keywd) (colour keywd))


(defun colctrl->char (ctrl)
  (code-char (foreign-enum-value 'colctrl ctrl)))

      
;; TCODLIB_API bool TCOD_color_equals (TCOD_color_t c1, TCOD_color_t c2); 
(defcfun ("TCOD_color_equals_wrapper" color-equals?) :boolean
	(c1 colournum) (c2 colournum))
(defun colour-equals? (c1 c2) (color-equals? c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_add (TCOD_color_t c1, TCOD_color_t c2);
(defcfun ("TCOD_color_add_wrapper" color-add) colournum
	(c1 colournum) (c2 colournum))
(defun colour-add (c1 c2) (color-add c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply (TCOD_color_t c1,
;; TCOD_color_t c2); 
(defcfun ("TCOD_color_multiply_wrapper" color-multiply) colournum
	(c1 colournum) (c2 colournum))
(defun colour-multiply (c1 c2) (color-multiply c1 c2))


;;TCODLIB_API TCOD_color_t TCOD_color_multiply_scalar (TCOD_color_t c1,
;; float value);
(defcfun ("TCOD_color_multiply_scalar_wrapper" color-multiply-scalar) colournum
	(c1 colournum) (value :float))
(defun colour-multiply-scalar (c1 value) (color-multiply-scalar c1 value))


;; TCODLIB_API TCOD_color_t TCOD_color_lerp(TCOD_color_t c1, TCOD_color_t c2,
;; float coef);
(defcfun ("TCOD_color_lerp_wrapper" color-lerp) colournum
	(c1 colournum) (c2 colournum) (coef :float))
(defun colour-lerp (c1 c2 coef) (color-lerp c1 c2 coef))


;; TCODLIB_API void TCOD_color_set_HSV(TCOD_color_t *c,float h, float s,
;; float v);
(defcfun ("TCOD_color_set_HSV" color-set-hsv) :void
	(c :pointer) (h :float) (s :float) (v :float))
(defun colour-set-hsv (c h s v) (color-set-hsv c h s v))


;; TCODLIB_API void TCOD_color_get_HSV(TCOD_color_t c,float * h, float * s,
;; float * v);

(defvar *internal-color-hue-ptr* (foreign-alloc :int))
(defvar *internal-color-saturation-ptr* (foreign-alloc :int))
(defvar *internal-color-value-ptr* (foreign-alloc :int))

(defcfun ("TCOD_color_get_HSV_wrapper" %color-get-hsv) :void
	(c colournum) (h :pointer) (s :pointer) (v :pointer))

(defun color-get-hsv (color)
    (%color-get-hsv color
		    *internal-color-hue-ptr*
		    *internal-color-saturation-ptr*
		    *internal-color-value-ptr*)
    (list (mem-ref *internal-color-hue-ptr* :int)
	  (mem-ref *internal-color-saturation-ptr* :int)
	  (mem-ref *internal-color-value-ptr* :int)))

(defun colour-get-hsv (colour) (color-get-hsv colour))

(defcfun ("TCOD_console_credits" console-credits) :void)

(defcfun ("TCOD_console_credits_render" console-credits-render) :boolean
  (x :int) (y :int) (alpha :boolean))


;;TCODLIB_API void TCOD_console_init_root(int w, int h, const char * title,
;;                                        bool fullscreen);
(defcfun ("TCOD_console_init_root" console-init-root) :void
	(w :int) (h :int) (title :string) (fullscreen :boolean))

;;TCODLIB_API void TCOD_console_set_custom_font(const char *fontFile,
;;                        int char_width, int char_height, int nb_char_horiz,
;;                        int nb_char_vertic, bool chars_by_row,
;;                        TCOD_color_t key_color);
(defcfun ("TCOD_console_set_custom_font" %console-set-custom-font) :void
	(fontfile :string) (flags custom-font-flags)
	(chars-horizontal :int) (chars-vertical :int)
	)
;; (nb-char-vertic :int)
;; 	(chars-by-row? :boolean) (key-colour colournum))


(defun console-set-custom-font (fontfile flags chars-horizontal chars-vertical)
  "FLAGS accepts a quoted list containing one or more of the symbols
:FONT-LAYOUT-ASCII-IN-ROW, :FONT-LAYOUT-ASCII-IN-COL, :FONT-TYPE-GREYSCALE,
or :FONT-LAYOUT-TCOD."
  (assert (probe-file fontfile))
  (%console-set-custom-font fontfile flags chars-horizontal chars-vertical))


;;TCODLIB_API void TCOD_console_set_window_title(const char *title);
(defcfun ("TCOD_console_set_window_title" console-set-window-title) :void
  (title :string))

;;TCODLIB_API void TCOD_console_set_fullscreen(bool fullscreen);
(defcfun ("TCOD_console_set_fullscreen" console-set-fullscreen) :void
  (full? :boolean))

;;TCODLIB_API bool TCOD_console_is_fullscreen();
(defcfun ("TCOD_console_is_fullscreen" console-is-fullscreen?) :boolean)

;;TCODLIB_API bool TCOD_console_is_window_closed();
(defcfun ("TCOD_console_is_window_closed" console-is-window-closed?) :boolean)


;;TCODLIB_API void TCOD_console_set_background_color(TCOD_console_t con,
;; TCOD_color_t col);
(defcfun ("TCOD_console_set_background_color_wrapper"
	  console-set-background-color) :void
	(con console) (col colournum))
(defun console-set-background-colour (con col)
  (console-set-background-color con col))


;;TCODLIB_API void TCOD_console_set_foreground_color(TCOD_console_t con,
;;                                                   TCOD_color_t col);
(defcfun ("TCOD_console_set_foreground_color_wrapper"
	  console-set-foreground-color) :void
	(con console) (col colournum))
(defun console-set-foreground-colour (con col)
  (console-set-foreground-color con col))


;;TCODLIB_API void TCOD_console_clear(TCOD_console_t con);
(defcfun ("TCOD_console_clear" console-clear) :void
	(con console))


;;TCODLIB_API void TCOD_console_set_back(TCOD_console_t con,int x, int y,
;;                                       TCOD_color_t col,
;;                                       TCOD_bkgnd_flag_t flag);
(defcfun ("TCOD_console_set_back_wrapper" %console-set-back) :void
  (con console) (x :int) (y :int) (col colournum) (flag background-flag))


(defun console-set-back (con x y col flag)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-set-back con x y col flag))


;;TCODLIB_API void TCOD_console_set_fore(TCOD_console_t con,int x, int y,
;;                                       TCOD_color_t col);
(defcfun ("TCOD_console_set_fore_wrapper" %console-set-fore) :void
  (con console) (x :int) (y :int) (col colournum))


(defun console-set-fore (con x y col)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-set-fore con x y col))



;;TCODLIB_API void TCOD_console_set_char(TCOD_console_t con,int x, int y,
;; int c);
(defcfun ("TCOD_console_set_char" %console-set-char) :void
  (con console) (x :int) (y :int) (c :unsigned-char))


(defun console-set-char (con x y ch)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-set-char con x y ch))


;;TCODLIB_API void TCOD_console_put_char(TCOD_console_t con,int x, int y,
;;                                       int c, TCOD_bkgnd_flag_t flag);
(defcfun ("TCOD_console_put_char" console-put-char) :void
	(con console) (x :int) (y :int) (c :unsigned-char)
	(flag background-flag))

;;TCODLIB_API void TCOD_console_put_char_ex(TCOD_console_t con,int x, int y,
;;                                          int c, TCOD_bkgnd_flag_t flag);
(defcfun ("TCOD_console_put_char_ex_wrapper" console-put-char-ex) :void
	(con console) (x :int) (y :int) (c :unsigned-char)
	(fg colournum) (bg colournum))

;;TCODLIB_API void TCOD_console_print_left(TCOD_console_t con,int x, int y,
;;                                         TCOD_bkgnd_flag_t flag,
;;                                         const char *fmt, ...); 
(defcfun ("TCOD_console_print_left" %console-print-left) :void
	(con console) (x :int) (y :int) (flag background-flag) (fmt :string)
	&rest)

(defun console-print-left (con x y flag fmt &rest args)
  (%console-print-left con x y flag (apply #'format nil fmt args)))

(defcfun ("TCOD_console_print_return_string"
	  %console-print-return-string) :string
	(con console) (x :int) (y :int) (rw :int) (rh :int)
	(flag background-flag) (align alignment) (msg :string)
	(can-split? :boolean) (count-only? :boolean))

(defun console-print-return-string (con x y rw rh flag align msg can-split?
				    count-only?)
  (%console-print-return-string con x y rw rh flag align msg
				can-split? count-only?))

;;TCODLIB_API void TCOD_console_print_right(TCOD_console_t con,int x, int y,
;; TCOD_bkgnd_flag_t flag, const char *fmt, ...); 
(defcfun ("TCOD_console_print_right" %console-print-right) :void
	(con console) (x :int) (y :int) (flag background-flag) (fmt :string)
	&rest)

(defun console-print-right (con x y flag fmt &rest args)
  (%console-print-right con x y flag (apply #'format nil fmt args)))

;;TCODLIB_API void TCOD_console_print_center(TCOD_console_t con,int x, int y,
;; TCOD_bkgnd_flag_t flag, const char *fmt, ...); 
(defcfun ("TCOD_console_print_center" console-print-centre) :void
	(con console) (x :int) (y :int) (flag background-flag) (fmt :string)
	&rest)

;;TCODLIB_API int TCOD_console_print_left_rect(TCOD_console_t con,int x, int y,
;; int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...); 
(defcfun ("TCOD_console_print_left_rect" console-print-left-rect) :int
	(con console) (x :int) (y :int) (w :int) (h :int)
	(flag background-flag) (fmt :string)
	&rest)

;;TCODLIB_API int TCOD_console_print_right_rect(TCOD_console_t con,int x,
;; int y, int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...); 
(defcfun ("TCOD_console_print_right_rect" console-print-right-rect) :int
	(con console) (x :int) (y :int) (w :int) (h :int)
	(flag background-flag) (fmt :string)
	&rest)


;;TCODLIB_API int TCOD_console_print_center_rect(TCOD_console_t con,int x,
;; int y, int w, int h, TCOD_bkgnd_flag_t flag, const char *fmt, ...); 
(defcfun ("TCOD_console_print_center_rect" console-print-centre-rect) :int
	(con console) (x :int) (y :int) (w :int) (h :int)
	(flag background-flag) (fmt :string)
	&rest)

;;TCODLIB_API void TCOD_console_rect(TCOD_console_t con,int x, int y, int w,
;; int h, bool clear, TCOD_bkgnd_flag_t flag);
(defcfun ("TCOD_console_rect" console-rect) :void
  (con console) (x :int) (y :int) (w :int) (h :int) (clear? :boolean)
  (flag background-flag))

;;TCODLIB_API void TCOD_console_hline(TCOD_console_t con,int x,int y, int l,
;; TCOD_bkgnd_flag_t flag);
;;TCODLIB_API void TCOD_console_vline(TCOD_console_t con,int x,int y, int l,
;; TCOD_bkgnd_flag_t flag);

;;TCODLIB_API void TCOD_console_print_frame(TCOD_console_t con,int x,int y,
;; int w,int h, bool empty, const char *fmt, ...);
(defcfun ("TCOD_console_print_frame" console-print-frame) :void
  (con console) (x :int) (y :int) (w :int) (h :int)
  (empty? :boolean) (flag background-flag) (fmt :string) &rest)

;; Added in wrappers.c
(defcfun ("TCOD_console_print_double_frame" console-print-double-frame) :void
  (con console) (x :int) (y :int) (w :int) (h :int)
  (empty? :boolean) (flag background-flag) (fmt :string) &rest)

;;TCODLIB_API TCOD_color_t TCOD_console_get_background_color(TCOD_console_t con);
(defcfun ("TCOD_console_get_background_color"
	  console-get-background-color) colournum
  (con console))
(defun console-get-background-colour (con)
  (console-get-background-color con))


;;TCODLIB_API TCOD_color_t TCOD_console_get_foreground_color(TCOD_console_t con);
(defcfun ("TCOD_console_get_foreground_color"
	  console-get-foreground-color) colournum
  (con console))
(defun console-get-foreground-colour (con)
  (console-get-foreground-color con))


;;TCODLIB_API TCOD_color_t TCOD_console_get_back(TCOD_console_t con,int x, int y)
(defcfun ("TCOD_console_get_back_wrapper" %console-get-back) colournum
  (con console) (x :int) (y :int))


(defun legal-console-coordinates? (con x y)
  (and (not (null-pointer-p con))
       (< x (console-get-width con))
       (< y (console-get-height con))))


(defun console-get-back (con x y)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-get-back con x y))


;;TCODLIB_API TCOD_color_t TCOD_console_get_fore(TCOD_console_t con,
;;                                               int x, int y);
(defcfun ("TCOD_console_get_fore_wrapper" %console-get-fore) colournum
  (con console) (x :int) (y :int))


(defun console-get-fore (con x y)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-get-fore con x y))

  
;;TCODLIB_API int TCOD_console_get_char(TCOD_console_t con,int x, int y);
(defcfun ("TCOD_console_get_char" %console-get-char) :unsigned-char
  (con console) (x :int) (y :int))


(defun console-get-char (con x y)
  ;; Assertion in libtcod
  (assert (legal-console-coordinates? con x y))
  (%console-get-char con x y))


;;TCODLIB_API void TCOD_console_set_fade(uint8 val, TCOD_color_t fade);
(defcfun ("TCOD_console_set_fade_wrapper" console-set-fade) :void
  (val :uint8) (fade colournum))

;;TCODLIB_API uint8 TCOD_console_get_fade();
(defcfun ("TCOD_console_get_fade" console-get-fade) :uint8)


;;TCODLIB_API TCOD_color_t TCOD_console_get_fading_color();
(defcfun ("TCOD_console_get_fading_color_wrapper"
	  console-get-fading-color) colournum)
(defun console-get-fading-colour ()
  (console-get-fading-color))


;;TCODLIB_API void TCOD_console_flush();
(defcfun ("TCOD_console_flush" console-flush) :void)

;; (sys-flush t) forces a redraw of the whole root console.
(defcfun ("TCOD_sys_flush" sys-flush) :void
	(flag :boolean))

;;TCODLIB_API void TCOD_console_set_color_control(TCOD_colctrl_t con,
;;     TCOD_color_t fore, TCOD_color_t back);
(defcfun ("TCOD_console_set_color_control_wrapper"
	  console-set-color-control) :void
  (con colctrl) (fore colournum) (back colournum))
(defun console-set-colour-control (con fore back)
  (console-set-color-control con fore back))


;;TCODLIB_API TCOD_key_t TCOD_console_check_for_keypress(int flags);
(defcfun ("TCOD_console_check_for_keypress_wrapper"
	  %console-check-for-keypress) :boolean
	(keyptr key-press) (flags key-state))


(defun get-bit (n pos)
  "POS = 1 refers to the 1's bit"
  (/= 0 (logand n (expt 2 (1- pos)))))


(defun key->keypress (keyptr)
  (let ((flags (foreign-slot-value keyptr 'key-press 'flags)))
    (make-key :vk (foreign-slot-value keyptr 'key-press 'vk)
	      :c (code-char (foreign-slot-value keyptr 'key-press 'c))
	      :pressed (get-bit flags 1)
	      :lalt (get-bit flags 2)
	      :lctrl (get-bit flags 3)
	      :ralt (get-bit flags 4)
	      :rctrl (get-bit flags 5)
	      :shift (get-bit flags 6))))


(defvar *key* nil)

(defun console-check-for-keypress (flags)
  (unless *key*
    (setf *key* (foreign-alloc 'key-press)))
  (%console-check-for-keypress *key* flags)
  (unless (eql :none (foreign-slot-value *key* 'key-press 'vk))
    (key->keypress *key*)))

(defun console-wait-for-keypress (flush)
  (unless *key*
    (setf *key* (foreign-alloc 'key-press)))
  (%console-wait-for-keypress *key* flush)
  (key->keypress *key*))


;;TCODLIB_API TCOD_key_t TCOD_console_wait_for_keypress(bool flush);
(defcfun ("TCOD_console_wait_for_keypress_wrapper"
	  %console-wait-for-keypress) :void
	(keyptr key-press) (flush :boolean))

;;TCODLIB_API void TCOD_console_set_keyboard_repeat(int initial_delay,
;; int interval);
;;TCODLIB_API void TCOD_console_disable_keyboard_repeat();
;;TCODLIB_API bool TCOD_console_is_key_pressed(TCOD_keycode_t key);
(defcfun ("TCOD_console_is_key_pressed" is-key-pressed?) :boolean
  (code keycode))

;;TCODLIB_API TCOD_console_t TCOD_console_new(int w, int h);
(defcfun ("TCOD_console_new" console-new) console
  (w :int) (h :int))

;;TCODLIB_API int TCOD_console_get_width(TCOD_console_t con);
(defcfun ("TCOD_console_get_width" console-get-width) :int
  (con console))

;;TCODLIB_API int TCOD_console_get_height(TCOD_console_t con);
(defcfun ("TCOD_console_get_height" console-get-height) :int
  (con console))

;;TCODLIB_API void TCOD_console_blit(TCOD_console_t src,int xSrc, int ySrc,
;; int wSrc, int hSrc, TCOD_console_t dst, int xDst, int yDst, int fade);
(defcfun ("TCOD_console_blit" %console-blit) :void
  (src console)
  (xsrc :int) (ysrc :int)
  (wsrc :int) (hsrc :int)
  (dest console)
  (xdest :int) (ydest :int)
  (foreground-alpha :float) (background-alpha :float))


(defun console-blit (src xsrc ysrc wsrc hsrc dest xdest ydest
                     foreground-alpha background-alpha)
  (check-type xsrc (integer 0))
  (check-type ysrc (integer 0))
  (check-type wsrc (integer 0))
  (check-type hsrc (integer 0))
  (check-type xdest (integer 0))
  (check-type ydest (integer 0))
  (check-type foreground-alpha (real 0 1.0))
  (check-type background-alpha (real 0 1.0))
  ;; Blitting a console to a position that lies completely outside the
  ;; destination console's bounds will do nothing, rather than causing
  ;; an error.
  (unless (or (>= xdest (console-get-width dest))
              (>= ydest (console-get-height dest)))
    ;; TCOD_console_blit unceremoniously crashes libtcod if this assertion
    ;; is not true when it is called. We therefore check the assertion here
    ;; first, so we have access to debugging facilities if the conditions
    ;; are not met.
    (assert (and (plusp wsrc) (plusp hsrc)
                 (>= (+ xdest wsrc) 0) (>= (+ ydest hsrc) 0)))
    (%console-blit src xsrc ysrc wsrc hsrc dest xdest ydest
                   foreground-alpha background-alpha)))


;;TCODLIB_API void TCOD_console_delete(TCOD_console_t console);
(defcfun ("TCOD_console_delete" console-delete) :void
  (con console))

;;; sys.h

;;TCODLIB_API uint32 TCOD_sys_elapsed_milli();
;;TCODLIB_API float TCOD_sys_elapsed_seconds();
;;TCODLIB_API void TCOD_sys_sleep_milli(uint32 val);
(defcfun ("TCOD_sys_sleep_milli" sys-sleep-milli) :void
  (val :unsigned-int))

;;TCODLIB_API void TCOD_sys_save_screenshot(const char *filename);
(defcfun ("TCOD_sys_save_screenshot" %sys-save-screenshot) :void
  (filename :string))

(defun sys-save-screenshot (&optional (filename (null-pointer)))
  (%sys-save-screenshot filename))

;;TCODLIB_API void TCOD_sys_force_fullscreen_resolution(int width, int height);
;;TCODLIB_API void TCOD_sys_set_fps(int val);
(defcfun ("TCOD_sys_set_fps" sys-set-fps) :void
  (val :int))

;;TCODLIB_API int TCOD_sys_get_fps();
(defcfun ("TCOD_sys_get_fps" sys-get-fps) :int)

;;TCODLIB_API float TCOD_sys_get_last_frame_length();
;;TCODLIB_API void TCOD_sys_get_current_resolution(int *w, int *h);
(defvar *internal-width-ptr* (foreign-alloc :int))
(defvar *internal-height-ptr* (foreign-alloc :int))

(defcfun ("TCOD_sys_get_current_resolution" %sys-get-current-resolution) :void
  (w-ptr :pointer) (h-ptr :pointer))

(defun sys-get-current-resolution ()
  (%sys-get-current-resolution *internal-width-ptr* *internal-height-ptr*)
  (values (mem-ref *internal-width-ptr* :int)
	  (mem-ref *internal-height-ptr* :int)))

;;; mersenne.h

;; TCOD_random_t
(defctype randomptr :pointer)

;;TCODLIB_API TCOD_random_t TCOD_random_get_instance();
;;TCODLIB_API TCOD_random_t TCOD_random_new();
(defcfun ("TCOD_random_new" random-new) randomptr)

;;TCODLIB_API TCOD_random_t TCOD_random_new_from_seed(uint32 seed);
;;TCODLIB_API int TCOD_random_get_int(TCOD_random_t mersenne, int min, int max);
(defcfun ("TCOD_random_get_int" random-get-int) :int
  (rng randomptr) (min :int) (max :int))

;;TCODLIB_API float TCOD_random_get_float(TCOD_random_t mersenne, float min,
;;   float max);
(defcfun ("TCOD_random_get_float" random-get-float) :float
  (rng randomptr) (min :float) (max :float))

;;TCODLIB_API int TCOD_random_get_int_from_byte_array(int min, int max,
;;   const char *data,int len);
;;TCODLIB_API void TCOD_random_delete(TCOD_random_t mersenne);

;;; mouse.h

(defcstruct mouse-state
	(x :int)	
	(y :int)	
	(dx :int)	
	(dy :int)
	(cx :int)	
	(cy :int)
	(dcx :int)	
	(dcy :int)
	(flags :uint8))

(defstruct mouse
	(x 0 :type integer)	;; absolute position
	(y 0 :type integer)	
	(dx 0 :type integer)	;; movement since last update in pixels
	(dy 0 :type integer)
	(cx 0 :type integer)	;; cell coordinates in the root console 
	(cy 0 :type integer)
	(dcx 0 :type integer)	;; movement since last update in console cells
	(dcy 0 :type integer)
	(lbutton nil :type boolean)	;; left button status
	(rbutton nil :type boolean)	;; right button status
	(mbutton nil :type boolean) ;; middle button status
	(lbutton-pressed nil :type boolean)	;; left button pressed event
	(rbutton-pressed nil :type boolean)	;; right button pressed event
	(mbutton-pressed nil :type boolean)	;; middle button pressed event
	(wheel-up nil :type boolean)		;; wheel up event
	(wheel-down nil :type boolean)   ;; wheel down event 
	(flags 0 :type integer))	;; copied from mouse-state


(defun mouse-state->mouse (ms)
  (let ((flags (foreign-slot-value ms 'mouse-state 'flags)))
    (make-mouse :x (foreign-slot-value ms 'mouse-state 'x)
		:y (foreign-slot-value ms 'mouse-state 'y)
		:dx (foreign-slot-value ms 'mouse-state 'dx)
		:dy (foreign-slot-value ms 'mouse-state 'dy)
		:cx (foreign-slot-value ms 'mouse-state 'cx)
		:cy (foreign-slot-value ms 'mouse-state 'cy)
		:dcx (foreign-slot-value ms 'mouse-state 'dcx)
		:dcy (foreign-slot-value ms 'mouse-state 'dcy)
		:lbutton (get-bit flags 1)
		:rbutton  (get-bit flags 2)
		:mbutton (get-bit flags 3)
		:lbutton-pressed (get-bit flags 4)
		:rbutton-pressed  (get-bit flags 5)
		:mbutton-pressed (get-bit flags 6)
		:wheel-up (get-bit flags 7)
		:wheel-down (get-bit flags 8)
		:flags flags)))

	      
;;TCODLIB_API TCOD_mouse_t TCOD_mouse_get_status();
(defcfun ("TCOD_mouse_get_status_wrapper" %mouse-get-status) :void
  (mouseptr mouse-state))

(let ((rodent nil))
  (defun mouse-get-status ()
    (unless rodent
      (setf rodent (foreign-alloc 'mouse-state)))
    (%mouse-get-status rodent)
    (mouse-state->mouse rodent)))


;;TCODLIB_API void TCOD_mouse_show_cursor(bool visible);
;;TCODLIB_API bool TCOD_mouse_is_cursor_visible();
;;TCODLIB_API void TCOD_mouse_move(int x, int y);
(defcfun ("TCOD_mouse_move" mouse-move) :void
  (x :int) (y :int))


;;; image.h

;; TCOD_image_t
(defctype image :pointer)

;;TCODLIB_API TCOD_image_t TCOD_image_new(int width, int height);
;;TCODLIB_API TCOD_image_t TCOD_image_from_console(TCOD_console_t console);
(defcfun ("TCOD_image_from_console" image-from-console) image
  (con console))

;;TCODLIB_API TCOD_image_t TCOD_image_load(const char *filename);
(defcfun ("TCOD_image_load" image-load) image
  (filename :string))


;;TCODLIB_API void TCOD_image_clear(TCOD_image_t image, TCOD_color_t color);
(defcfun ("TCOD_image_clear_wrapper" image-clear) :void
  (image image) (color colournum))

;;TCODLIB_API void TCOD_image_save(TCOD_image_t image, const char *filename);
(defcfun ("TCOD_image_save" image-save) :void
  (image image) (filename :string))


;;TCODLIB_API void TCOD_image_get_size(TCOD_image_t image, int *w,int *h);
;;TCODLIB_API TCOD_color_t TCOD_image_get_pixel(TCOD_image_t image,int x, int y);
(defcfun ("TCOD_image_get_pixel_wrapper" image-get-pixel) colournum
  (image image) (x :int) (y :int))

;;TCODLIB_API TCOD_color_t TCOD_image_get_mipmap_pixel(TCOD_image_t image,
;; float x0,float y0, float x1, float y1);
(defcfun ("TCOD_image_get_mipmap_pixel_wrapper"
	  image-get-mipmap-pixel) colournum
  (image image) (x0 :float) (y0 :float) (x1 :float) (y1 :float))

;;TCODLIB_API void TCOD_image_put_pixel(TCOD_image_t image,int x, int y,
;; TCOD_color_t col);
(defcfun ("TCOD_image_put_pixel_wrapper" image-put-pixel) :void
  (image image) (x :int) (y :int) (col colournum))

;;TCODLIB_API void TCOD_image_blit(TCOD_image_t image, TCOD_console_t console,
;; float x, float y, 
;;	TCOD_bkgnd_flag_t bkgnd_flag, float scalex, float scaley, float angle);
(defcfun ("TCOD_image_blit" image-blit) :void
  (image image) (con console) (x :float) (y :float) (flag background-flag)
  (scalex :float) (scaley :float) (angle :float))


;;TCODLIB_API void TCOD_image_blit_rect(TCOD_image_t image,
;; TCOD_console_t console, int x, int y, int w, int h, 
;;	TCOD_bkgnd_flag_t bkgnd_flag);
(defcfun ("TCOD_image_blit_rect" image-blit-rect) :void
  (image image) (con console) (x :int) (y :int) (w :int) (h :int)
  (flag background-flag))


;;TCODLIB_API void TCOD_image_delete(TCOD_image_t image);
;;TCODLIB_API void TCOD_image_set_key_color(TCOD_image_t image,
;; TCOD_color_t key_color);
(defcfun ("TCOD_image_set_key_color" image-set-key-color) :void
  (image image) (key-color colournum))
(defun image-set-key-colour (image key-colour)
  (image-set-key-color image key-colour))


;;TCODLIB_API bool TCOD_image_is_pixel_transparent(TCOD_image_t image, int x,
;;  int y);


;;; ========
;;;  Noise
;;; ========

(defctype noise :pointer)

;; TCODLIB_API TCOD_noise_t TCOD_noise_new(int dimensions, float hurst,
;; float lacunarity, TCOD_random_t random);
;; For randomptr, use +NULL+ to use the default RNG
(defcfun ("TCOD_noise_new" %noise-new) noise
  (dimensions :int) (hurst :float) (lacunarity :float) (randomptr :pointer))

(defun noise-new (dimensions &key (hurst +NOISE-DEFAULT-HURST+)
		  (lacunarity +NOISE-DEFAULT-LACUNARITY+)
		  (rng +NULL+))
  (%noise-new dimensions hurst lacunarity rng))


;; // basic perlin noise
;; TCODLIB_API float TCOD_noise_get( TCOD_noise_t noise, float *f );
(defcfun ("TCOD_noise_perlin" %noise-perlin) :float
  (noise noise) (f :pointer))

(defun noise-perlin (noise &rest nums)
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-perlin noise f)))


;; // fractional brownian motion
;; TCODLIB_API float TCOD_noise_fbm( TCOD_noise_t noise, float *f,
;; float octaves );
(defcfun ("TCOD_noise_fbm_perlin" %noise-fbm-perlin) :float
  (noise noise) (f :pointer) (octaves :float))

(defun noise-fbm-perlin (noise octaves &rest nums)
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-fbm-perlin noise f octaves)))

;; // turbulence
;; TCODLIB_API float TCOD_noise_turbulence( TCOD_noise_t noise,
;;   float *f, float octaves );
(defcfun ("TCOD_noise_turbulence_perlin" %noise-turbulence-perlin) :float
  (noise noise) (f :pointer) (octaves :float))

(defun noise-turbulence-perlin (noise octaves &rest nums)
  (with-foreign-object (f :float (length nums))
    (dotimes (i (length nums))
      (setf (mem-aref f :float i) (coerce (nth i nums) 'single-float)))
    (%noise-turbulence-perlin noise f octaves)))


;; TCODLIB_API void TCOD_noise_delete(TCOD_noise_t noise);
(defcfun ("TCOD_noise_delete" noise-delete) :void
  (noise noise))



