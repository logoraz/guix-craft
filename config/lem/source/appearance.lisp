;;;; appearance.lisp

(uiop:define-package :config/appearance
  (:use :cl :lem))

(in-package :config/appearance)

;;; SDL2 specific
;; FIXME: Faling with the following error:
;; The value 0.0 is not of type SINGLE-FLOAT when binding SB-ALIEN::VALUE
;; Backtrace for: #<SB-THREAD:THREAD tid=14584 "editor" RUNNING {100CE50003}>
;; 0: (SDL2-FFI.FUNCTIONS:SDL-SET-WINDOW-OPACITY #<SDL2-FFI:SDL-WINDOW {#X11C3BB10}> 0.0d0)
;; 1: (CONFIG/APPEARANCE::TOGGLE-TRANSPARENCY)
;; 2: (LEM-CORE:CALL-COMMAND #<CONFIG/APPEARANCE::TOGGLE-TRANSPARENCY {100CE95233}> NIL)
;; 3: (LEM-CORE:CALL-COMMAND LEM-CORE/COMMANDS/OTHER:EXECUTE-COMMAND NIL)
;; 4: (LEM-CORE::COMMAND-LOOP-BODY)
;; 5: (LEM-CORE:COMMAND-LOOP)
;; 6: (LEM-CORE::TOPLEVEL-COMMAND-LOOP #<FUNCTION (LAMBDA NIL :IN LEM-CORE::RUN-EDITOR-THREAD) {1005F81B3B}>)
;; 7: ((LAMBDA NIL :IN LEM-CORE::RUN-EDITOR-THREAD))
;; 8: ((LAMBDA NIL :IN LEM-CORE::RUN-EDITOR-THREAD))
;; 9: ((FLET BORDEAUX-THREADS-2::RUN-FUNCTION :IN BORDEAUX-THREADS-2::ESTABLISH-DYNAMIC-ENV))
;; 10: ((LABELS BORDEAUX-THREADS-2::%ESTABLISH-DYNAMIC-ENV-WRAPPER :IN BORDEAUX-THREADS-2::ESTABLISH-DYNAMIC-ENV))
;; 11: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
;; 12: ((FLET "WITHOUT-INTERRUPTS-BODY-" :IN SB-THREAD::RUN))
;; 13: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
;; 14: ((FLET "WITHOUT-INTERRUPTS-BODY-" :IN SB-THREAD::RUN))
;; 15: (SB-THREAD::RUN)
;; 16: ("foreign function: call_into_lisp_")
;; 17: ("foreign function: funcall1")

#+lem-sdl2
(progn
  "Transparency toggler for SDL2 frontend."
  (defvar *transparent* nil)
  (define-command toggle-transparency () ()
    (sdl2-ffi.functions:sdl-set-window-opacity
     (lem-sdl2/display::display-window lem-sdl2/display::*display*)
     (if *transparent*
         1.0
         0.8))
    (setf *transparent* (not *transparent*))))

;;; When evaluated in-buffer, does not error on value not being of type single-float...
;; (sdl2-ffi.functions:sdl-set-window-opacity
;;  (lem-sdl2/display::display-window lem-sdl2/display::*display*) 0.8)