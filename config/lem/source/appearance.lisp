;;;; appearance.lisp

(uiop:define-package :config/appearance
  (:use :cl :lem))
(in-package :config/appearance)

;; FIXME: Faling with the following error:
;; The value 0.8 is not of type SINGLE-FLOAT when binding SB-ALIEN::VALUE

;;; SDL2 specific
#+lem-sdl2
(progn
  (defvar *opaque* t
    "Transparency toggler for SDL2 frontend.")

  (defun set-opacity (opacity)
    "Set SDL2 opacity, aka transparency."
    (sdl2-ffi.functions:sdl-set-window-opacity
     (lem-sdl2/display::display-window lem-sdl2/display::*display*) opacity))

  (define-command toggle-opacity () ()
    (set-opacity (if *opaque* 0.8 1.0))
    (setf *opaque* (not *opaque*))))

;; https://github.com/lem-project/lem/discussions/748


;; (set-opacity 0.8)
  