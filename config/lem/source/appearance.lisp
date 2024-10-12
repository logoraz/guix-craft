;;;; appearance.lisp

(uiop:define-package :config/appearance
  (:use :cl :lem))
(in-package :config/appearance)

;; FIXME: Faling with the following error:
;; The value 0.8 is not of type SINGLE-FLOAT when binding SB-ALIEN::VALUE

;;; SDL2 specific
#+lem-sdl2
(progn
  (defvar *transparent* nil
    "Transparency toggler for SDL2 frontend.")
  (define-command toggle-transparency () ()
    (sdl2-ffi.functions:sdl-set-window-opacity
     (lem-sdl2/display::display-window lem-sdl2/display::*display*)
     (if *transparent*
         1.0
         0.8))
    (setf *transparent* (not *transparent*))))

;; https://github.com/lem-project/lem/discussions/748
(defun set-transparency (opacity)
  "Set SDL2 opacity"
  (sdl2-ffi.functions:sdl-set-window-opacity
   (lem-sdl2/display::display-window lem-sdl2/display::*display*) opacity))

;; (set-transparency 0.8)
  