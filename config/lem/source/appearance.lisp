;;;; appearance.lisp

(defpackage :config/appearance
  (:use :cl :lem :micros))

(in-package :config/appearance)

;;; SDL2 specific
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