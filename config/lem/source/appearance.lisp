;;;; appearance.lisp

(uiop:define-package :config/appearance
  (:use :cl :lem)
  (:export :set-opacity))
(in-package :config/appearance)

;; FIXME: Faling with the following error:
;; 
;;; SDL2 specific
#+lem-sdl2
(progn
  (defvar *opaque* nil
    "Transparency toggler for SDL2 frontend.")

  (defun set-opacity (opacity)
    "Set SDL2 opacity, aka transparency."
    (sdl2-ffi.functions:sdl-set-window-opacity
     (lem-sdl2/display:display-window (lem-sdl2/display:current-display)) (float opacity)))

  (define-command toggle-opacity () ()
    ;; The value 0.8 is not of type SINGLE-FLOAT when binding SB-ALIEN::VALUE
    (set-opacity (if *opaque* 1 4/5))
    (setf *opaque* (not *opaque*)))
  ;; Always start off as transparent
  (set-opacity 4/5))
