;;;; package.lisp --> screenshot

(uiop:define-package #:screenshot
  (:use #:cl :stumpwm)
  (:local-nicknames (:xl :xlib)
                    (:lt :local-time))
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
