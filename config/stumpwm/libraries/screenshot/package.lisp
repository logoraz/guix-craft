;;;; package.lisp --> screenshot

(defpackage #:screenshot
  (:use #:cl :stumpwm :zpng :local-time)
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
